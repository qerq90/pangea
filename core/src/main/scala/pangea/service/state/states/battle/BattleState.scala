package pangea.service.state.states.battle

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.generator.loot.LootGenerator
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{BattleAlly, BattleEffects, Bleed, Buff, Burn, Element, GroupState, MonsterSlot, Poison, Regen, SoloPveBattle, SkillSlotState, TimedDefenceDebuff}
import pangea.model.squad.{AllyKind, AllySkill}
import pangea.model.hero.{Achievement, AzatState, CubeStatus, Hero, WeaponDust}
import pangea.model.item.QuestItemKind
import pangea.model.quest.NpcQuest
import pangea.model.item.{FlaskEffect, FlaskRates, ItemDetails, PassiveKind, PotionKind}
import pangea.model.monster.{MiniBoss, Race, Rarity}
import pangea.model.stats.FightStats
import pangea.model.trauma.TraumaRoll
import pangea.model.schedule.TaskKind
import pangea.model.skill.{MonsterEnergy, MonsterSkill, Skill}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.states.LootState
import pangea.service.state.MurlocQuest
import pangea.service.state.states.gustavo.GustavoState
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.schedule.Scheduler
import pangea.service.state.{AzatData, MarisaQuest, NpcQuestLog, State, UserAction}
import zio.{Random, Task, ZIO}
import java.util.concurrent.TimeUnit

/** Бой строится как «прочитать всё в начале хода → чисто посчитать ход →
  * записать всё один раз в конце». Боевые функции ([[attackTurn]]/[[skillTurn]]/
  * [[flaskTurn]]/[[beltTurn]]/[[fleeTurn]] и внутренние [[playerStrike]]/
  * [[monsterPhase]]/[[dealSkillDamage]]) НЕ трогают `heroDao`/`renderer` — они
  * только считают и накапливают лог сообщений ([[BattleState.TurnResult]]).
  * Единственная точка I/O — [[resolve]]/[[commit]]: она персистит итог хода
  * ровно один раз и показывает склеенный лог. Так исключён целый класс багов
  * «одна из веток забыла сохранить стат» (см. Кровавую жатву). */
case class BattleState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent,
  // Таймер раундов без героя (он обнулён, отряд дерётся дальше).
  scheduler:     Scheduler = Scheduler.none
) extends State {

  import BattleState.{AllyBlow, MobStrike, Outcome, TurnResult, VictoryOutcome}

  /** Чистое вычисление одного хода: снимок состояния → результат хода. */
  private type Turn = (Hero, SoloPveBattle, Long) => Task[TurnResult]

  private val branch = new Branch(
    routes = Map(
      "Attack"      -> Target.Run((u, ua, r) => attackRoute(u, BattleState.parseTarget(ua), r)),
      "Wait"        -> Target.Run((u, _, r) => resolve(u, r)(waitTurn)),
      "Move"        -> Target.Run((u, _, r) => moveRoute(u, r)),
      "MoveTo"      -> Target.Run((u, ua, r) => resolve(u, r)(moveTurn(BattleState.parseTarget(ua)))),
      "SquadTick"   -> Target.Run((u, _, r) => squadTick(u, r)),
      "UseFlask"    -> Target.Run((u, _, r) => resolve(u, r)(flaskTurn)),
      "UseBelt"     -> Target.Run((u, _, r) => resolve(u, r)(beltTurn)),
      "Flee"        -> Target.Run((u, _, r) => flee(u, r)),
      "ConfirmFlee" -> Target.Run((u, _, r) => resolve(u, r)(fleeTurn)),
      "CancelFlee"  -> Target.Run((u, _, r) => showScreen(u, r).as(StateType.Battle)),
      "CancelTarget" -> Target.Run((u, _, r) => showScreen(u, r).as(StateType.Battle))
    ),
    fallback = Target.Run { (user, ua, renderer) =>
      // Кнопки способностей именуются Skill_<itemId> и роутятся динамически: id
      // указывает на конкретный предмет, поэтому два предмета с «одним» Skill
      // имеют независимые cd/uses. В группе к умению может прилагаться цель —
      // номер моба в строю (см. skillRoute).
      BattleState.parseSkillAction(ua) match {
        case Some(itemId) => skillRoute(user, itemId, BattleState.parseTarget(ua), renderer)
        case None         => showScreen(user, renderer).as(StateType.Battle)
      }
    }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    getBattle(user).flatMap { battle =>
      if (battle.group.heroDown) showDown(user, battle, renderer).unit else showScreen(user, renderer)
    }

  override def action(
      user: User,
      ua: UserAction,
      renderer: Renderer
  ): Task[StateType] =
    heroDao.readActiveBattle(user.userId).flatMap {
      case None       => recover(user, renderer)
      case Some(json) =>
        ZIO.fromEither(json.as[SoloPveBattle]).flatMap { battle =>
          // Герой обнулён — ходит только таймер отряда; кнопки лишь показывают, как дела.
          if (battle.group.heroDown && !BattleState.parseAction(ua).contains("SquadTick")) showDown(user, battle, renderer)
          else branch.act(user, ua, renderer)
        }
    }

  /** Герой числится в бою, а боя нет. Так бывает, когда победа уже записана
    * (бой очищен, опыт начислен, добыча в scene_data), а переход к экрану
    * добычи не состоялся — оборвалась отправка сообщений, перезапуск между
    * записью и переходом, — либо бой сняли снаружи. Раньше это была ошибка
    * «пропишите /home» на каждое нажатие. Теперь: добыча ждёт — ведём к ней,
    * иначе — тихо в лабиринт. */
  private def recover(user: User, renderer: Renderer): Task[StateType] =
    heroDao.readSceneData(user.userId).flatMap { scene =>
      val won = scene.flatMap(_.as[LootState.LootData].toOption).exists(_.won)
      if (won) ZIO.succeed(StateType.Loot)
      else heroDao.writeSceneData(user.userId, Json.Null) *>
        renderer.show(user, Screen(content.text("battle.interrupted"), Nil)).as(StateType.Dungeon)
    }

  // ── Оболочка I/O: читаем всё → считаем ход → пишем всё ──────────────────────

  /** Прочитать снимок (герой + бой + время) один раз, посчитать ход чистой
    * функцией `turn`, затем один раз закоммитить результат. */
  private def resolve(user: User, renderer: Renderer)(turn: Turn): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      state  <- resolveLoaded(user, renderer, hero, battle, now)(turn)
    } yield state

  private def resolveLoaded(user: User, renderer: Renderer, hero: Hero, battle: SoloPveBattle, now: Long)(turn: Turn): Task[StateType] =
    for {
      result <- turn(hero, battle, now)
      // «Каменный страж» (порог 12) смотрит на ход целиком: важно не то, какой
      // именно источник добил героя до полоски, а что за этот ход он её перешёл.
      guarded = stoneGuardRescue(hero, result, now)
      // Гнилой Джо поднимается после обнуления HP — победу над ним ещё рано
      // засчитывать. Ловим здесь, где сходятся ВСЕ пути урона: удар, навык,
      // шипы, яд. Иначе каждый из них пришлось бы проверять отдельно.
      risen   = joeRises(guarded)
      // Группа: павший в паре уступает место следующему, мобы вне пары ходят
      // (сбоку бьёт только сосед), в конце раунда — подкрепление, перемешивание
      // каждый четвёртый раунд и отложенный Таран. Ход, который не был ходом
      // (навык не готов и т.п.), группу не двигает.
      promoted = promoteAfterKill(risen)
      // Союзники бьют после ответа активного моба; добили активного — снова
      // продвигаем следующего к герою.
      allied  <- allyPhase(promoted)
      promoted2 = promoteAfterKill(allied)
      sided   <- sideMobsPhase(promoted2, now)
      // Активный вне пары мог истечь ранами на своём ходу — тогда следующий.
      promoted3 = promoteAfterKill(sided)
      ended   <- endRound(battle, promoted3)
      // Напротив героя пусто, а свободный моб появился — он шагает к герою.
      pulled  = pullFreeStep(ended)
      // Подсказка про поглощённый удар идёт последней строкой раунда — уже после
      // всего, что в нём случилось.
      hinted  = plainSteelHint(hero, pulled)
      state  <- commit(user, hinted, now, renderer)
    } yield state

  /** Напоминание, что голое железо против этого врага почти бесполезно: без
    * такой строки игрок видит только маленькие числа и решает, что игра его
    * обманывает. Висит, пока бой идёт и оружие без стихий; на победе и смерти
    * не нужна. Сейчас единственный, кто так держит сталь, — каменный элементаль,
    * поэтому и текст про камень. */
  private def plainSteelHint(hero: Hero, res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Continue) res
    else if (!res.battle.boss.exists(_.plainDamageTakenMult < 1.0)) res
    else if (hero.gems.weaponElements.nonEmpty) res
    else res.copy(log = res.log :+ content.text("battle.elemental.stoneAbsorbs"))

  /** «Отказывается умирать»: пока у минибосса остались подъёмы, обнуление HP не
    * заканчивает бой — он встаёт с частью здоровья и слабеет в атаке. Пламя всё
    * меняет: начиная со второго падения горящий Джо упокаивается насовсем.
    *
    * Возвращает исход как есть, если это не он, подъёмы кончились или победы в
    * этом ходу вовсе не было. */
  private def joeRises(res: TurnResult): TurnResult = {
    val battle = res.battle
    val revives = battle.boss.map(_.revives).getOrElse(Nil)
    if (res.outcome != Outcome.Victory || revives.isEmpty) res
    else {
      val deathNo = battle.bossRevives + 1 // какое это по счёту падение
      val burning = battle.effects.monsterBurn.isDefined
      if (deathNo > revives.size) res // подъёмы кончились — смерть окончательная
      else if (burning && deathNo >= MiniBoss.RottenJoe.FireEndsFromDeath)
        res.copy(log = res.log :+ content.text("battle.joe.burnedOut"))
      else {
        val maxHp  = battle.monsterStats.hp
        val newHp  = (maxHp * revives(deathNo - 1) / 100L).max(1L)
        val risen  = battle.copy(
          monsterCurrentHp = newHp,
          bossRevives      = deathNo,
          effects = battle.effects.copy(
            monsterWeakenedTurns = MiniBoss.RottenJoe.ReviveAtkCutTurns,
            monsterWeakenedPct   = MiniBoss.RottenJoe.ReviveAtkCutPct))
        val key = if (deathNo == 1) "battle.joe.risesFirst" else "battle.joe.risesAgain"
        res.copy(
          battle  = risen,
          outcome = Outcome.Continue,
          log     = (res.log :+ content.text(key)) :+ content.format("battle.joe.riseWeakness",
                      "pct" -> MiniBoss.RottenJoe.ReviveAtkCutPct.toString,
                      "turns" -> MiniBoss.RottenJoe.ReviveAtkCutTurns.toString))
      }
    }
  }

  /** «Каменный страж» (порог 12): ход, за который HP героя провалилось ниже
    * порога, поднимает ему броню. Срабатывает именно НА ПЕРЕСЕЧЕНИИ — пока герой
    * сидит под порогом, каждый следующий удар брони уже не даёт. */
  private def stoneGuardRescue(before: Hero, res: TurnResult, nowMs: Long): TurnResult = {
    val hero = res.hero
    if (!hero.sets.rescuesOnLowHp || hero.fightStats.hp <= 0) res
    else {
      val limit = hero.effectiveMaxHp(nowMs) * hero.sets.lowHpThresholdPct / 100L
      if (before.fightStats.hp < limit || hero.fightStats.hp >= limit) res
      else {
        val maxArmor = hero.effectiveMaxArmor(nowMs)
        val restore  = (maxArmor * hero.sets.rescueArmorPct / 100L).max(1L)
        val newArmor = (hero.fightStats.armor + restore).min(maxArmor)
        val gained   = newArmor - hero.fightStats.armor
        if (gained <= 0L) res
        else res.copy(
          hero = hero.copy(fightStats = hero.fightStats.copy(armor = newArmor)),
          log  = res.log :+ content.format("battle.stoneGuardRescue", "armor" -> gained.toString)
        )
      }
    }
  }

  /** Единственная точка записи и показа за ход. Персист итоговых
    * снаряжения+статов героя выполняется ВСЕГДА и вне ветвления по исходу —
    * поэтому ни одна ветка не может «забыть» сохранить изменённый стат.
    *
    * ЖЁСТКИЙ ИНВАРИАНТ: сначала ВСЕ записи хода (неразрывным блоком), только
    * потом показы. Отправка сообщений — сетевой вызов к ВК: он может упасть,
    * а фибру обработки может прервать разрыв HTTP-соединения (ВК не дождался
    * ответа). Если бы запись шла после показа, ход «отображался, но не
    * случался»: игрок видел урон, а в БД оставалось прежнее состояние боя —
    * так терялось добивание навыком (моб оживал с прежним HP). */
  private def commit(
      user: User,
      raw: TurnResult,
      nowMs: Long,
      renderer: Renderer
  ): Task[StateType] = {
    // «Упырь» (порог 12): победа — это пир, герой сразу восстанавливает часть HP
    // и брони. Считаем ДО persistHero, чтобы восстановленное сохранилось вместе с
    // остальным исходом боя, а сообщение попало в тот же лог.
    val res     = ghoulFeast(raw, nowMs)
    val persistHero =
      heroDao.updateEquipmentAndFightStats(user.userId, res.hero.equipment, res.hero.fightStats)
    // Отряд: что с союзниками стало и кто ушёл по свитку — по концу боя.
    val persistSquad =
      ZIO.when(res.hero.squad.nonEmpty || res.battle.group.alliesGone.nonEmpty)(
        heroDao.updateSquad(user.userId, squadAfterBattle(res.hero, res.battle, nowMs)))
    // Пыль на оружии — покрытие на один бой: чем бы бой ни кончился, она осыпается.
    val clearDust =
      ZIO.when(!res.hero.weaponDust.isEmpty)(heroDao.updateWeaponDust(user.userId, WeaponDust.empty))
    val msg     = res.log.mkString("\n")
    val showLog = ZIO.when(msg.nonEmpty)(renderer.show(user, Screen(msg, Nil)))
    // Групповая сводка: что делали мобы вне пары и кто где стоит. Строй
    // показываем, пока бой идёт и в нём больше одного моба; удары сбоку — и
    // тогда, когда они добили героя, иначе смерть придёт без объяснения.
    val standing  = if (res.outcome == Outcome.Continue && res.battle.group.hasFormation) groupLines(res.battle) else Vector.empty
    val groupMsg  = (res.sideLog ++ standing).mkString("\n")
    val showGroup = ZIO.when(groupMsg.nonEmpty && (res.outcome == Outcome.Continue || res.outcome == Outcome.Death))(
      renderer.show(user, Screen(groupMsg, Nil)))
    res.outcome match {
      case Outcome.Continue =>
        (persistHero *> heroDao.writeActiveBattle(user.userId, res.battle.asJson)).uninterruptible *>
          showLog *> showGroup *>
          renderer
            .show(user, buildBattleScreen(res.hero, res.battle, res.hero.effectiveMaxHp(nowMs), nowMs))
            .as(StateType.Battle)
      case Outcome.Victory =>
        for {
          outcome <- (persistHero *> persistSquad *> clearDust *> applyVictory(user, res.hero, res.battle)).uninterruptible
          _       <- showLog
          _       <- showVictory(user, outcome, renderer)
        } yield StateType.Loot
      case Outcome.Death if res.battle.group.allies.exists(_.alive) && !res.battle.group.heroDown =>
        // Отряд ещё на ногах: герой падает, но не умирает — бой идёт без него,
        // раунд за раундом по таймеру, а смерть отложена до исхода.
        val down = res.battle.copy(group = res.battle.group.copy(heroDown = true))
        (persistHero *> heroDao.writeActiveBattle(user.userId, down.asJson)).uninterruptible *>
          showLog *> showGroup *>
          scheduler.schedule(user.userId, nowMs + BattleState.SquadTickMs, TaskKind.SquadFight, StateType.Battle, BattleState.SquadTickAction) *>
          renderer.show(user, Screen(content.text("battle.squad.heroDown"), Nil)) *>
          showDown(user, down, renderer)
      case Outcome.Death =>
        (persistHero *> persistSquad *> clearDust).uninterruptible *> showLog *> showGroup *>
          renderer.show(user, Screen(content.text("battle.death"), Nil)).as(StateType.Death)
      case Outcome.Fled =>
        // Сюжетный бой идёт не в лабиринте — бежать из него в город.
        val to = if (res.battle.story.isDefined) StateType.GlobalMap else StateType.Dungeon
        (persistHero *> persistSquad *> clearDust *> heroDao.clearActiveBattle(user.userId)).uninterruptible *>
          showLog *>
          renderer.show(user, Screen(content.text("battle.fled"), Nil)).as(to)
    }
  }

  /** Continue без изменения состояния — только сообщение (неготовый скилл, пустая
    * фляга и т.п.): ход не тратится, экран перерисовывается. */
  private def cont(hero: Hero, battle: SoloPveBattle, msg: String): Task[TurnResult] =
    ZIO.succeed(TurnResult(hero, battle, Vector(msg), Outcome.Continue, endsRound = false))

  // ── Ход игрока: базовая атака ───────────────────────────────────────────────

  /** Кнопка «Атака». Целей несколько (моб в паре и соседи) — сперва
    * спрашиваем, в кого бить, как у умений; одна — бьём сразу. */
  private def attackRoute(user: User, target: Option[Int], renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      state  <-
        if (target.isEmpty && battle.group.attackTargets.size > 1)
          renderer.show(user, attackTargetScreen(battle)).as(StateType.Battle)
        else resolveLoaded(user, renderer, hero, battle, now)(attackTurn(target))
    } yield state

  /** Базовая атака по месту `target` (без цели — по единственной досягаемой):
    * по мобу в паре — с его ответом; по соседу — через подмену в полях, после
    * чего отвечает моб в паре, а если пара пуста — ход тихо кончается и мобы
    * ходят сами. Бить некого — ход не тратится. */
  private def attackTurn(target: Option[Int])(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] = {
    val targets = battle.group.attackTargets
    target.orElse(targets.headOption) match {
      case None                                 => cont(hero, battle, content.text("battle.group.noTarget"))
      case Some(pos) if !targets.contains(pos)  => cont(hero, battle, content.text("battle.group.targetGone"))
      case Some(pos) if pos == battle.group.heroPos => playerStrike(hero, battle.rememberTarget(pos), nowMs, Vector.empty, Set.empty)
      case Some(pos) => strikeSide(hero, battle, pos, nowMs, Vector.empty, Set.empty).flatMap(closeTurn(_, nowMs, Set.empty))
    }
  }

  /** «Ждать»: бить некого — герой пропускает удар, раунд идёт своим чередом. */
  private def waitTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    closeTurn(TurnResult(hero, battle, Vector(content.text("battle.group.waited")), Outcome.Continue), nowMs, Set.empty)

  /** «Переместиться»: с кем из союзников поменяться местами. */
  private def moveRoute(user: User, renderer: Renderer): Task[StateType] =
    getBattle(user).flatMap { battle =>
      val choices = battle.group.allies.filter(_.alive).sortBy(_.position).zipWithIndex.map { case (a, i) =>
        pangea.engine.Choice("MoveTo", pangea.engine.Choice.fit(content.format("battle.group.moveTo", "n" -> a.position.toString, "name" -> a.name)),
          data = Map("target" -> a.position.toString), row = Some(i))
      }
      val cancel = pangea.engine.Choice("CancelTarget", content.text("battle.group.cancelTarget"),
        color = pangea.engine.ChoiceColor.Negative, row = Some(choices.size))
      renderer.show(user, Screen(content.text("battle.group.moveWhom"), choices :+ cancel)).as(StateType.Battle)
    }

  /** Герой меняется местами с союзником на позиции `target`: тот встаёт на
    * прежнее место героя. Стоял там моб — герой теперь с ним в паре, и тот
    * отвечает; ход на этом кончается. */
  private def moveTurn(target: Option[Int])(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    target.flatMap(p => battle.group.allyAt(p).filter(_.alive)) match {
      case None => cont(hero, battle, content.text("battle.group.targetGone"))
      case Some(ally) =>
        val from    = battle.group.heroPos
        val swapped = battle.copy(group = battle.group.updateAlly(ally.kind)(_.copy(position = from)))
        val moved   = if (swapped.group.hasMonster(ally.position)) swapped.moveHeroTo(ally.position)
                      else swapped.copy(group = swapped.group.copy(heroPos = ally.position))
        val line    = content.format("battle.group.moved", "name" -> ally.name, "n" -> ally.position.toString)
        closeTurn(TurnResult(hero, moved, Vector(line), Outcome.Continue), nowMs, Set.empty)
    }

  /** Раунд без героя (он обнулён): союзники бьют, мобы отвечают им, конец
    * раунда — как обычно, но героя никто не трогает. Мобов не осталось — герой
    * приходит в себя с [[BattleState.DownReviveHp]] HP и забирает победу;
    * не осталось союзников — смерть, отложенная до этого момента; иначе —
    * следующий раунд по таймеру. */
  private def squadTick(user: User, renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      state  <-
        if (!battle.group.heroDown) showScreen(user, renderer).as(StateType.Battle)
        else for {
          allied   <- allyPhase(TurnResult(hero, battle, Vector.empty, Outcome.Continue))
          promoted  = promoteAfterKill(allied)
          sided    <- sideMobsPhase(promoted, now)
          promoted2 = promoteAfterKill(sided)
          ended    <- endRound(battle, promoted2)
          res       = ended
          state    <- res.outcome match {
            case Outcome.Victory =>
              val revived = res.hero.copy(fightStats = res.hero.fightStats.copy(hp = BattleState.DownReviveHp))
              commit(user, res.copy(hero = revived, log = res.log :+ content.text("battle.squad.heroUp")), now, renderer)
            case _ if res.battle.group.allies.forall(!_.alive) =>
              commit(user, res.copy(outcome = Outcome.Death, log = res.log :+ content.text("battle.squad.allGone")), now, renderer)
            case _ =>
              val msg      = (res.log ++ res.sideLog).mkString("\n")
              val standing = groupLines(res.battle).mkString("\n")
              heroDao.writeActiveBattle(user.userId, res.battle.asJson) *>
                ZIO.when(msg.nonEmpty)(renderer.show(user, Screen(msg, Nil))) *>
                renderer.show(user, Screen(standing, Nil)) *>
                scheduler.schedule(user.userId, now + BattleState.SquadTickMs, TaskKind.SquadFight, StateType.Battle, BattleState.SquadTickAction) *>
                showDown(user, res.battle, renderer)
          }
        } yield state
    } yield state

  /** Экран героя, лежащего без сил: строй и кнопка «Как дела». */
  private def showDown(user: User, battle: SoloPveBattle, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(
      content.text("battle.squad.downScreen") + "\n\n" + groupLines(battle).mkString("\n"),
      List(pangea.engine.Choice("Look", content.text("battle.squad.lookLabel"), row = Some(0))))).as(StateType.Battle)

  /** Удар героя по мобу на месте `pos`, стоящему не напротив него: моб
    * разворачивается в поля, получает удар (без ответа) и сворачивается
    * обратно; добитый сосед — в павшие, добитый активный вне пары — Victory,
    * дальше промоут. Что было в полях, туда и возвращается. */
  private def strikeSide(
      hero: Hero, battle: SoloPveBattle, pos: Int, nowMs: Long, log: Vector[String], skip: Set[Long]
  ): Task[TurnResult] = {
    val home = battle.group.activePos
    playerStrike(hero, battle.engage(pos), nowMs, log, skip, retaliate = false)
      .map(res => settleAfterHit(res, pos, home, nowMs, skip))
  }

  /** После удара по месту `pos` (см. [[strikeSide]]): запомнить цель, вернуть
    * прежнего активного в поля, добитого — в павшие или в победу. */
  private def settleAfterHit(res: TurnResult, pos: Int, home: Int, nowMs: Long, skip: Set[Long]): TurnResult = {
    val struck = res.battle.rememberTarget(pos)
    val dead   = struck.monsterCurrentHp <= 0L
    if (pos == home) {
      if (dead) victoryByHero(res.hero, struck, res.log, nowMs, skip) else res.copy(battle = struck)
    } else {
      val back = struck.engage(home)
      if (dead)
        res.copy(battle = back.sideFallen(back.group.idxOf(pos)),
          log = res.log :+ content.format("battle.group.sideSlain", "monster" -> struck.monsterName))
      else res.copy(battle = back)
    }
  }

  /** Завершить ход героя: в паре отвечает моб, без пары ход кончается тихо.
    * Ход, который уже кончился (победа, смерть, «ничего не случилось»), не трогаем. */
  private def closeTurn(res: TurnResult, nowMs: Long, skip: Set[Long]): Task[TurnResult] =
    if (res.outcome != Outcome.Continue || !res.endsRound) ZIO.succeed(res)
    else if (res.hero.fightStats.hp <= 0) ZIO.succeed(res.copy(outcome = Outcome.Death))
    else respond(res.hero, res.battle, nowMs, res.log, skip)

  /** Ответ на ход героя: моб в паре бьёт и кастует ([[monsterPhase]]); пары
    * нет — только геройская сторона конца раунда ([[quietPhase]]). */
  private def respond(hero: Hero, battle: SoloPveBattle, nowMs: Long, log: Vector[String], skip: Set[Long]): Task[TurnResult] =
    if (battle.group.paired) monsterPhase(hero, battle, nowMs, log, skip)
    else quietPhase(hero, battle, nowMs, log, skip)

  /** Конец раунда без моба в паре: тик бафов и кулдаунов, раны и реген героя,
    * энергия. Мобы ходят потом сами — как мобы вне пары. */
  private def quietPhase(hero: Hero, battle: SoloPveBattle, nowMs: Long, log: Vector[String], skip: Set[Long]): Task[TurnResult] =
    ZIO.succeed {
      val ticked = battle.tickBuffs(skip)
      val (tickedHero, tickedBattle, heroEffectLine) = tickHeroEffects(hero, ticked, nowMs)
      val alive = tickedHero.fightStats.hp > 0
      val fed   = if (alive) regainEnergy(tickedHero, nowMs) else tickedHero
      val lines = if (heroEffectLine.isEmpty) log else log :+ heroEffectLine
      TurnResult(fed, tickedBattle, lines, if (alive) Outcome.Continue else Outcome.Death)
    }

  /** Напротив героя пусто, а свободный моб есть — шагает к герою (конец раунда). */
  private def pullFreeStep(res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Continue || !res.endsRound || res.battle.group.heroDown) res
    else res.battle.pullFree match {
      case None    => res
      case Some(b) => res.copy(battle = b, sideLog = res.sideLog :+ content.format("battle.group.steps", "monster" -> b.monsterName))
    }

  /** Базовая атака после умения, когда пары нет: по ближайшей досягаемой
    * цели, а если бить некого — ход кончается тихо. */
  private def followUp(hero: Hero, battle: SoloPveBattle, nowMs: Long, log: Vector[String], skip: Set[Long]): Task[TurnResult] =
    battle.group.attackTargets.headOption match {
      case None      => quietPhase(hero, battle, nowMs, log, skip)
      case Some(pos) => strikeSide(hero, battle, pos, nowMs, log, skip).flatMap(closeTurn(_, nowMs, skip))
    }

  /** Базовая атака игрока. `log` — уже накопленные строки хода (например, строка
    * применённого скилла, после которого идёт базовая атака). `skipSlots`
    * пробрасывается в `monsterPhase → tickBuffs`, чтобы только что использованный
    * слот не тикался в этом же ходу (см. SoloPveBattle.tickBuffs).
    */
  /** `isRepeat` — это переигранный промах от «Охотника» (порог 6). Такой заход не
    * повторяется ещё раз (не чаще раза за раунд) и НЕ прогоняет боевой реген
    * второй раз: раунд остался тем же, регенерировать герою заново нечего. */
  private def playerStrike(
      hero0: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      log: Vector[String],
      skip: Set[Long],
      isRepeat: Boolean = false,
      // false — только удар, без ответа моба: вызывающий сам решит, кто отвечает
      // (удар по соседу возвращает героя в пару, и отвечает моб оттуда).
      retaliate: Boolean = true
  ): Task[TurnResult] = {
    // Реген пассивок «Целебный»/«Самовосстанавливающийся» учитывается перед атакой.
    val hero = if (isRepeat) hero0 else hero0.withCombatRegen(nowMs)
    for {
      buffedEff <- ZIO.succeed(effWithAir(hero, battle, nowMs))
      hitRoll <- Random.nextIntBetween(1, 101)
      mobDodge   = mobDodgeChance(buffedEff.accuracy, battle)
      // Попадание считается как hitRoll(1..100) > dodge, т.е. фактический шанс =
      // 100 - floor(dodge). Округляем ТАК ЖЕ, как экран боя (см. buildBattleScreen),
      // иначе при дробном dodge сообщение расходится с экраном на 1.
      heroHitPct = 100 - mobDodge.toInt
      result <-
        if (hitRoll > mobDodge) {
          for {
            spread <- Random.nextLongBetween(80L, 121L)
            noWeapon =
              hero.equipment.weapon.itemType == pangea.model.item.ItemType.NoItem
            weaponMod: Double = if (noWeapon) 0.5 else 1.0
            // «Разбойник» (+5%) множит итоговый урон. Усиление стихии сюда НЕ
            // входит: оно сдвигает грани урона по броне/HP в splitElementalDamage.
            // Пыль, севшая неудачно (всполох магии или четвёртая горсть), режет
            // весь урон героя на четверть — ровно этот бой (см. WeaponDust).
            // «Мерзавец» бьёт на 5% сильнее — и получает на 5% больше (см. mobStrike).
            damage =
              (((hero.effectiveBaseStats(nowMs).str * 3L + buffedEff.atk) * spread / 100L) *
                weaponMod * hero.passives.finalDamageMult * hero.weaponDust.damageMult *
                Achievement.damagePct(hero) / 100.0).toLong
                .max(1L)
            // Стихии оружия модифицируют раздельно урон по броне и по HP
            // (см. splitElementalDamage). Без стихий поведение прежнее.
            // Сопротивление минибосса стихии оружия — до разбивки по броне/HP.
            resisted = (damage * bossResistance(hero, battle)).toLong.max(1L)
            // Защита моба режет удар ДО разбивки по броне/HP, иначе поехали бы
            // грани стихий (см. splitElementalDamage).
            guarded = (resisted * (1.0 - monsterDefenceCut(hero, battle, buffedEff, nowMs))).toLong.max(1L)
            (armorDmg, hpDmg) = splitElementalDamage(battle.monsterCurrentArmor, guarded, hero)
            // В лог идёт то, что РЕАЛЬНО ушло в цель: сырое число сбивало с толку
            // там, где сопротивление режет удар в разы (каменного элементаля голая
            // сталь берёт на 20%, и «357 урона» превращались в 71 снятой брони).
            attackLine = content.format(
              "battle.hit",
              "damage"  -> (armorDmg + hpDmg).toString,
              "monster" -> battle.monsterName
            )
            newArmor = battle.monsterCurrentArmor - armorDmg
            newHp    = (battle.monsterCurrentHp - hpDmg).max(0L)
            // Баф «ядовитые атаки»: на любом попадании снимается. Моб травится, только
            // если удар прошёл в HP (hpDmg > 0) и моб жив: повторное попадание стакает
            // текущий яд (+OnHit), иначе накладывает свежий.
            poisonsNow = battle.effects.heroPoisonousAttacks && hpDmg > 0 && newHp > 0
            effectsAfterPotion =
              if (!battle.effects.heroPoisonousAttacks) battle.effects
              else battle.effects.copy(
                heroPoisonousAttacks = false,
                monsterPoison =
                  if (poisonsNow) Some(battle.effects.monsterPoison.map(_.stacked).getOrElse(Poison.onHit))
                  else battle.effects.monsterPoison
              )
            // Изумруд в оружии: при уроне по HP пассивно накладывает/стакает яд.
            gemPoisonPct = hero.gems.weaponPoisonPct
            gemPoisons   = gemPoisonPct > 0 && hpDmg > 0 && newHp > 0
            effectsAfterGem =
              if (!gemPoisons) effectsAfterPotion
              else effectsAfterPotion.copy(
                monsterPoison = Some(effectsAfterPotion.monsterPoison
                  .map(p => Poison(p.pct + gemPoisonPct))
                  .getOrElse(Poison(gemPoisonPct))))
            // Фляги яда и крови (на раунды) и смазки из отваров (на весь бой): пока
            // оружие смазано, удар по HP травит (как отравленное оружие) либо пускает
            // кровь — стакается с тем, что висит.
            coatPoisons = (battle.effects.heroPoisonCoat || hero.weaponDust.poisonCoated) && hpDmg > 0 && newHp > 0
            coatBleeds  = (battle.effects.heroBleedCoat || hero.weaponDust.bleedCoated) && hpDmg > 0 && newHp > 0
            effectsCoated =
              if (!coatPoisons) effectsAfterGem
              else effectsAfterGem.copy(
                monsterPoison = Some(effectsAfterGem.monsterPoison.map(_.stacked).getOrElse(Poison.onHit)))
            effects =
              if (!coatBleeds) effectsCoated
              else effectsCoated.copy(
                monsterBleed = Some(effectsCoated.monsterBleed.map(_.stackedWith(FlaskRates.CoatBleedPct))
                                 .getOrElse(Bleed(FlaskRates.CoatBleedPct))))
            // «Упырь» (порог 6): шанс, что удар по HP пустит цели кровь. Бросок
            // не тратится, если набора нет, — детерминизм боевых тестов.
            setBleeds <- chanceRoll(
                           hero.sets.bleedOnHitChancePct > 0 && hpDmg > 0 && newHp > 0,
                           hero.sets.bleedOnHitChancePct)
            effectsBled =
              if (!setBleeds) effects
              else effects.copy(monsterBleed = Some(
                effects.monsterBleed.map(_.stackedWith(hero.sets.bleedPct)).getOrElse(Bleed(hero.sets.bleedPct))))
            // Череп в оружии и «Упырь» (порог 4): вампиризм с урона по HP. Кровь
            // пьётся только с раны — удар, целиком поглощённый бронёй, не лечит.
            maxHp      = hero.effectiveMaxHp(nowMs)
            vamp       = if (hero.gems.vampirismPct > 0 && hpDmg > 0) (hpDmg * hero.gems.vampirismPct / 100L).max(0L) else 0L
            setSteal   = if (hero.sets.lifestealPct > 0 && hpDmg > 0) (hpDmg * hero.sets.lifestealPct / 100L).max(0L) else 0L
            // Вампирская фляга: удар по HP лечит и тратит один из её ударов.
            flaskBite  = battle.effects.heroVampiricHits > 0 && hpDmg > 0
            flaskSteal = if (flaskBite) (hpDmg * FlaskRates.VampiricPct / 100L).max(0L) else 0L
            healedHero = if (vamp + setSteal + flaskSteal > 0) hero.copy(fightStats = hero.fightStats.copy(hp = (hero.fightStats.hp + vamp + setSteal + flaskSteal).min(maxHp))) else hero
            vampGained = healedHero.fightStats.hp - hero.fightStats.hp
            effectsFed = if (!flaskBite) effectsBled else effectsBled.copy(heroVampiricHits = effectsBled.heroVampiricHits - 1)
            hitBattle = battle
              .copy(monsterCurrentHp = newHp, monsterCurrentArmor = newArmor)
              .withEffects(effectsFed)
            // Проки стихий оружия (30% каждый) — только если моб жив после удара.
            elemResult <- if (newHp > 0) resolveElementProcs(hero, hitBattle)
                          else ZIO.succeed((hitBattle, Vector.empty[String]))
            (updated, elemLog) = elemResult
            // Шипы огненного элементаля: пока цела броня, он возвращает часть
            // урона и поджигает бьющего. Скованный холодом молчит.
            thornsResult = bossThorns(healedHero, updated, battle.monsterCurrentArmor, resisted)
            (thornedHero, thornedBattle, thornsLine) = thornsResult
            log1 = log :+ (attackLine + dotIndicators(thornedBattle))
            log2 =
              if (poisonsNow || gemPoisons || coatPoisons) log1 :+ content.format("battle.poisonApplied", "monster" -> battle.monsterName)
              else log1
            log3 =
              if (vampGained > 0) log2 :+ content.format("battle.vampirism", "healed" -> vampGained.toString)
              else log2
            log4 = (log3 ++ elemLog) ++ Vector(thornsLine).filter(_.nonEmpty)
            r <-
              if (!retaliate) ZIO.succeed(TurnResult(thornedHero, thornedBattle, log4, Outcome.Continue))
              else if (thornedBattle.monsterCurrentHp <= 0) ZIO.succeed(victoryByHero(thornedHero, thornedBattle, log4, nowMs, skip))
              else if (thornedHero.fightStats.hp <= 0) ZIO.succeed(TurnResult(thornedHero, thornedBattle, log4, Outcome.Death))
              else respond(thornedHero, thornedBattle, nowMs, log4, skip)
          } yield r
        } else {
          val attackLine = content.format("battle.miss", "chance" -> heroHitPct.toString)
          val missLog    = log :+ attackLine
          // «Охотник» (порог 6): промах можно переиграть — но не больше одного
          // повтора за раунд, поэтому повторный заход идёт с canRepeat = false.
          // Бросок не тратится без набора: детерминизм боевых тестов.
          chanceRoll(!isRepeat && hero.sets.repeatOnMissChancePct > 0, hero.sets.repeatOnMissChancePct)
            .flatMap { repeats =>
              if (repeats)
                playerStrike(hero, battle, nowMs, missLog :+ content.text("battle.hunterRepeat"), skip,
                  isRepeat = true, retaliate = retaliate)
              else if (!retaliate) ZIO.succeed(TurnResult(hero, battle, missLog, Outcome.Continue))
              else respond(hero, battle, nowMs, missLog, skip)
            }
        }
    } yield result
  }

  // ── Стихии оружия ───────────────────────────────────────────────────────────

  /** Боевые статы героя с учётом бафов боя, временного буста Воздуха (прок
    * Воздуха: +10% уклонения/точности на `airBoostTurns` ходов) и дебафов
    * каменного элементаля. Постоянный +5% от воздуха в оружии уже учтён в
    * `hero.effectiveFightStats`.
    *
    * Единая точка: и уклонение, и снижение урона, и шанс попасть по мобу считают
    * статы отсюда, поэтому дебаф достаточно применить здесь один раз. */
  private def effWithAir(hero: Hero, battle: SoloPveBattle, nowMs: Long): FightStats = {
    val fs0 = battle.heroBattleState.applyTo(hero.effectiveFightStats(nowMs))
    val fs =
      if (battle.effects.airBoostTurns <= 0) fs0
      else {
        val p = Element.Air.ProcBonusPct
        fs0.copy(
          evasion  = fs0.evasion + fs0.evasion * p / 100L,
          accuracy = fs0.accuracy + fs0.accuracy * p / 100L
        )
      }
    // Каменный элементаль: россыпь валунов сбивает защиту и уклонение, вязкая
    // земля — точность и уклонение. Уклонению может достаться от обоих сразу.
    val stunned  = if (battle.effects.heroStunned) MiniBoss.StoneElemental.BurstDebuffPct else 0L
    val grounded = if (battle.effects.heroGrounded) MiniBoss.StoneElemental.GroundCutPct else 0L
    if (stunned == 0L && grounded == 0L) fs
    else fs.copy(
      defence  = fs.defence * (100L - stunned) / 100L,
      evasion  = fs.evasion * (100L - stunned - grounded).max(0L) / 100L,
      accuracy = fs.accuracy * (100L - grounded) / 100L
    )
  }

  /** «Упырь» (порог 12): при убийстве врага герой мгновенно восстанавливает часть
    * HP и брони. Проценты считаются от эффективных потолков, восстановленное не
    * может опустить текущее значение (если оно почему-то выше потолка). Возвращает
    * исход как есть, если набор не собран, победы не случилось или лечить нечего. */
  private def ghoulFeast(res: TurnResult, nowMs: Long): TurnResult =
    if (res.outcome != Outcome.Victory || !res.hero.sets.feastsOnKill) res
    else {
      val hero     = res.hero
      val maxHp    = hero.effectiveMaxHp(nowMs)
      val maxArmor = hero.effectiveMaxArmor(nowMs)
      val newHp    = (hero.fightStats.hp + maxHp * hero.sets.feastHpPct / 100L).min(maxHp).max(hero.fightStats.hp)
      val newArmor = (hero.fightStats.armor + maxArmor * hero.sets.feastArmorPct / 100L).min(maxArmor).max(hero.fightStats.armor)
      val hpGained    = newHp - hero.fightStats.hp
      val armorGained = newArmor - hero.fightStats.armor
      if (hpGained <= 0 && armorGained <= 0) res
      else res.copy(
        hero = hero.copy(fightStats = hero.fightStats.copy(hp = newHp, armor = newArmor)),
        log  = res.log :+ content.format("battle.ghoulFeast",
                 "hp" -> hpGained.toString, "armor" -> armorGained.toString))
    }

  /** Насколько урон героя по элементалю ослаблен или усилен его стихией: своя
    * стихия почти не вредит (огонь по огненному — 20%), противоположная бьёт
    * сильнее (холод — 150%). Оружие сразу с двумя стихиями перемножает их
    * множители. Для обычных мобов всегда 1.0. */
  private def bossResistance(hero: Hero, battle: SoloPveBattle): Double =
    battle.boss.fold(1.0) { e =>
      // Оружие без единой стихии — «голая сталь»: каменного она почти не берёт.
      if (hero.gems.weaponElements.isEmpty) e.plainDamageTakenMult
      else hero.gems.weaponElements.foldLeft(1.0)((m, el) => m * e.damageTakenMult(el))
    }

  /** Как удар моба ложится на героя. Обычно — привычным порядком (сперва броня,
    * остаток в HP). У каменного элементаля грани РАЗДЕЛЬНЫ: пока броня цела, 90%
    * урона снимает её и одновременно 30% уходит в HP, так что броня от него не
    * спасает; чего броня не покрыла — уходит в HP, и у героя без брони удар
    * целиком приходится на здоровье.
    *
    * Моб, посыпавший оружие порошком, бьёт стихией: её грани двигают доли урона
    * по броне и HP так же, как стихии оружия двигают урон героя по мобу.
    * Возвращает новые hp и armor героя. */
  private def bossHit(battle: SoloPveBattle, hero: Hero, damage: Long): (Long, Long) =
    battle.boss.flatMap(_.heroHitSplit) match {
      // Минибосс с расколом: пара чисел — ДОЛИ удара, каменный бьёт и в броню,
      // и в HP мимо неё одновременно, это его особенность.
      case Some((armorPart, hpPart)) =>
        val curArmor = hero.fightStats.armor.max(0L)
        val absorbed = (damage * armorPart).toLong.min(curArmor)
        val toHp     = (damage - absorbed).max((damage * hpPart).toLong)
        ((hero.fightStats.hp - toHp).max(0L), curArmor - hero.sets.armorSpent(absorbed))
      case None =>
        powderSplit(battle) match {
          case None => MonsterSkill.applyPhysicalDamage(battle, hero, damage)
          // Стихия порошка: пара чисел — МНОЖИТЕЛИ, как у стихии в оружии героя.
          // Броня поглощает удар (со своим множителем), в HP идёт только то, что
          // за неё вылилось (со своим). Подставлять множители в раскол минибосса
          // нельзя: огонь с 1.1 по HP гнал 110% удара сквозь броню.
          case Some((armorMult, hpMult)) =>
            val curArmor  = hero.fightStats.armor.max(0L)
            val rawArmor  = math.min(curArmor, damage)
            val rawHp     = damage - rawArmor
            val armorDmg  = (rawArmor * armorMult).toLong.min(curArmor).max(0L)
            val hpDmg     = (rawHp * hpMult).toLong.max(0L)
            ((hero.fightStats.hp - hpDmg).max(0L), curArmor - hero.sets.armorSpent(armorDmg))
        }
    }

  /** Шипы огненного элементаля. Пока у него ЦЕЛА БРОНЯ (проверяется её запас до
    * удара), обычная атака героя возвращается частью урона и поджигает бьющего.
    * Скованный холодом элементаль шипами не отвечает.
    *
    * Возвращает героя после отдачи, бой с наложенным горением и строку лога
    * (пустую, если шипов не было). */
  private def bossThorns(
      hero: Hero,
      battle: SoloPveBattle,
      armorBeforeHit: Long,
      damageDealt: Long
  ): (Hero, SoloPveBattle, String) =
    battle.boss match {
      // Шипы — особенность огненного: о каменного руки не обжигают.
      case Some(MiniBoss.FireElemental) if armorBeforeHit > 0 && !battle.effects.chilled =>
        val thorns = bossDamageTaken(hero, battle, (damageDealt * MiniBoss.FireElemental.ThornsPct / 100L).max(1L))
        val (hurt, _) = hurtHero(hero, thorns)
        val burned = battle.copy(effects = battle.effects.copy(heroBurn = Some(
          battle.effects.heroBurn.map(_.reignited).getOrElse(Burn(MiniBoss.FireElemental.ThornsBurnPct)))))
        (hurt, burned, content.format("battle.elemental.thorns", "damage" -> thorns.toString))
      case _ => (hero, battle, "")
    }

  /** Ход элементаля-минибосса: он применяет способности строго ПО КРУГУ, а не
    * случайно, как рядовые мобы. Не хватило энергии или способность сейчас
    * бесполезна (щит на целом элементале) — раунд пропускается, но очередь всё
    * равно сдвигается: иначе он навсегда застрял бы на дорогом умении.
    *
    * Возвращает то же, что обычный каст: обновлённый бой, героя и строку лога. */
  private def bossTurnCast(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long
  ): Task[(SoloPveBattle, Hero, String)] =
    battle.boss match {
      case Some(MiniBoss.StoneElemental) => stoneTurnCast(hero, battle, nowMs)
      case Some(MiniBoss.RottenJoe)      => joeTurnCast(hero, battle, nowMs)
      case Some(MiniBoss.WhiteWolf)      => wolfTurnCast(hero, battle, nowMs)
      case _                             => fireTurnCast(hero, battle, nowMs)
    }

  /** Круг огненного: всплеск → сфера → щит → пропуск. */
  private def fireTurnCast(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long
  ): Task[(SoloPveBattle, Hero, String)] = {
    val lvl      = battle.monsterLvl
    val atk      = battle.monsterStats.atk
    val energy   = battle.monsterCurrentEnergy
    val next     = nextTurn(battle, MiniBoss.FireElemental)

    def spend(cost: Long, b: SoloPveBattle): SoloPveBattle = b.copy(monsterCurrentEnergy = energy - cost)

    battle.bossTurn match {
      // 1) Огненный всплеск: урон огнём + поджог героя на 1%.
      case 0 =>
        val cost = MiniBoss.FireElemental.SplashCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          val dmg     = bossDamageTaken(hero, battle, (atk * MiniBoss.FireElemental.SplashDamageFactor).toLong.max(1L))
          val (h, l)  = hurtHero(hero, dmg)
          val effects = next.effects.copy(heroBurn = Some(
            next.effects.heroBurn.map(_.reignited).getOrElse(Burn(MiniBoss.FireElemental.SplashBurnPct))))
          ZIO.succeed((spend(cost, next).copy(effects = effects), h,
            content.format("battle.elemental.fireSplash", "damage" -> l.toString)))
        }

      // 2) Сфера огня: копится до трёх, третья сразу срывается в смерч.
      case 1 =>
        val cost = MiniBoss.FireElemental.OrbCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          val orbs = battle.bossCharges + 1
          if (orbs < MiniBoss.FireElemental.OrbsToBurst)
            ZIO.succeed((spend(cost, next).copy(bossCharges = orbs), hero,
              content.text("battle.elemental.orbCreated")))
          else chargeBurst(hero, spend(cost, next).copy(bossCharges = 0), atk, nowMs, MiniBoss.FireElemental)
        }

      // 3) Огненный щит: чинит броню и HP, но только если есть что чинить.
      case 2 =>
        val cost     = MiniBoss.FireElemental.ShieldCostPerLvl * lvl
        val maxHp    = battle.monsterStats.hp
        val maxArmor = battle.monsterStats.armor
        val hurt     = battle.monsterCurrentHp < maxHp || battle.monsterCurrentArmor < maxArmor
        if (energy < cost || !hurt) ZIO.succeed((next, hero, ""))
        else {
          val newHp    = (battle.monsterCurrentHp + maxHp * MiniBoss.FireElemental.ShieldHpPct / 100L).min(maxHp)
          val newArmor = (battle.monsterCurrentArmor + maxArmor * MiniBoss.FireElemental.ShieldArmorPct / 100L).min(maxArmor)
          val healed   = newHp - battle.monsterCurrentHp
          val repaired = newArmor - battle.monsterCurrentArmor
          ZIO.succeed((
            spend(cost, next).copy(monsterCurrentHp = newHp, monsterCurrentArmor = newArmor),
            hero,
            content.format("battle.elemental.shield",
              "hp" -> healed.toString, "armor" -> repaired.toString)))
        }

      // 4) Пропуск хода.
      case _ => ZIO.succeed((next, hero, ""))
    }
  }

  /** Залп накопленных зарядов: ×2 атаки моба плюс 5% от максимумов HP и брони
    * героя. Если урон дошёл до HP, с шансом 20% герой получает травму — ту же,
    * что при смерти (см. [[pangea.model.trauma.TraumaRoll]]). Считается одинаково
    * у смерча огненного и россыпи каменного; расходятся они лишь текстом и
    * последействием — камни вдобавок сбивают героя с ног. */
  private def chargeBurst(
      hero: Hero,
      battle: SoloPveBattle,
      atk: Long,
      nowMs: Long,
      kind: MiniBoss
  ): Task[(SoloPveBattle, Hero, String)] = {
    val stone    = kind == MiniBoss.StoneElemental
    val statPct  = if (stone) MiniBoss.StoneElemental.BurstHeroStatPct else MiniBoss.FireElemental.BurstHeroStatPct
    val traumaAt = if (stone) MiniBoss.StoneElemental.BurstTraumaChancePct else MiniBoss.FireElemental.BurstTraumaChancePct
    val textKey  = if (stone) "battle.elemental.boulderBurst" else "battle.elemental.orbBurst"
    val maxHp    = hero.effectiveMaxHp(nowMs)
    val maxArmor = hero.effectiveMaxArmor(nowMs)
    val raw      = atk * 2L + (maxHp * statPct / 100L) + (maxArmor * statPct / 100L)
    val damage   = bossDamageTaken(hero, battle, raw)
    // Залп бьёт как обычная атака: сперва броня, остаток — в HP.
    val toArmor  = damage.min(hero.fightStats.armor)
    val toHp     = damage - toArmor
    val hit = hero.copy(fightStats = hero.fightStats.copy(
      armor = hero.fightStats.armor - hero.sets.armorSpent(toArmor),
      hp    = (hero.fightStats.hp - toHp).max(0L)))
    // Каменная россыпь сбивает с ног: защита и уклонение героя срезаны на 3 хода.
    val after =
      if (!stone) battle
      else battle.copy(effects = battle.effects.copy(heroStunnedTurns = MiniBoss.StoneElemental.BurstDebuffTurns))
    val burstLine = content.format(textKey, "damage" -> damage.toString)
    val line =
      if (!stone) burstLine
      else burstLine + "\n" + content.format("battle.elemental.boulderStun",
             "pct" -> MiniBoss.StoneElemental.BurstDebuffPct.toString,
             "turns" -> MiniBoss.StoneElemental.BurstDebuffTurns.toString)
    if (toHp <= 0 || hit.fightStats.hp <= 0) ZIO.succeed((after, hit, line))
    else
      for {
        roll   <- Random.nextIntBetween(1, 101)
        result <- if (roll > traumaAt) ZIO.succeed((after, hit, line))
                  else giveTrauma(hit, nowMs).map { case (h, traumaLine) => (after, h, line + "\n" + traumaLine) }
      } yield result
  }

  /** Сдвиг очереди способностей: круг у каждого вида своей длины. */
  private def nextTurn(battle: SoloPveBattle, kind: MiniBoss): SoloPveBattle =
    battle.copy(bossTurn = (battle.bossTurn + 1) % kind.abilities)

  /** Атака моба с учётом временного ослабления: подожжённый камень бьёт слабее,
    * поднявшийся из мёртвых Джо — тоже. Процент приходит вместе со сроком.
    * Волк, подстроившийся под добычу, наоборот, бьёт сильнее. */
  private def monsterAttack(battle: SoloPveBattle): Long = {
    val weakened =
      if (battle.effects.monsterWeakened)
        (battle.monsterStats.atk * (100L - battle.effects.monsterWeakenedPct).max(0L) / 100L).max(1L)
      else battle.monsterStats.atk
    if (battle.effects.mobInstinct) weakened * (100L + MiniBoss.WhiteWolf.InstinctBoostPct) / 100L
    else weakened
  }

  /** Потолок брони моба с учётом того, сколько его срезали поджоги. Текущая броня
    * может остаться ВЫШЕ потолка — её поджог не трогает, но чинить выше уже нельзя. */
  private def monsterArmorCap(battle: SoloPveBattle): Long =
    (battle.monsterStats.armor - battle.effects.monsterMaxArmorCut).max(0L)

  /** Круг Гнилого Джо: смрад → широкий удар → гнилое восстановление → пропуск. */
  private def joeTurnCast(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long
  ): Task[(SoloPveBattle, Hero, String)] = {
    val joe    = MiniBoss.RottenJoe
    val lvl    = battle.monsterLvl
    val energy = battle.monsterCurrentEnergy
    val next   = nextTurn(battle, joe)

    def spend(cost: Long, b: SoloPveBattle): SoloPveBattle = b.copy(monsterCurrentEnergy = energy - cost)

    battle.bossTurn match {
      // 1) Ядовитый смрад: травит героя. Повторный смрад стакает яд, как и у мобов.
      case 0 =>
        val cost = joe.StenchCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          val poison = next.effects.heroPoison
            .map(p => Poison(p.pct + joe.StenchPoisonPct))
            .getOrElse(Poison(joe.StenchPoisonPct))
          ZIO.succeed((spend(cost, next).copy(effects = next.effects.copy(heroPoison = Some(poison))),
            hero, content.text("battle.joe.stench")))
        }

      // 2) Широкий удар: три четверти атаки, и с шансом 5% — травма, если дошло до HP.
      case 1 =>
        val cost = joe.SweepCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          val dmg      = bossDamageTaken(hero, battle, (monsterAttack(battle) * joe.SweepDamageFactor).toLong.max(1L))
          val hpBefore = hero.fightStats.hp
          val (hurt, dealt) = hurtHero(hero, dmg)
          val line     = content.format("battle.joe.sweep", "damage" -> dealt.toString)
          val spent    = spend(cost, next)
          if (hurt.fightStats.hp >= hpBefore || hurt.fightStats.hp <= 0)
            ZIO.succeed((spent, hurt, line))
          else
            for {
              roll   <- Random.nextIntBetween(1, 101)
              result <- if (roll > joe.SweepTraumaChancePct) ZIO.succeed((spent, hurt, line))
                        else giveTrauma(hurt, nowMs).map { case (h, tl) => (spent, h, line + "\n" + tl) }
            } yield result
        }

      // 3) Гнилое восстановление: черви латают своё жилище.
      case 2 =>
        val cost  = joe.RegrowCostPerLvl * lvl
        val maxHp = battle.monsterStats.hp
        if (energy < cost || battle.monsterCurrentHp >= maxHp) ZIO.succeed((next, hero, ""))
        else {
          val newHp  = (battle.monsterCurrentHp + maxHp * joe.RegrowHpPct / 100L).min(maxHp)
          val healed = newHp - battle.monsterCurrentHp
          ZIO.succeed((spend(cost, next).copy(monsterCurrentHp = newHp), hero,
            content.format("battle.joe.regrow", "hp" -> healed.toString)))
        }

      // 4) Пропуск хода.
      case _ => ZIO.succeed((next, hero, ""))
    }
  }

  /** Круг Белого волка: пасть → когти → инстинкт → пропуск → смыкание пасти.
    * Первое бьющее умение боя наносит двойной урон, дальше двойной — с шансом
    * 5% (см. MiniBoss.WhiteWolf.CritChancePct). Бьёт он холодом, как и обычной
    * атакой: урон ложится через `bossHit` с гранями стихии. Пасть и когти
    * оставляют кровотечение, если удар дошёл до HP. Не хватило энергии —
    * умение пропускается, а круг сдвигается, как у остальных боссов. */
  private def wolfTurnCast(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long
  ): Task[(SoloPveBattle, Hero, String)] = {
    val wolf   = MiniBoss.WhiteWolf
    val lvl    = battle.monsterLvl
    val atk    = monsterAttack(battle)
    val energy = battle.monsterCurrentEnergy
    val next   = nextTurn(battle, wolf)

    def spend(cost: Long, b: SoloPveBattle): SoloPveBattle = b.copy(monsterCurrentEnergy = energy - cost)

    /** Множитель урона умения: первое бьющее — всегда вдвое (и запоминается),
      * дальше бросок на крит. Возвращает множитель, был ли крит и бой. */
    def critMult(b: SoloPveBattle): Task[(Long, Boolean, SoloPveBattle)] =
      if (!b.bossFirstSkillSpent) ZIO.succeed((wolf.CritMult, true, b.copy(bossFirstSkillSpent = true)))
      else Random.nextIntBetween(1, 101).map(roll =>
        if (roll <= wolf.CritChancePct) (wolf.CritMult, true, b) else (1L, false, b))

    /** Удар умением: крит, урон холодом по броне и HP, кровотечение при уроне по HP. */
    def bite(b: SoloPveBattle, raw: Long, bleedPct: Int, textKey: String): Task[(SoloPveBattle, Hero, String)] =
      critMult(b).map { case (mult, critted, b1) =>
        val damage            = bossDamageTaken(hero, battle, (raw * mult).max(1L))
        val (newHp, newArmor) = bossHit(battle, hero, damage)
        val hurt              = hero.copy(fightStats = hero.fightStats.copy(hp = newHp, armor = newArmor))
        val bleeds            = bleedPct > 0 && newHp < hero.fightStats.hp
        val b2 =
          if (!bleeds) b1
          else b1.copy(effects = b1.effects.copy(heroBleed = Some(
            b1.effects.heroBleed.map(_.stackedWith(bleedPct)).getOrElse(Bleed(bleedPct)))))
        val line = List(
          Some(content.format(textKey, "damage" -> damage.toString)),
          Option.when(critted)(content.text("battle.wolf.crit")),
          Option.when(bleeds)(content.format("battle.wolf.bleeds", "pct" -> bleedPct.toString))
        ).flatten.mkString("\n")
        (b2, hurt, line)
      }

    battle.bossTurn match {
      // 1) Яростная пасть: половина атаки, кровь при уроне по HP.
      case 0 =>
        val cost = wolf.FangsCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else bite(spend(cost, next), (atk * wolf.FangsDamageFactor).toLong.max(1L), wolf.FangsBleedPct, "battle.wolf.fangs")

      // 2) Яростные когти: то же, но слабее.
      case 1 =>
        val cost = wolf.ClawsCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else bite(spend(cost, next), (atk * wolf.ClawsDamageFactor).toLong.max(1L), wolf.ClawsBleedPct, "battle.wolf.claws")

      // 3) Животный инстинкт: атака и уклонение выше на 4 хода.
      case 2 =>
        val cost = wolf.InstinctCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else ZIO.succeed((
          spend(cost, next).copy(effects = next.effects.copy(mobInstinctTurns = wolf.InstinctTurns)),
          hero,
          content.text("battle.wolf.instinct")))

      // 4) Пропуск хода.
      case 3 => ZIO.succeed((next, hero, ""))

      // 5) Смыкание пасти: четверть недостающего герою HP плюс пятая часть атаки. Бесплатно.
      case _ =>
        val missing = (hero.effectiveMaxHp(nowMs) - hero.fightStats.hp).max(0L)
        val raw     = missing * wolf.JawsMissingHpPct / 100L + (atk * wolf.JawsDamageFactor).toLong
        bite(next, raw.max(1L), 0, "battle.wolf.jaws")
    }
  }

  /** Круг каменного: всплеск → валун → восстановление → вязкая земля → пропуск. */
  private def stoneTurnCast(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long
  ): Task[(SoloPveBattle, Hero, String)] = {
    val lvl    = battle.monsterLvl
    val atk    = monsterAttack(battle)
    val energy = battle.monsterCurrentEnergy
    val next   = nextTurn(battle, MiniBoss.StoneElemental)

    def spend(cost: Long, b: SoloPveBattle): SoloPveBattle = b.copy(monsterCurrentEnergy = energy - cost)

    battle.bossTurn match {
      // 1) Каменный всплеск: осколок в упор, половина атаки.
      case 0 =>
        val cost = MiniBoss.StoneElemental.SplashCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          val dmg    = bossDamageTaken(hero, battle, (atk * MiniBoss.StoneElemental.SplashDamageFactor).toLong.max(1L))
          val (h, l) = hurtHero(hero, dmg)
          ZIO.succeed((spend(cost, next), h,
            content.format("battle.elemental.stoneSplash", "damage" -> l.toString)))
        }

      // 2) Каменный валун: копится до трёх, третий сразу уходит в россыпь.
      case 1 =>
        val cost = MiniBoss.StoneElemental.BoulderCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          val boulders = battle.bossCharges + 1
          if (boulders < MiniBoss.StoneElemental.BouldersToBurst)
            ZIO.succeed((spend(cost, next).copy(bossCharges = boulders), hero,
              content.text("battle.elemental.boulderCreated")))
          else chargeBurst(hero, spend(cost, next).copy(bossCharges = 0), atk, nowMs, MiniBoss.StoneElemental)
        }

      // 3) Восстановление камня: чинит броню и HP, но только если есть что чинить.
      case 2 =>
        val cost     = MiniBoss.StoneElemental.RestoreCostPerLvl * lvl
        val maxHp    = battle.monsterStats.hp
        val cap      = monsterArmorCap(battle)
        val hurt     = battle.monsterCurrentHp < maxHp || battle.monsterCurrentArmor < cap
        if (energy < cost || !hurt) ZIO.succeed((next, hero, ""))
        else {
          val newHp    = (battle.monsterCurrentHp + maxHp * MiniBoss.StoneElemental.RestoreHpPct / 100L).min(maxHp)
          val newArmor = (battle.monsterCurrentArmor + battle.monsterStats.armor * MiniBoss.StoneElemental.RestoreArmorPct / 100L)
                           .min(cap.max(battle.monsterCurrentArmor))
          val healed   = newHp - battle.monsterCurrentHp
          val repaired = newArmor - battle.monsterCurrentArmor
          ZIO.succeed((
            spend(cost, next).copy(monsterCurrentHp = newHp, monsterCurrentArmor = newArmor),
            hero,
            content.format("battle.elemental.stoneRestore",
              "hp" -> healed.toString, "armor" -> repaired.toString)))
        }

      // 4) Вязкая земля: режет герою точность и уклонение.
      case 3 =>
        val cost = MiniBoss.StoneElemental.GroundCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else
          ZIO.succeed((
            spend(cost, next).copy(effects = next.effects.copy(heroGroundedTurns = MiniBoss.StoneElemental.GroundTurns)),
            hero,
            content.format("battle.elemental.stickyGround",
              "pct" -> MiniBoss.StoneElemental.GroundCutPct.toString,
              "turns" -> MiniBoss.StoneElemental.GroundTurns.toString)))

      // 5) Пропуск хода.
      case _ => ZIO.succeed((next, hero, ""))
    }
  }

  /** Травма от смерча — та же механика, что и при смерти: прогресс по тирам
    * (лёгкие → средние → тяжёлые), уже полученные исключаются. Пишется сразу,
    * потому что `commit` сохраняет только экипировку и боевые статы. */
  private def giveTrauma(hero: Hero, nowMs: Long, key: String = "battle.elemental.orbTrauma"): Task[(Hero, String)] = {
    val existing = if (hero.traumaActive(nowMs)) hero.traumaNames else Nil
    val pool     = TraumaRoll.pool(existing)
    val until    = nowMs + TraumaRoll.DurationMs
    if (pool.isEmpty) ZIO.succeed((hero, content.text("battle.elemental.orbTraumaMax")))
    else
      for {
        idx   <- Random.nextIntBetween(0, pool.length)
        trauma = pool(idx)
        names  = existing :+ trauma.name
        _     <- heroDao.updateTrauma(hero.userId, Some(until), names)
      } yield (
        hero.copy(traumaUntil = Some(until), traumaNames = names),
        content.format(key, "traumaName" -> trauma.name)
      )
  }

  /** Удар моба по HP может оставить травму. Мелкие удары (до 5% потолка HP) не в
    * счёт; ощутимый — 1%; сокрушительный (больше половины потолка) — 50%.
    * Считается по каждому удару и приёму отдельно; яд, кровь и огонь — не удары.
    * Мёртвому травму не добавляем (её даст смерть), и не дублируем ту, что приём
    * выдал сам (смерч, широкий удар Джо). Бросок не тратится, если удар был
    * мелким или ушёл в броню. */
  private def hitTrauma(before: Hero, after: Hero, nowMs: Long): Task[(Hero, Option[String])] = {
    val lost = before.fightStats.hp - after.fightStats.hp
    // Потолок — эффективный максимум, но не ниже того, что было: HP выше потолка
    // (сошёл баф, сменилась вещь) не должно делать каждый удар сокрушительным.
    val ceiling = after.effectiveMaxHp(nowMs).max(before.fightStats.hp)
    if (lost * 100L <= ceiling * BattleState.HitTraumaMinPct || after.fightStats.hp <= 0L ||
        after.traumaNames != before.traumaNames) ZIO.succeed((after, None))
    else {
      val crushing = lost * 100L > ceiling * BattleState.HitTraumaCrushPct
      val chance   = if (crushing) BattleState.HitTraumaCrushChancePct else BattleState.HitTraumaChancePct
      val key      = if (crushing) "battle.hitTraumaCrush" else "battle.hitTrauma"
      chanceRoll(active = true, chance).flatMap { hit =>
        if (hit) giveTrauma(after, nowMs, key).map { case (h, l) => (h, Some(l)) }
        else ZIO.succeed((after, None))
      }
    }
  }

  /** ЕДИНАЯ точка активного исцеления героя — всё, чем он лечит себя сам:
    * любая фляга, зелье из пояса, активное умение. Механика висит на самом
    * действии, а не на предмете, поэтому новая фляга или новое умение попадают
    * под неё сами, без правок здесь.
    *
    * Пока герой горит, исцеление слабее на 50% + текущий процент горения: при
    * 50% горения от него не остаётся ничего. Само горение при этом НЕ гаснет —
    * герой продолжает гореть на свои проценты (в отличие от мобов, которых
    * лечение из горения выводит). Тикающая регенерация сюда не входит: она не
    * действие игрока, а эффект, который идёт своим чередом.
    *
    * Возвращает новое HP, сколько реально вылечено и строку-пояснение, если
    * пламя часть лечения съело (без неё слабое зелье читалось бы как поломка). */
  private def activeHeal(
      hero: Hero,
      battle0: SoloPveBattle,
      raw: Long,
      nowMs: Long
  ): (Long, Long, SoloPveBattle, Option[String]) = {
    // Кровотечение лечение снимает целиком — то же правило, что и у моба
    // (см. Bleed): рана затянута.
    val battle = battle0.copy(effects = battle0.effects.copy(heroBleed = None))
    battle.effects.heroBurn match {
      case None =>
        val newHp = (hero.fightStats.hp + raw).min(hero.effectiveMaxHp(nowMs))
        (newHp, newHp - hero.fightStats.hp, battle, None)

      case Some(burn) =>
        // Ослабление = 50% базовых + текущий процент горения. Пламя при этом
        // ТРАТИТСЯ: базовые 50 п.п. оно гасит собой, поэтому горение до 50%
        // сгорает целиком, а сверх того остаётся гореть излишек (51% → 1%).
        val cut     = burn.healWeakenPct.min(100)
        val amount  = (raw * (100 - cut) / 100).max(0L)
        val newHp   = (hero.fightStats.hp + amount).min(hero.effectiveMaxHp(nowMs))
        val left    = Burn.of(burn.pct - Burn.HealWeakenBase)
        val updated = battle.copy(effects = battle.effects.copy(heroBurn = left))
        val line = left match {
          case None    => Some(content.format("battle.burnEatsHealOut", "pct" -> cut.toString))
          case Some(b) => Some(content.format("battle.burnEatsHeal",
                                 "pct" -> cut.toString, "left" -> b.pct.toString))
        }
        (newHp, newHp - hero.fightStats.hp, updated, line)
    }
  }

  /** Итоговое снижение урона у героя: его защита против атаки моба, плюс бонус
    * «Заслона», минус то, что обгрызли морозные удары моба (Холод режет снижение
    * в п.п. и не затухает — так же, как прок Холода у героя режет снижение моба). */
  private def heroDamageReduction(
      hero: Hero,
      battle: SoloPveBattle,
      buffed: FightStats,
      monsterAtk: Long,
      nowMs: Long
  ): Double = {
    val base = BattleState.damageReduction(
      protection  = buffed.defence,
      defenderInt = hero.effectiveBaseStats(nowMs).int,
      // «Интеллект» моба в атаке = его атака (у мобов нет отдельного стата интеллекта).
      attackerInt = monsterAtk,
      bonusPct    = battle.heroBattleState.reductionBonusPct
    )
    (base - battle.effects.heroColdDefenceCut / 100.0).max(0.0)
  }

  /** Насколько защита моба срежет удар героя — одинаково для базовой атаки и для
    * умений, которые режутся защитой. Порядок: пробитие героя сначала съедает саму
    * защиту (уже с учётом %-дебафа комбо), остаток идёт в общую формулу против
    * Мощи героя, и только потом Холод и «Дикое пламя» срезают итоговые проценты. */
  private def monsterDefenceCut(
      hero: Hero,
      battle: SoloPveBattle,
      buffed: FightStats,
      nowMs: Long
  ): Double = {
    val base   = hero.effectiveBaseStats(nowMs)
    val power  = BattleState.power(base.str, buffed.atk)
    val pierce = BattleState.pierce(base.int, base.agi, power, buffed.pierce)
    val red0   = BattleState.defenceReduction(effectiveMonsterDefence(battle), pierce, power)
    // «Дикое пламя» (порог 12) добавляет к срезу столько п.п., на сколько цель горит.
    val burnCut =
      if (hero.sets.skillsAlwaysIgnite) battle.effects.monsterBurn.map(_.pct).getOrElse(0) else 0
    (red0 - (battle.effects.monsterColdDefenceCut + burnCut) / 100.0).max(0.0)
  }

  /** Стихия, которой бьёт моб: своя по природе (Белый волк — холодом) либо
    * наведённая «Порошком!», если он его высыпал. */
  private def powderElement(battle: SoloPveBattle): Option[Element] =
    battle.boss.flatMap(_.attackElement)
      .orElse(battle.effects.monsterAttackElement.flatMap(Element.withNameOption))

  /** Доли урона по броне и HP от стихии удара моба. У каменного элементаля
    * свой раскол, он важнее — там это природа удара, а не стихия. */
  private def powderSplit(battle: SoloPveBattle): Option[(Double, Double)] =
    powderElement(battle).map(e => (e.armorMult, e.hpMult))

  /** «Каменный страж» (порог 4): стихийный урон по герою слабее на 20%.
    * Стихийным считается ВЕСЬ урон элементаля — он не бьёт «обычной атакой», он
    * и есть огонь: и удар, и всплеск, и смерч, и шипы, и горение. У прочих мобов
    * стихийного урона нет, поэтому им порог ничего не режет. */
  private def bossDamageTaken(hero: Hero, battle: SoloPveBattle, damage: Long): Long =
    if (battle.boss.isEmpty || damage <= 0L) damage
    else (damage * hero.sets.elementalDamageTakenMult).toLong.max(1L)

  /** Урон герою от способности минибосса: сперва броня, остаток в HP. Сам по
    * себе НИЧЕГО не поджигает — пламя вешает тот, кто им владеет. Возвращает
    * героя и сколько урона реально прошло (для строки лога). Расход брони режет
    * порог 6 «Каменного стража» — прикрывает она при этом всё так же. */
  private def hurtHero(hero: Hero, damage: Long): (Hero, Long) = {
    val toArmor = damage.min(hero.fightStats.armor)
    val toHp    = damage - toArmor
    (hero.copy(fightStats = hero.fightStats.copy(
       armor = hero.fightStats.armor - hero.sets.armorSpent(toArmor),
       hp    = (hero.fightStats.hp - toHp).max(0L))),
     damage)
  }

  /** Разбивает `damage` на урон по броне и по HP с учётом стихийных модификаторов
    * оружия. Здесь же учитывается усиление стихии (+2%/грейд): оно входит в
    * `armorDamageMult`/`hpDamageMult` процентными пунктами, а не множит общий
    * урон — см. [[pangea.model.hero.HeroGems.elementalBoost]]. Без стихий
    * (мультипликаторы = 1, доля молнии = 0) поведение прежнее: броня поглощает
    * `min(armor, damage)`, остальное — в HP. */
  private def splitElementalDamage(curArmor: Long, damage: Long, hero: Hero): (Long, Long) = {
    val g = hero.gems
    // «Дикое пламя» (порог 4): обе грани урона огнём выше на свои п.п. Бонус
    // работает только если Огонь реально есть в оружии — набор усиливает стихию,
    // а не выдаёт её.
    val fireBonus    = if (g.hasElement(Element.Fire)) hero.sets.fireDamageBonus else 0.0
    val rawArmorPart = math.min(curArmor, damage)
    val rawHpPart    = damage - rawArmorPart
    val armorDmg     = math.min(curArmor, (rawArmorPart * (g.armorDamageMult + fireBonus)).toLong).max(0L)
    val hpFromHp     = (rawHpPart * (g.hpDamageMult + fireBonus)).toLong
    // Молния: часть урона по броне дополнительно бьёт по HP.
    val hpFromArmor  = (armorDmg * g.lightningArmorToHpFrac).toLong
    (armorDmg, (hpFromHp + hpFromArmor).max(0L))
  }

  /** Роллит проки стихий оружия (по 30% на каждую стихию в оружии) и применяет их:
    * сперва комбо (Огонь+Воздух, Молния+Холод), затем оставшиеся одиночные эффекты.
    * Возвращает обновлённый бой и строки лога. Урон комбо снимается сразу (может
    * добить моба — исход перепроверяется вызывающим по `monsterCurrentHp`). */
  private def resolveElementProcs(hero: Hero, battle: SoloPveBattle): Task[(SoloPveBattle, Vector[String])] = {
    val elems = hero.gems.weaponElements
    if (elems.isEmpty) ZIO.succeed((battle, Vector.empty))
    else
      // Роллим в фиксированном порядке Element.values для детерминизма тестов.
      ZIO.foreach(Element.values.filter(elems.contains).toList) { e =>
        // «Дикое пламя» (порог 10) поднимает шанс поджечь — только у Огня.
        val chance =
          if (e == Element.Fire) Element.ProcChancePct + hero.sets.igniteChanceBonusPct
          else Element.ProcChancePct
        Random.nextIntBetween(1, 101).map(roll => e -> (roll <= chance))
      }.map(rolls => applyProcs(battle, rolls.collect { case (e, true) => e }.toSet))
  }

  /** Применяет сработавшие проки стихий `fired` к мобу: сперва комбо, затем
    * одиночные. Чистая функция — ею же пользуется стихийная фляга, у которой прок
    * гарантирован. */
  private def applyProcs(battle: SoloPveBattle, fired: Set[Element]): (SoloPveBattle, Vector[String]) = {
    val maxHp      = battle.monsterStats.hp
    val maxArmor   = MonsterSkill.monsterMaxArmor(battle)
    val fireAir    = fired(Element.Fire) && fired(Element.Air)
    val lightCold  = fired(Element.Lightning) && fired(Element.Cold)

    var b   = battle
    var log = Vector.empty[String]

    // Комбо Огонь+Воздух: мгновенный урон 30% макс.HP + текущий % горения, горение → 2%.
    if (fireAir) {
      val burnPct = b.effects.monsterBurn.map(_.pct).getOrElse(0)
      val dmg     = (maxHp.toDouble * (30 + burnPct) / 100.0).toLong.max(0L)
      b = b.copy(
        monsterCurrentHp = (b.monsterCurrentHp - dmg).max(0L),
        effects = b.effects.copy(monsterBurn = Some(Burn(Burn.Initial)))
      )
      log = log :+ content.format("battle.comboFireAir", "damage" -> dmg.toString)
    }

    // Комбо Молния+Холод: -10% макс.брони (не переходит в HP) и застрявшее
    // умение — ближайший каст моб пропускает целиком. Одиночные проки обеих
    // стихий при этом ОСТАЮТСЯ: комбо идёт им в довесок, а не вместо них.
    if (lightCold) {
      val armorCut = (maxArmor.toDouble * BattleState.ComboArmorCutPct / 100.0).toLong.max(0L)
      b = b.copy(
        monsterCurrentArmor = (b.monsterCurrentArmor - armorCut).max(0L),
        effects = b.effects.copy(monsterSkillBlockedTurns = BattleState.ComboSkillBlockTurns)
      )
      log = log :+ content.text("battle.comboLightningCold")
    }

    // Одиночные проки стихий — идут своим чередом, в том числе после комбо.
    if (fired(Element.Cold)) {
      b = b.copy(effects = b.effects.copy(
        monsterColdDefenceCut = b.effects.monsterColdDefenceCut + Element.Cold.DefenceReductionCut))
      log = log :+ Element.Cold.procText
      // Огненного элементаля прок Холода вдобавок сковывает: шипы молчат,
      // его атаки перестают поджигать, точность срезана, и одна собранная
      // сфера гаснет. Каменному холод ничего сверх обычного не делает.
      if (b.boss.contains(MiniBoss.FireElemental)) {
        val orbsLeft = (b.bossCharges - 1).max(0)
        if (b.bossCharges > 0)
          log = log :+ content.format("battle.elemental.orbDestroyed", "orbs" -> orbsLeft.toString)
        b = b.copy(
          bossCharges = orbsLeft,
          effects          = b.effects.copy(chilledTurns = MiniBoss.FireElemental.ChilledTurns))
      }
    }
    if (fired(Element.Fire) && !fireAir) {
      val burn = b.effects.monsterBurn.map(_.reignited).getOrElse(Burn.onIgnite)
      b = b.copy(effects = b.effects.copy(monsterBurn = Some(burn)))
      log = log :+ Element.Fire.procText
      // Каменный элементаль от пламени плывёт: бьёт слабее, теряет валун и
      // часть ПОТОЛКА брони (текущая при этом остаётся какая была).
      if (b.boss.contains(MiniBoss.StoneElemental)) {
        val left = (b.bossCharges - 1).max(0)
        if (b.bossCharges > 0)
          log = log :+ content.format("battle.elemental.boulderMelted", "boulders" -> left.toString)
        b = b.copy(
          bossCharges = left,
          effects = b.effects.copy(
            monsterWeakenedTurns = MiniBoss.StoneElemental.BurnedTurns,
            monsterWeakenedPct   = MiniBoss.StoneElemental.BurnedDamageCutPct,
            monsterMaxArmorCut   = b.effects.monsterMaxArmorCut + MiniBoss.StoneElemental.BurnedMaxArmorCut))
        log = log :+ content.format("battle.elemental.stoneMelts",
                "pct" -> MiniBoss.StoneElemental.BurnedDamageCutPct.toString,
                "turns" -> MiniBoss.StoneElemental.BurnedTurns.toString,
                "armor" -> MiniBoss.StoneElemental.BurnedMaxArmorCut.toString)
      }
    }
    if (fired(Element.Air) && !fireAir) {
      b = b.copy(effects = b.effects.copy(airBoostTurns = Element.Air.ProcTurns))
      log = log :+ Element.Air.procText
    }
    if (fired(Element.Lightning)) {
      // Выжиг энергии: снимаем долю от ПОТОЛКА, поэтому эффект не зависит от
      // того, сколько моб успел накопить, и одинаково чувствуется на любом
      // уровне. Умение, на которое он копил, откладывается. Строка одна: и
      // про сам разряд, и про сожжённую энергию.
      val burned = (b.monsterStats.energy * Element.LightningEnergyBurnPct / 100L).max(1L)
      val left   = (b.monsterCurrentEnergy - burned).max(0L)
      val lost   = b.monsterCurrentEnergy - left
      b = b.copy(monsterCurrentEnergy = left)
      log = log :+ content.format("battle.lightningBurn", "energy" -> lost.toString)
    }
    (b, log)
  }

  /** Умения, которые моб может применить прямо сейчас: доступные его расе,
    * полезные в этой ситуации и оплатимые текущей энергией. */
  private def affordableSkills(battle: SoloPveBattle): Seq[MonsterSkill] = {
    val race = Race.withName(battle.monsterRace)
    MonsterSkill.values.filter { s =>
      s.availableTo(race) && s.applicable(battle) && s.cost(battle.monsterLvl) <= battle.monsterCurrentEnergy
    }
  }

  /** Итоговая защита моба с учётом временного %-дебафа (комбо Молния+Холод). */
  private def effectiveMonsterDefence(battle: SoloPveBattle): Long =
    battle.effects.monsterDefenceDebuff match {
      case Some(d) => (battle.monsterStats.defence * (100 - d.pct) / 100).max(0L)
      case None    => battle.monsterStats.defence
    }

  /** Урон яда или кровотечения по мобу с поправкой на его природу: зверь из
    * плоти и крови (Белый волк) истекает на пятую часть сильнее. Горение сюда
    * не входит. */
  private def monsterDotDamage(battle: SoloPveBattle, base: Long): Long =
    battle.boss.map(_.dotDamageTakenMult).filter(_ != 1.0).fold(base)(m => (base * m).toLong)

  /** Компактная сводка активных DoT на мобе для приписки к строкам атаки:
    * ` (🟢 -N ❤)(🔴 -N ❤)(🔥 -N ❤)`. Пусто, если эффектов нет. */
  private def dotIndicators(battle: SoloPveBattle): String = {
    val maxHp = battle.monsterStats.hp
    val parts = List(
      battle.effects.monsterPoison.map(p => s"🟢 -${monsterDotDamage(battle, p.damageOn(maxHp))} ❤"),
      battle.effects.monsterBleed.map(b => s"🔴 -${monsterDotDamage(battle, b.damageOn(maxHp))} ❤"),
      battle.effects.monsterBurn.map(bn => s"🔥 -${bn.damageOn(maxHp)} ❤")
    ).flatten
    if (parts.isEmpty) "" else parts.mkString(" (", ")(", ")")
  }

  // ── Ход монстра ─────────────────────────────────────────────────────────────

  /** Ход моба после атаки игрока. `log` — строки, накопленные в фазе игрока;
    * между сегментами игрока и монстра вставляется пустая строка-разделитель
    * (в склейке `mkString("\n")` даёт двойной перенос).
    */
  /** Что случилось с героем от обычной атаки моба: новые HP и броня, сам урон,
    * побочные строки (Непробиваемый, Крепкость, шипы, поджог, яд, проки
    * порошка) и бой после удара (шипы, порошок). Строку «наносит N урона»
    * собирает вызывающий — у активного моба и у бьющего сбоку она разная. */
  private def mobStrike(hero: Hero, battle: SoloPveBattle, buffedEff: FightStats, nowMs: Long): Task[MobStrike] = {
    val monster = battle.toMonster
    for {
            spread <- Random.nextLongBetween(80L, 121L)
            rawDamage = (monsterAttack(battle) * spread / 100L * Achievement.damagePct(hero) / 100L).max(1L)
            reduction = heroDamageReduction(hero, battle, buffedEff, monster.fightStats.atk, nowMs)
            baseReduced = (rawDamage * (1.0 - reduction)).toLong.max(1L)
            // «Непробиваемый»: 20% шанс срезать полученный урон обычной атаки вдвое.
            impenTriggered <- chanceRoll(hero.passives.hasImpenetrable, PassiveKind.Impenetrable.TriggerPct)
            impenDamage =
              if (impenTriggered) (baseReduced * (100L - PassiveKind.Impenetrable.ReductionPct) / 100L).max(1L)
              else baseReduced
            // Удар элементаля — это стихия, а не сталь: «Каменный страж» его режет.
            reducedDamage = bossDamageTaken(hero, battle, impenDamage)
            (newHp, newArmor) = bossHit(battle, hero, reducedDamage)
            // «Крепкость»: при обнулении брони 25% шанс одноразово восстановить 10% макс.брони.
            armorEmptied = hero.fightStats.armor > 0 && newArmor <= 0
            toughTriggered <- chanceRoll(
              armorEmptied && !battle.toughnessUsed && hero.passives.hasToughness,
              PassiveKind.Toughness.TriggerPct
            )
            restoredArmor =
              if (toughTriggered) (hero.effectiveMaxArmor(nowMs) * PassiveKind.Toughness.RestorePct / 100L).max(1L) else 0L
            // «Шипастый»: вернуть 5% полученного урона врагу (по HP моба, мимо брони).
            thorns = if (hero.passives.hasSpiky) (reducedDamage * PassiveKind.Spiky.ThornsPct / 100L).max(1L) else 0L
            // Поджигает героя обычной атакой только тот, у кого огонь в природе
            // (огненный элементаль), и только пока не скован холодом. У камня и
            // гнили шанс нулевой, поэтому бросок у них не тратится.
            // Огненный порошок даёт мобу тот же шанс поджечь, что и обычный прок Огня.
            igniteChance = battle.boss.map(_.heroIgniteChancePct)
                             .getOrElse(if (powderElement(battle).contains(Element.Fire)) Element.ProcChancePct else 0L)
            ignites <- chanceRoll(
              igniteChance > 0L && !battle.effects.chilled,
              (igniteChance - hero.sets.igniteResistPct).max(0L)
            )
            burnPct = battle.boss.map(_.heroIgniteBurnPct).getOrElse(Burn.Initial)
            // Прок стихии порошка — тот же 30%-й бросок, что и у стихий оружия.
            // Огонь уже отыгран поджогом выше, здесь остаются холод и воздух.
            procElement = powderElement(battle).filter(e => e == Element.Cold || e == Element.Air)
            mobProc <- chanceRoll(procElement.isDefined, Element.ProcChancePct)
            // Порошок мурлока и эльфа: удар, дошедший до HP, всегда травит.
            poisons = battle.effects.monsterPoisonsOnHit && newHp < hero.fightStats.hp
            effectsAfterHit = {
              val burned =
                if (!ignites) battle.effects
                else battle.effects.copy(heroBurn = Some(
                  battle.effects.heroBurn.map(_.reignited).getOrElse(Burn(burnPct))))
              val poisonedE =
                if (!poisons) burned
                else burned.copy(heroPoison = Some(
                  burned.heroPoison.map(p => Poison(p.pct + Poison.OnHit)).getOrElse(Poison.onHit)))
              // Холод обгрызает защиту героя (накопительно, не затухает), воздух
              // поднимает мобу точность и уклонение на несколько ходов.
              (if (mobProc) procElement else None) match {
                case Some(Element.Cold) =>
                  poisonedE.copy(heroColdDefenceCut =
                    poisonedE.heroColdDefenceCut + Element.Cold.DefenceReductionCut)
                case Some(Element.Air) =>
                  poisonedE.copy(mobAirBoostTurns = Element.Air.ProcTurns)
                case _ => poisonedE
              }
            }
            battleAfterAtk = battle.copy(
              monsterCurrentHp = (battle.monsterCurrentHp - thorns).max(0L),
              toughnessUsed    = battle.toughnessUsed || toughTriggered,
              effects          = effectsAfterHit
            )
            lines = List(
              Option.when(impenTriggered)(content.text("battle.impenetrable")),
              Option.when(toughTriggered)(content.format("battle.toughness", "armor" -> restoredArmor.toString)),
              Option.when(thorns > 0)(content.format("battle.thorns", "damage" -> thorns.toString, "monster" -> battle.monsterName)),
              Option.when(ignites)(content.text("battle.elemental.ignites")),
              Option.when(poisons)(content.text("battle.mobPoisons")),
              Option.when(mobProc && procElement.contains(Element.Cold))(content.text("battle.mobColdBite")),
              Option.when(mobProc && procElement.contains(Element.Air))(content.text("battle.mobAirBoost"))
            ).flatten
          } yield MobStrike(newHp, newArmor + restoredArmor, reducedDamage, lines, battleAfterAtk)
  }

  /** Умение моба поверх обычной атаки: самое дорогое по карману, оплачивается
    * энергией. «Охотник» может погасить первую вредную способность. Возвращает
    * бой моба, героя и строку лога (пустую, если кастовать нечего). */
  private def mobSkillCast(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[(SoloPveBattle, Hero, String)] =
    for {
      affordable <- ZIO.succeed(affordableSkills(battle).toVector)
      out <-
        if (affordable.nonEmpty)
          for {
            best <- ZIO.succeed {
                      val top = affordable.map(_.cost(battle.monsterLvl)).max
                      affordable.filter(_.cost(battle.monsterLvl) == top)
                    }
            idx <- Random.nextIntBetween(0, best.size)
            ms   = best(idx)
            paid = battle.copy(monsterCurrentEnergy = (battle.monsterCurrentEnergy - ms.cost(battle.monsterLvl)).max(0L))
            cast = ms.cast(paid, hero, nowMs)
            hurts = cast.heroHp < hero.fightStats.hp || cast.heroArmor < hero.fightStats.armor
            cancels = hurts && hero.sets.cancelsFirstEnemySkill && !battle.effects.cancelSpent
          } yield
            if (cancels)
              (cast.battle.copy(effects = cast.battle.effects.copy(cancelSpent = true)), hero, content.text("battle.hunterCancel"))
            else
              (cast.battle, hero.copy(fightStats = hero.fightStats.copy(hp = cast.heroHp, armor = cast.heroArmor)), cast.line)
        else ZIO.succeed((battle, hero, ""))
    } yield out

  private def monsterPhase(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      log: Vector[String],
      skip: Set[Long]
  ): Task[TurnResult] =
    for {
      ticked <- ZIO.succeed(battle.tickBuffs(skip))
      buffedEff = effWithAir(hero, ticked, nowMs)
      hitRoll <- Random.nextIntBetween(1, 101)
      dodge = playerDodgeChance(hero, ticked, nowMs)
      // Тот же порядок округления, что на экране боя: 100 - floor(dodge).
      mobHitPct = 100 - dodge.toInt

      // 1) Обычная атака моба — обновляем hp/armor героя и (для Шипастого/Крепкости) бой.
      atkResult <-
        if (hitRoll > dodge)
          mobStrike(hero, ticked, buffedEff, nowMs).map { st =>
            val line = content.format("battle.mobHit", "damage" -> st.damage.toString, "monster" -> ticked.monsterName)
            (st.newHp, st.newArmor, (line :: st.extraLines).mkString("\n"), st.battle)
          }
        else
          ZIO.succeed(
            (
              hero.fightStats.hp,
              hero.fightStats.armor,
              content.format(
                "battle.mobMiss",
                "monster" -> ticked.monsterName,
                "chance"  -> mobHitPct.toString
              ),
              ticked
            )
          )
      (hpAfterAtk, armorAfterAtk, mobLine0, battleAfterAtk) = atkResult
      // Удар по HP может оставить травму — сразу за строкой удара.
      struckTrauma <- hitTrauma(hero, hero.copy(fightStats = hero.fightStats.copy(hp = hpAfterAtk, armor = armorAfterAtk)), nowMs)
      (heroAfterAtk, strikeTraumaLine) = struckTrauma
      mobLine = strikeTraumaLine.fold(mobLine0)(l => mobLine0 + "\n" + l)

      // 2) Умение моба оплачивается ЭНЕРГИЕЙ, а не броском кубика: моб берёт самое
      // дорогое из того, что сейчас по карману, применимо и доступно его расе, и
      // списывает цену. Не хватило — бьёт только обычной атакой и копит дальше.
      // Умение идёт ПОВЕРХ обычной атаки и может быть как уроном (CrushingStrike —
      // мимо брони и damageReduction), так и хилом/починкой моба.
      castResult <-
        // Моб не кастует, если герой мёртв или моб добит Шипастым (battleAfterAtk).
        if (heroAfterAtk.fightStats.hp <= 0 || battleAfterAtk.monsterCurrentHp <= 0)
          ZIO.succeed((battleAfterAtk, heroAfterAtk, ""))
        // Скован комбо Молния+Холод: умение застряло, и круг минибосса тоже стоит.
        else if (battleAfterAtk.effects.monsterSkillBlockedTurns > 0)
          ZIO.succeed((battleAfterAtk, heroAfterAtk, content.text("battle.skillBlocked")))
        // Минибосс не выбирает умение: он идёт строго по своему кругу.
        else if (battleAfterAtk.boss.isDefined)
          bossTurnCast(heroAfterAtk, battleAfterAtk, nowMs)
        else mobSkillCast(heroAfterAtk, battleAfterAtk, nowMs)
      (finalBattle, finalHero0, castLine0) = castResult
      // Приём по HP — тоже может оставить травму.
      castTrauma <- hitTrauma(heroAfterAtk, finalHero0, nowMs)
      (finalHero, castTraumaLine) = castTrauma
      castLine = castTraumaLine.fold(castLine0)(l => (if (castLine0.isEmpty) "" else castLine0 + "\n") + l)

      // 3) Конец раунда: тик статус-эффектов. Стадии героя и монстра — раздельные,
      // каждая со своей строкой. Применяются только пока герой жив.
      heroAlive = finalHero.fightStats.hp > 0
      (tickedHero, battleAfterHero, heroEffectLine) =
        if (heroAlive) tickHeroEffects(finalHero, finalBattle, nowMs)
        else (finalHero, finalBattle, "")
      // Энергия копится в конце раунда — из неё оплачивается следующее умение.
      // У минибосса свой реген от BossLvL, у обычного моба — от уровня и редкости.
      battleWithEnergy = {
        val maxEn = battleAfterHero.monsterStats.energy
        val gain  = battleAfterHero.boss match {
          case Some(e) => e.energyRegen(battleAfterHero.monsterLvl)
          case None    => MonsterEnergy.regen(battleAfterHero.monsterLvl, battleAfterHero.rarity)
        }
        battleAfterHero.copy(monsterCurrentEnergy = (battleAfterHero.monsterCurrentEnergy + gain).min(maxEn))
      }
      (tickedBattle, monsterEffectLine, bleedDealt) =
        if (heroAlive) tickMonsterEffects(battleWithEnergy, ticked.monsterName, tickedHero.sets.burnGrowthMult)
        else (battleWithEnergy, "", 0L)
      // «Упырь» (порог 10): чужая кровь идёт герою в лечение.
      heroFedByBleed =
        if (bleedDealt > 0 && tickedHero.sets.healsFromBleed)
          tickedHero.copy(fightStats = tickedHero.fightStats.copy(
            hp = (tickedHero.fightStats.hp + bleedDealt).min(tickedHero.effectiveMaxHp(nowMs))))
        else tickedHero

      // Реген энергии в конце хода: +(Интеллект + 0.5·Ловкость), не меньше 1 и не
      // выше максимума. «Сосредоточенность» множит реген на 1.1. Пока герой жив.
      finalHeroWithEnergy = if (heroAlive) regainEnergy(heroFedByBleed, nowMs) else heroFedByBleed

      // Сегмент монстра: пустой разделитель, строка атаки, затем (в исходном
      // порядке) каст моба, тик яда моба, тик регена героя — только непустые.
      monsterLog = {
        // К строке атаки моба приписываем сводку активных DoT (яд/кровь/огонь) —
        // берём состояние ДО тика конца раунда (finalBattle), как на стороне игрока.
        val base       = (log :+ "") :+ (mobLine + dotIndicators(finalBattle))
        val withCast   = if (castLine.nonEmpty) base :+ castLine else base
        val withPoison = if (monsterEffectLine.nonEmpty) withCast :+ monsterEffectLine else withCast
        if (heroEffectLine.nonEmpty) withPoison :+ heroEffectLine else withPoison
      }

      outcome =
        if (finalHeroWithEnergy.fightStats.hp <= 0) Outcome.Death
        else if (tickedBattle.monsterCurrentHp <= 0) Outcome.Victory
        else Outcome.Continue
    } yield TurnResult(finalHeroWithEnergy, tickedBattle, monsterLog, outcome)

  /** Реген энергии героя в конце раунда: +(Интеллект + 0.5·Ловкость), не меньше
    * 1 и не выше максимума. «Сосредоточенность» (пассивка) и черепа в оружии
    * множат реген, а «Охотник» (порог 4) удваивает вклад именно ловкости. Одна
    * точка на оба конца раунда: после хода моба и на добивании, когда до хода
    * моба не дошло. */
  private def regainEnergy(hero: Hero, nowMs: Long): Hero = {
    val b       = hero.effectiveBaseStats(nowMs)
    val agiPart = 0.5 * b.agi * hero.sets.agiEnergyRegenMult
    val regen   = ((b.int + agiPart) * hero.passives.energyRegenMult * hero.gems.energyRegenMult).toLong.max(1L)
    val maxEn   = hero.maxEnergy(nowMs)
    hero.copy(fightStats = hero.fightStats.copy(energy = (hero.fightStats.energy + regen).min(maxEn)))
  }

  /** Победа ударом или умением героя: раунд кончился без хода моба, но энергия
    * за него всё равно восстанавливается, а бафы, кулдауны и «фляга уже пита»
    * отсчитывают раунд, как после хода моба (`tickBuffs`, `skip` — слот только
    * что применённого умения). Без этого в группе следующий моб встаёт в пару,
    * а фляга и умения считают, что раунд не кончился. В бою 1 на 1 тик
    * безобиден — бой всё равно кончился. */
  private def victoryByHero(hero: Hero, battle: SoloPveBattle, log: Vector[String], nowMs: Long, skip: Set[Long]): TurnResult =
    TurnResult(regainEnergy(hero, nowMs), battle.tickBuffs(skip), log, Outcome.Victory)

  /** Тик эффектов ГЕРОЯ в конце раунда: реген лечит на `pct`% макс.HP и слабеет.
    * Возвращает обновлённых героя и бой (с ослабленным регеном) плюс строку
    * эффекта (пустая, если регена нет). */
  private def tickHeroEffects(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long
  ): (Hero, SoloPveBattle, String) = {
    // Горение на герое (огненный элементаль) тикает до регена: оно усиливается
    // каждый раунд, как и горение на мобе.
    val (burnedHero, burnedBattle, burnLine) = battle.effects.heroBurn match {
      case Some(burn) =>
        val maxHp = hero.effectiveMaxHp(nowMs)
        val dmg   = bossDamageTaken(hero, battle, burn.damageOn(maxHp))
        (hero.copy(fightStats = hero.fightStats.copy(hp = (hero.fightStats.hp - dmg).max(0L))),
         battle.copy(effects = battle.effects.copy(heroBurn = Some(burn.grown))),
         content.format("battle.elemental.burnTick", "damage" -> dmg.toString))
      case None => (hero, battle, "")
    }
    // Яд на герое: снимает % макс.HP мимо брони и слабеет, как яд на мобе.
    // Гниль Джо и отравленное оружие мобов делят одно поле, но не одну строку:
    // разъедающая гниль — это про минибосса, у обычного яда свой текст.
    val (poisonedHero, poisonedBattle, poisonLine) = burnedBattle.effects.heroPoison match {
      case Some(poison) =>
        val dmg = poison.damageOn(burnedHero.effectiveMaxHp(nowMs))
        val key =
          if (burnedBattle.boss.contains(MiniBoss.RottenJoe)) "battle.joe.poisonTick"
          else "battle.heroPoisonTick"
        (burnedHero.copy(fightStats = burnedHero.fightStats.copy(
           hp = (burnedHero.fightStats.hp - dmg).max(0L))),
         burnedBattle.copy(effects = burnedBattle.effects.copy(heroPoison = poison.decayed)),
         content.format(key, "damage" -> dmg.toString))
      case None => (burnedHero, burnedBattle, "")
    }
    // Кровотечение на герое (пасть и когти волка): % макс.HP мимо брони, не
    // затухает — снимает его только лечение (см. activeHeal).
    val (bledHero, bleedLine) = poisonedBattle.effects.heroBleed match {
      case Some(bleed) =>
        val dmg = bleed.damageOn(poisonedHero.effectiveMaxHp(nowMs))
        (poisonedHero.copy(fightStats = poisonedHero.fightStats.copy(
           hp = (poisonedHero.fightStats.hp - dmg).max(0L))),
         content.format("battle.heroBleedTick", "damage" -> dmg.toString))
      case None => (poisonedHero, "")
    }
    val (finalHero, finalBattle, regenLine) = poisonedBattle.effects.heroRegen match {
      case Some(regen) =>
        val maxHp  = bledHero.effectiveMaxHp(nowMs)
        // Регенерация горением НЕ режется: это не действие игрока, а эффект,
        // который тикает сам по себе (см. activeHeal).
        val heal   = regen.healOn(maxHp)
        val newHp  = (bledHero.fightStats.hp + heal).min(maxHp)
        val healed = newHp - bledHero.fightStats.hp
        (
          bledHero.copy(fightStats = bledHero.fightStats.copy(hp = newHp)),
          poisonedBattle.copy(effects = poisonedBattle.effects.copy(heroRegen = regen.decayed)),
          content.format("battle.regenTick", "healed" -> healed.toString)
        )
      case None => (bledHero, poisonedBattle, "")
    }
    // Строки бывают непустыми одновременно (горим, травимся и регенерируем) — склеиваем.
    (finalHero, finalBattle, List(burnLine, poisonLine, bleedLine, regenLine).filter(_.nonEmpty).mkString("\n"))
  }

  /** Тик DoT-эффектов МОНСТРА в конце раунда (каждый снимает `pct`% макс.HP мимо
    * брони). Порядок: яд → кровотечение → горение. Яд слабеет ([[Poison.decayed]]),
    * кровотечение НЕ затухает, горение усиливается ([[Burn.grown]]). Возвращает
    * обновлённый бой и склеенную строку эффектов (пустую, если DoT нет). */
  private def tickMonsterEffects(
      battle: SoloPveBattle,
      monsterName: String,
      burnGrowthMult: Long
  ): (SoloPveBattle, String, Long) = {
    val maxHp = battle.monsterStats.hp
    var eff   = battle.effects
    var hp    = battle.monsterCurrentHp
    var lines = Vector.empty[String]
    var bled  = 0L // урон кровотечения за этот тик — «Упырь» (порог 10) лечит им героя

    eff.monsterPoison.foreach { p =>
      val dmg = monsterDotDamage(battle, p.damageOn(maxHp))
      hp = (hp - dmg).max(0L)
      eff = eff.copy(monsterPoison = p.decayed)
      lines = lines :+ content.format("battle.poisonTick", "damage" -> dmg.toString, "monster" -> monsterName)
    }
    eff.monsterBleed.foreach { b =>
      val dmg = monsterDotDamage(battle, b.damageOn(maxHp))
      hp = (hp - dmg).max(0L)
      bled += dmg
      // Кровотечение не затухает — сила остаётся прежней.
      lines = lines :+ content.format("battle.bleedTick", "damage" -> dmg.toString, "monster" -> monsterName)
    }
    eff.monsterBurn.foreach { bn =>
      val dmg = bn.damageOn(maxHp)
      hp = (hp - dmg).max(0L)
      // «Дикое пламя» (порог 6) ускоряет рост: шаг применяется несколько раз,
      // сохраняя саму кривую роста (+2 п.п. до порога, дальше +1).
      eff = eff.copy(monsterBurn = Some(
        (1L to burnGrowthMult.max(1L)).foldLeft(bn)((b, _) => b.grown)))
      lines = lines :+ content.format("battle.burnTick", "damage" -> dmg.toString, "monster" -> monsterName)
    }
    (battle.copy(monsterCurrentHp = hp, effects = eff), lines.mkString("\n"), bled)
  }

  // ── Ход игрока: активный навык ──────────────────────────────────────────────

  /** Кнопка умения. В группе умение с уроном достаёт и соседа героя (моб под
    * номером 2), поэтому сначала спрашиваем, в кого бить; лечение и броня — на
    * себя, без вопросов. Неготовому умению экран цели не нужен — обычный ход сам
    * скажет, почему нельзя. */
  private def skillRoute(user: User, itemId: Long, target: Option[Int], renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      state  <-
        if (target.isEmpty && needsTarget(hero, battle, itemId))
          renderer.show(user, targetScreen(battle, itemId)).as(StateType.Battle)
        else resolveLoaded(user, renderer, hero, battle, now)(
          skillTurn(itemId, target.orElse(battle.group.attackTargets.headOption).getOrElse(battle.group.heroPos)))
    } yield state

  /** Спрашивать ли цель: рядом с героем кто-то стоит, умение готово и бьёт. */
  private def needsTarget(hero: Hero, battle: SoloPveBattle, itemId: Long): Boolean =
    battle.slotByItem(itemId).exists { slot =>
      val ready = slot.cooldown <= 0 && hero.fightStats.energy >= slot.skill.energyCost(hero)
      ready && (
        (BattleState.dealsDamage(slot.skill.effect) && battle.group.attackTargets.size > 1) ||
        (BattleState.supports(slot.skill.effect) && battle.group.allies.exists(_.alive)))
    }

  /** Экран выбора цели для базовой атаки: те же кнопки, что у умения с уроном. */
  private def attackTargetScreen(battle: SoloPveBattle): Screen =
    targetChoices(battle, "Attack", content.text("battle.group.attackLabel"), support = false)

  /** Экран выбора цели: моб напротив и соседи по местам, с их полосками.
    * Подпись кнопки ограничена по длине, поэтому имя — короткое (у отмеченного
    * тьмой — только раса), а что всё равно не влезло, срезается. */
  private def targetScreen(battle: SoloPveBattle, itemId: Long): Screen = {
    val slot       = battle.slotByItem(itemId)
    val skillLabel = slot.map(_.skill.label).getOrElse("")
    val support    = slot.exists(s => BattleState.supports(s.skill.effect))
    targetChoices(battle, s"Skill_$itemId", skillLabel, support)
  }

  /** Кнопки целей под действие `choiceId`: лечение и броня — себе или
    * союзнику по позициям отряда; урон — моб в паре и соседи по местам. */
  private def targetChoices(battle: SoloPveBattle, choiceId: String, label: String, support: Boolean): Screen = {
    val targets =
      if (support) {
        val me = pangea.engine.Choice(choiceId, content.text("battle.group.targetSelf"),
          data = Map("target" -> battle.group.heroPos.toString), row = Some(0))
        me :: battle.group.allies.filter(_.alive).sortBy(_.position).zipWithIndex.map { case (a, i) =>
          pangea.engine.Choice(choiceId,
            pangea.engine.Choice.fit(content.format("battle.group.targetAlly",
              "n" -> a.position.toString, "name" -> a.name, "hp" -> a.hpPct.toString, "armor" -> a.armorPct.toString)),
            data = Map("target" -> a.position.toString), row = Some(i + 1))
        }
      } else
        battle.group.attackTargets.zipWithIndex.flatMap { case (pos, i) =>
          battle.monsterAt(pos).map(m => pangea.engine.Choice(
            id    = choiceId,
            label = pangea.engine.Choice.fit(
              content.format("battle.group.targetLabel", "n" -> pos.toString, "monster" -> m.shortName, "hp" -> m.hpPct.toString)),
            data  = Map("target" -> pos.toString),
            row   = Some(i)))
        }
    val cancel = pangea.engine.Choice("CancelTarget", content.text("battle.group.cancelTarget"),
      color = pangea.engine.ChoiceColor.Negative, row = Some(targets.size))
    Screen(content.format("battle.group.chooseTarget", "skill" -> label), targets :+ cancel)
  }

  /** Применение активного навыка. Проверки (слот существует / готов / хватает
    * энергии) дают Continue-сообщение без траты хода. Умение ВСЕГДА срабатывает:
    * списываем энергию (в героя, персист — в commit), применяем эффект с ±20%
    * разбросом, ставим cd и инкрементируем uses. `target` — место цели в строю:
    * место героя — моб напротив, соседнее — сосед. */
  private def skillTurn(itemId: Long, target: Int)(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    battle.slotByItem(itemId) match {
      case None =>
        cont(hero, battle, content.text("battle.skillUnavailable"))
      case Some(slot) if slot.cooldown > 0 =>
        cont(hero, battle, content.text("battle.skillOnCooldown"))
      case Some(slot) if hero.fightStats.energy < slot.skill.energyCost(hero) =>
        cont(
          hero,
          battle,
          content.format(
            "battle.notEnoughEnergy",
            "cost"   -> slot.skill.energyCost(hero).toString,
            "energy" -> hero.fightStats.energy.toString
          )
        )
      case Some(slot) if BattleState.needsEnemy(slot.skill.effect) && battle.group.attackTargets.isEmpty =>
        cont(hero, battle, content.text("battle.group.noTarget"))
      case Some(slot) =>
        val cost      = slot.skill.energyCost(hero)
        val heroAfter = hero.copy(fightStats =
          hero.fightStats.copy(energy = (hero.fightStats.energy - cost).max(0L)))
        skillHit(heroAfter, battle, slot, nowMs, target)
    }

  /** Применяет эффект навыка (матч по Skill.Effect). После урона/лечения идёт
    * базовая атака игрока (кроме случая, когда моб убит самим скиллом). Кулдаун
    * использованного слота не тикается в этом же ходу (`skip`). Изменения статов
    * героя (энергия/HP Кровавой жатвы/лечение) едут в `hero` результата и
    * персистятся в commit — здесь никаких записей в БД.
    *
    * Удар по соседу (место в радиусе, не своё) идёт через временный шаг героя к
    * нему: сосед оказывается напротив, получает урон и проки, и герой шагает
    * обратно — базовая атака и ответ мобов идут по-прежнему. Убитый умением
    * сосед уходит в павшие сразу, Таран по живому соседу помечает место, на
    * которое герой шагнёт в конце раунда. Умения, задевающие остальных в строю
    * (дуга Размашистого — соседей цели, Вихрь и Веерный порез — всех в
    * досягаемости героя, см. [[strikeOthers]]), могут добить того, кто стоял в
    * полях до подмены; тогда возвращать некого, цель остаётся в полях, а пары у
    * героя больше нет. «Отбросить» меняет строй само (см. [[knockBack]]): в
    * полях после него стоит тот, кто вышел на место отброшенного. */
  private def skillHit(
      hero: Hero,
      battle: SoloPveBattle,
      slot: SkillSlotState,
      nowMs: Long,
      target: Int
  ): Task[TurnResult] = {
    val home        = battle.group.activePos
    val paired      = battle.group.paired
    val damageSkill = BattleState.dealsDamage(slot.skill.effect)
    // Цель не в полях (сосед в досягаемости) — разворачиваем её туда на время.
    val aimSide = damageSkill && target != home && battle.group.inReach(target)
    val aimed   = if (aimSide) battle.engage(target) else battle
    val skip    = Set(slot.itemId)
    // Таран по мобу не из пары помечает место, куда герой шагнёт в конце раунда.
    def ramMark(b: SoloPveBattle): SoloPveBattle =
      if (slot.skill == Skill.Ram && target != b.group.heroPos) b.copy(group = b.group.copy(pendingMove = Some(target))) else b
    // Что делать после урона. В паре: победа или базовая атака по паре (по
    // соседу — сперва вернуть пару в поля). Пары нет: базовая атака идёт по
    // той же цели, а у лечения — по единственному соседу, если он есть; ответа
    // пары нет — ход кончается тихо.
    val next: (Hero, SoloPveBattle, Vector[String]) => Task[TurnResult] = (h, b0, lines) => {
      val b    = if (damageSkill) b0.rememberTarget(target) else b0
      val dead = damageSkill && b.monsterCurrentHp <= 0
      if (!aimSide) {
        if (dead) ZIO.succeed(victoryByHero(h, b, lines, nowMs, skip))
        else if (paired) playerStrike(h, b, nowMs, lines, skip)
        else if (damageSkill) playerStrike(h, ramMark(b), nowMs, lines, skip)
        else followUp(h, b, nowMs, lines, skip)
      } else {
        val back = b.engage(home)
        // Прежний активный пал от отголоска умения (дуга, вихрь, порез): его
        // место пусто, `engage` ничего не вернул — в полях так и стоит цель, и
        // пары у героя нет.
        val homeGone = back.group.activePos != home
        if (dead) {
          if (homeGone) ZIO.succeed(victoryByHero(h, back, lines, nowMs, skip))
          else {
            val fallen = back.sideFallen(back.group.idxOf(target))
            val slain  = lines :+ content.format("battle.group.sideSlain", "monster" -> b.monsterName)
            if (paired) playerStrike(h, fallen, nowMs, slain, skip) else followUp(h, fallen, nowMs, slain, skip)
          }
        } else {
          val rammed = ramMark(back)
          if (paired && !homeGone) playerStrike(h, rammed, nowMs, lines, skip)
          else strikeSide(h, rammed, target, nowMs, lines, skip).flatMap(closeTurn(_, nowMs, skip))
        }
      }
    }
    for {
      spread <- Random.nextLongBetween(80L, 121L)
      raw = (slot.skill.baseValue(hero, nowMs) * spread / 100.0 * hero.weaponDust.damageMult).toLong.max(1L)
      bumped = aimed.updateSlot(slot.itemId)(s => s.copy(cooldown = s.skill.cooldown, uses = s.uses + 1))
      tmpl = slot.skill.hitTemplate
      // Урон, срезанный защитой моба (для эффектов, которые «упираются» в защиту).
      // Дебаф Холода снижает итоговое %-снижение цели на monsterColdDefenceCut п.п.;
      // временный дебаф защиты (комбо) режет саму защиту (effectiveMonsterDefence).
      reduced = (raw * (1.0 - monsterDefenceCut(hero, aimed, effWithAir(hero, aimed, nowMs), nowMs))).toLong.max(1L)
      result <- slot.skill.effect match {
        case Skill.Effect.Damage(reducedByDefence) =>
          val value = if (reducedByDefence) reduced else raw
          dealSkillDamage(hero, bumped, value, dealt => tmpl.replace("{}", dealt.toString), next)

        case Skill.Effect.Sweep(pct) =>
          // По цели — как удар, срезанный защитой; её соседям по строю — доля
          // от того, что по ней прошло.
          dealSkillDamage(hero, bumped, reduced, dealt => tmpl.replace("{}", dealt.toString), next,
            splash = (b, dealt) => strikeOthers(b, around(b.group.activePos)) { tmp =>
              val amount = (dealt * pct / 100L).max(1L)
              (plainHit(tmp, amount),
               content.format("battle.group.sweepSplash", "monster" -> tmp.monsterName, "damage" -> amount.toString))
            })

        case Skill.Effect.Whirl =>
          // По цели — как удар, срезанный защитой; каждому другому в
          // досягаемости героя — тот же бросок, срезанный уже его защитой.
          dealSkillDamage(hero, bumped, reduced, dealt => tmpl.replace("{}", dealt.toString), next,
            splash = (b, _) => strikeOthers(b, around(b.group.heroPos)) { tmp =>
              val amount = (raw * (1.0 - monsterDefenceCut(hero, tmp, effWithAir(hero, tmp, nowMs), nowMs))).toLong.max(1L)
              (plainHit(tmp, amount),
               content.format("battle.group.whirlHit", "monster" -> tmp.monsterName, "damage" -> amount.toString))
            })

        case Skill.Effect.FanBleed(pct) =>
          // Цели — урон и кровь, как у «Кровотечения»; каждому другому в
          // досягаемости героя — тот же урон и та же кровь (иммунитеты — в withEffects).
          def bleed(b: SoloPveBattle): SoloPveBattle =
            b.withEffects(b.effects.copy(monsterBleed = Some(b.effects.monsterBleed.map(_.stackedWith(pct)).getOrElse(Bleed(pct)))))
          dealSkillDamage(hero, bleed(bumped), raw, dealt => tmpl.replace("{}", dealt.toString), next,
            splash = (b, _) => strikeOthers(b, around(b.group.heroPos)) { tmp =>
              (bleed(plainHit(tmp, raw)),
               content.format("battle.group.fanCut", "monster" -> tmp.monsterName, "damage" -> raw.toString, "pct" -> pct.toString))
            })

        case Skill.Effect.WarCry(maxPct, turns) =>
          // Урона нет: все мобы в досягаемости героя — и в полях, и в строю —
          // теряют pct% защиты на `turns` ходов и пропускают ближайшее умение.
          val pct = raw.min(maxPct.toLong).max(1L).toInt
          def cowed(b: SoloPveBattle): SoloPveBattle = b.withEffects(b.effects.copy(
            monsterDefenceDebuff     = Some(TimedDefenceDebuff(pct, turns)),
            monsterSkillBlockedTurns = BattleState.WarCrySkillBlockTurns))
          val active  = if (bumped.group.attackTargets.contains(bumped.group.activePos)) cowed(bumped) else bumped
          val (shouted, _) = strikeOthers(active, around(active.group.heroPos))(tmp => (cowed(tmp), ""))
          next(hero, shouted, Vector(tmpl.replace("{}", pct.toString)))

        case Skill.Effect.Knockback =>
          // Урон без защиты; выживший отлетает в конец строя, а последний
          // выходит на его место — базовая атака и ответ идут уже по нему.
          val line: Long => String = dealt =>
            tmpl.replaceFirst("\\{\\}", java.util.regex.Matcher.quoteReplacement(aimed.monsterName))
                .replaceFirst("\\{\\}", dealt.toString)
          val thrown: (Hero, SoloPveBattle, Vector[String]) => Task[TurnResult] = (h, b, lines) =>
            if (b.monsterCurrentHp <= 0) next(h, b, lines)
            else knockBack(b) match {
              case None                    => next(h, b, lines)
              case Some((moved, newcomer)) =>
                next(h, moved, lines :+ content.format("battle.group.knockedBack", "monster" -> b.monsterName, "next" -> newcomer))
            }
          dealSkillDamage(hero, bumped, raw, line, thrown)

        case Skill.Effect.BleedDamage(pct) =>
          // Урон сразу + наложение (стак) КРОВОТЕЧЕНИЯ на моба (отдельно от яда).
          val stacked  = bumped.effects.monsterBleed.map(_.stackedWith(pct)).getOrElse(Bleed(pct))
          val bled     = bumped.withEffects(bumped.effects.copy(monsterBleed = Some(stacked)))
          // Отдельное сообщение «истекает кровью» убрано — прок уже виден по
          // компактному индикатору (🔴 -N ❤), приписанному к строке атаки.
          dealSkillDamage(hero, bled, raw, dealt => tmpl.replace("{}", dealt.toString), next)

        case Skill.Effect.WeakSpotStrike =>
          for {
            roll <- Random.nextIntBetween(1, 101)
            chance = (2L * hero.effectiveBaseStats(nowMs).int - aimed.monsterLvl).max(0L).min(95L)
            doubled = roll <= chance
            value   = if (doubled) raw * 2L else raw
            line: (Long => String) = (dealt: Long) =>
              tmpl.replace("{}", dealt.toString) +
                (if (doubled) "\n" + content.text("battle.weakSpotDouble") else "")
            r <- dealSkillDamage(hero, bumped, value, line, next)
          } yield r

        case Skill.Effect.BloodHarvest =>
          // Урон (уже включает +8% тек.HP в baseValue) ценой 10% текущего HP.
          // Раненый герой едет дальше в цепочке и персистится в commit ЕДИНООБРАЗНО
          // во всех ветках победы — HP-цена не теряется при добивании базовой атакой.
          val hpLost      = (hero.fightStats.hp * Skill.BloodHarvestHpCostPct / 100L).max(0L)
          val woundedHero = hero.copy(fightStats = hero.fightStats.copy(hp = (hero.fightStats.hp - hpLost).max(1L)))
          // Первый {} — урон по цели (пишем фактический), второй — цена крови героя.
          val line: Long => String = dealt =>
            tmpl.replaceFirst("\\{\\}", dealt.toString).replaceFirst("\\{\\}", hpLost.toString)
          dealSkillDamage(woundedHero, bumped, raw, line, next)

        // Лечение и броня по союзнику: то же число, но в его потолки; «Заслон»
        // союзнику даёт только броню — бафа защиты у него нет.
        case Skill.Effect.Heal | Skill.Effect.RepairArmor | Skill.Effect.GuardRepair(_, _)
            if target != aimed.group.heroPos && aimed.group.allyAt(target).exists(_.alive) =>
          val ally  = aimed.group.allyAt(target).get
          val heals = slot.skill.effect == Skill.Effect.Heal
          val full  = if (heals) (raw * hero.passives.healMult).toLong.max(1L) else raw
          val (updated, gained) =
            if (heals) { val nh = (ally.hp + full).min(ally.stats.hp); (ally.copy(hp = nh), nh - ally.hp) }
            else       { val na = (ally.armor + full).min(ally.stats.armor); (ally.copy(armor = na), na - ally.armor) }
          val line = content.format("battle.group.supportAlly", "name" -> ally.name,
            "skill" -> slot.skill.label, "amount" -> gained.toString, "what" -> (if (heals) "HP" else "брони"))
          next(hero, bumped.copy(group = bumped.group.updateAlly(ally.kind)(_ => updated)), Vector(line))

        case Skill.Effect.Heal =>
          // «Целитель» множит активное лечение на 1.1, дальше — общий расчёт.
          val full = (raw * hero.passives.healMult).toLong.max(1L)
          val (newHp, healed, healedBattle, cutLine) = activeHeal(hero, bumped, full, nowMs)
          val healedHero = hero.copy(fightStats = hero.fightStats.copy(hp = newHp))
          val lines      = Vector(tmpl.replace("{}", healed.toString)) ++ cutLine
          next(healedHero, healedBattle, lines)

        case Skill.Effect.RepairArmor =>
          val (newStats, gained) = repairArmor(hero, raw, nowMs)
          next(hero.copy(fightStats = newStats), bumped, Vector(tmpl.replace("{}", gained.toString)))

        case Skill.Effect.GuardRepair(defencePct, turns) =>
          // Восстановление брони + временный %-баф итоговой защиты.
          val (newStats, gained) = repairArmor(hero, raw, nowMs)
          val guarded = bumped.copy(heroBattleState = bumped.heroBattleState.add(
            Buff(0L, 0L, 0L, dodgePct = 0L, defencePct = defencePct, turnsLeft = Some(turns))))
          next(hero.copy(fightStats = newStats), guarded, Vector(tmpl.replace("{}", gained.toString)))
      }
    } yield result
  }

  /** Наносит `value` урона мобу в паре (сначала броня, затем HP) и отдаёт ход
    * дальше в `next`: там решается, победа это, базовая атака или возврат
    * временной пары (см. [[skillHit]]). Никаких записей в БД: итог (в т.ч.
    * изменённые статы героя) уезжает в TurnResult и персистится в commit. */
  /** `line` — шаблон строки умения: получает урон, который РЕАЛЬНО прошёл по
    * цели (после сопротивления минибосса и граней стихий), а не заявленный. */
  private def dealSkillDamage(
      hero: Hero,
      battle: SoloPveBattle,
      value: Long,
      line: Long => String,
      next: (Hero, SoloPveBattle, Vector[String]) => Task[TurnResult],
      // Что тот же удар делает с остальным строем (Размашистый, Вихрь, Веерный
      // порез): получает бой после удара по цели и урон, который по ней реально
      // прошёл.
      splash: (SoloPveBattle, Long) => (SoloPveBattle, Vector[String]) = (b, _) => (b, Vector.empty)
  ): Task[TurnResult] = {
    // «Охотник» (порог 12): первая за бой способность, наносящая урон, бьёт вдвое.
    val doubles = hero.sets.doublesFirstSkill && !battle.effects.doubleSpent
    val raw     = if (doubles) value.max(1L) * 2L else value.max(1L)
    // Урон навыка тоже несёт стихию оружия — раздельный урон по броне/HP с тем же
    // усилением. Проки стихий роллятся отдельным броском (как и на обычной атаке).
    val resisted          = (raw * bossResistance(hero, battle)).toLong.max(1L)
    val (armorDmg, hpDmg) = splitElementalDamage(battle.monsterCurrentArmor, resisted, hero)
    val skillLine         = line(armorDmg + hpDmg)
    val newArmor = battle.monsterCurrentArmor - armorDmg
    val newHp    = (battle.monsterCurrentHp - hpDmg).max(0L)
    // «Упырь» (порог 12): любое умение, нанёсшее урон, всегда пускает цели кровь.
    val bledEffects0 =
      if (!hero.sets.skillsAlwaysBleed || newHp <= 0) battle.effects
      else battle.effects.copy(monsterBleed = Some(
        battle.effects.monsterBleed.map(_.stackedWith(hero.sets.bleedPct)).getOrElse(Bleed(hero.sets.bleedPct))))
    // «Дикое пламя» (порог 12): умение, нанёсшее урон, всегда поджигает — уже
    // горящая цель разгорается сильнее, как от обычного прока Огня.
    val bledEffects =
      if (!hero.sets.skillsAlwaysIgnite || newHp <= 0) bledEffects0
      else bledEffects0.copy(monsterBurn = Some(
        bledEffects0.monsterBurn.map(_.reignited).getOrElse(Burn.onIgnite)))
    // Удвоение тратится на первом же уроне — даже если он добил моба.
    val effects  = if (doubles) bledEffects.copy(doubleSpent = true) else bledEffects
    val hit      = battle
      .copy(monsterCurrentHp = newHp, monsterCurrentArmor = newArmor)
      .withEffects(effects)
    if (newHp <= 0) {
      val (swept, splashLog) = splash(hit, armorDmg + hpDmg)
      next(hero, swept, Vector(skillLine + dotIndicators(hit)) ++ splashLog)
    } else
      resolveElementProcs(hero, hit).flatMap { case (afterProcs, elemLog) =>
        val (swept, splashLog) = splash(afterProcs, armorDmg + hpDmg)
        next(hero, swept, Vector(skillLine + dotIndicators(afterProcs)) ++ elemLog ++ splashLog)
      }
  }

  /** Места в досягаемости от `pos`: оно само и соседние (см. `GroupState.Reach`). */
  private def around(pos: Int): Range = (pos - GroupState.Reach) to (pos + GroupState.Reach)

  /** Умение задевает и остальных в строю: каждый живой моб на местах `positions`
    * (кроме того, что в полях) разворачивается во временную пару, `hit` бьёт его
    * там — урон и эффекты ложатся с иммунитетами через `withEffects` — и он
    * сворачивается обратно; добитый уходит в павшие, а Таран в него сгорает
    * (см. `GroupState.withoutSlot`). Пустая строка от `hit` в лог не идёт.
    * Бросков нет — чистая функция. */
  private def strikeOthers(battle: SoloPveBattle, positions: Seq[Int])(
      hit: SoloPveBattle => (SoloPveBattle, String)
  ): (SoloPveBattle, Vector[String]) = {
    val near = positions.toList.distinct.sorted
      .filter(pos => pos != battle.group.activePos && battle.monsterAt(pos).exists(_.alive))
    near.foldLeft((battle, Vector.empty[String])) { case ((b, log), pos) =>
      val idx         = b.group.idxOf(pos)
      val (tmp, line) = hit(b.withActive(b.group.others(idx)))
      val placed      = b.copy(group = b.group.copy(others = b.group.others.updated(idx, tmp.activeSlot)))
      val lines       = if (line.isEmpty) log else log :+ line
      if (tmp.monsterCurrentHp > 0L) (placed, lines)
      else (placed.sideFallen(idx), lines :+ content.format("battle.group.sideSlain", "monster" -> tmp.monsterName))
    }
  }

  /** Урон-«отголосок» умения по мобу во временной паре: броня, потом HP — без
    * защиты, стихий и проков (это отголосок удара, а не удар). */
  private def plainHit(tmp: SoloPveBattle, amount: Long): SoloPveBattle = {
    val armorDmg = math.min(tmp.monsterCurrentArmor, amount)
    tmp.copy(monsterCurrentArmor = tmp.monsterCurrentArmor - armorDmg,
             monsterCurrentHp    = (tmp.monsterCurrentHp - (amount - armorDmg)).max(0L))
  }

  /** «Отбросить»: активный моб отлетает на последнее занятое место строя, а тот,
    * кто там стоял, выходит на его место — и в поля. Активный и так последний
    * (или один) — None. Возвращает бой и имя вышедшего. */
  private def knockBack(battle: SoloPveBattle): Option[(SoloPveBattle, String)] = {
    val g    = battle.group
    val last = (g.activePos :: g.places).max
    if (last == g.activePos) None
    else {
      val idx   = g.idxOf(last)
      val from  = g.activePos
      val moved = battle.copy(group = g.copy(places = g.places.updated(idx, from), activePos = last))
      Some((moved.engage(from), g.others(idx).name))
    }
  }

  /** Восстановление брони героя на `value` (кап — эффективный максимум). Возвращает
    * новые статы и фактически восстановленную величину. */
  private def repairArmor(hero: Hero, value: Long, nowMs: Long): (FightStats, Long) = {
    val maxArmor = hero.effectiveMaxArmor(nowMs)
    val newArmor = (hero.fightStats.armor + value).min(maxArmor)
    (hero.fightStats.copy(armor = newArmor), newArmor - hero.fightStats.armor)
  }

  // ── Фляга и пояс (расходники, ход монстра не провоцируют) ────────────────────

  /** Бросок «сработала ли пассивка» с шансом `pct`% — но ТОЛЬКО если она активна
    * (`active`). Если пассивки нет, случайное число не тратится (важно для
    * детерминированных боевых тестов): сразу `false`. */
  private def chanceRoll(active: Boolean, pct: Long): Task[Boolean] =
    if (!active) ZIO.succeed(false)
    else Random.nextIntBetween(1, 101).map(_ <= pct)

  /** Пассивка «Быстрые руки»: с 25% шансом расходник не тратит раунд (можно применить
    * ещё раз). Возвращает флаг «раунд потрачен» (в `consumableUsedThisRound`) и
    * приписку в лог, если прокнуло. */
  private def quickHandsRoll(hero: Hero): Task[(Boolean, Vector[String])] =
    chanceRoll(hero.passives.hasQuickHands, PassiveKind.QuickHands.RepeatChancePct).map { repeated =>
      if (repeated) (false, Vector(content.text("battle.quickHands")))
      else (true, Vector.empty)
    }

  private def flaskTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] = {
    val flask = hero.equipment.flask
    flask.details match {
      case f: ItemDetails.Flask if f.charges <= 0 =>
        cont(hero, battle, content.text("battle.flaskEmpty"))
      case _: ItemDetails.Flask if battle.consumableUsedThisRound =>
        cont(hero, battle, content.text("battle.consumableAlreadyUsed"))
      case f: ItemDetails.Flask =>
        quickHandsRoll(hero).map { case (consumed, qhLog) =>
          val newEquipment  = hero.equipment.copy(flask = flask.copy(details = f.spent))
          val usedBattle    = battle.copy(consumableUsedThisRound = consumed)
          f.effect match {
            case FlaskEffect.HealPercent(pct) =>
              val maxHp    = hero.effectiveMaxHp(nowMs)
              // «Целитель» множит лечение фляги на 1.1, дальше — общий расчёт.
              val fullHeal = (maxHp * pct / 100L * hero.passives.healMult).toLong.max(1L)
              val (newHp, healed, healedBattle, cutLine) = activeHeal(hero, battle, fullHeal, nowMs)
              // Глоток фляги дополнительно восстанавливает 5% максимума Энергии.
              val maxEn      = hero.maxEnergy(nowMs)
              val newEnergy  = (hero.fightStats.energy + (maxEn * 5 / 100L).max(1L)).min(maxEn)
              val energyBack = newEnergy - hero.fightStats.energy
              val newStats   = hero.fightStats.copy(hp = newHp, energy = newEnergy)
              val msg = content.format(
                "battle.flaskUsed",
                "healed" -> healed.toString,
                "hp"     -> newHp.toString,
                "max"    -> maxHp.toString,
                "energy" -> energyBack.toString
              )
              val lines = (Vector(msg) ++ cutLine) ++ qhLog
              // Бой берём из расчёта лечения — в нём уже потраченное горение.
              TurnResult(hero.copy(equipment = newEquipment, fightStats = newStats),
                healedBattle.copy(consumableUsedThisRound = consumed), lines, Outcome.Continue, endsRound = false)
            case FlaskEffect.AddBuff(buff, rounds) =>
              val timedBuff = buff.copy(turnsLeft = Some(rounds))
              val newBattle = usedBattle.copy(heroBattleState = usedBattle.heroBattleState.add(timedBuff))
              TurnResult(hero.copy(equipment = newEquipment), newBattle, Vector(content.text("battle.flaskBuff")) ++ qhLog,
                Outcome.Continue, endsRound = false)

            // Фляга кузнеца: чинит броню до потолка.
            case FlaskEffect.ArmorPercent(pct) =>
              val maxArmor = hero.effectiveMaxArmor(nowMs)
              val newArmor = (hero.fightStats.armor + (maxArmor * pct / 100L).max(1L)).min(maxArmor).max(hero.fightStats.armor)
              val msg = content.format("battle.flaskArmor",
                "armor" -> (newArmor - hero.fightStats.armor).toString, "cur" -> newArmor.toString, "max" -> maxArmor.toString)
              TurnResult(hero.copy(equipment = newEquipment, fightStats = hero.fightStats.copy(armor = newArmor)),
                usedBattle, Vector(msg) ++ qhLog, Outcome.Continue, endsRound = false)

            // Фляга бодрости: энергия до потолка.
            case FlaskEffect.EnergyPercent(pct) =>
              val maxEn = hero.maxEnergy(nowMs)
              val newEn = (hero.fightStats.energy + (maxEn * pct / 100L).max(1L)).min(maxEn).max(hero.fightStats.energy)
              val msg = content.format("battle.flaskEnergy",
                "energy" -> (newEn - hero.fightStats.energy).toString, "cur" -> newEn.toString, "max" -> maxEn.toString)
              TurnResult(hero.copy(equipment = newEquipment, fightStats = hero.fightStats.copy(energy = newEn)),
                usedBattle, Vector(msg) ++ qhLog, Outcome.Continue, endsRound = false)

            // Стихийная фляга: урона нет, прок стихии — гарантирован. Иммунитеты
            // цели в силе: огненного элементаля пламя не берёт, элементалю и
            // нежити прок ложится как обычный (см. withEffects).
            case FlaskEffect.Splash(element) =>
              val (procced, procLog) = applyProcs(usedBattle, Set(element))
              val guarded = procced.withEffects(procced.effects)
              val head = content.format("battle.flaskSplash", "flask" -> flask.name)
              val lines =
                if (element == Element.Fire && guarded.boss.exists(_.immuneToBurn))
                  Vector(head, content.text("battle.flaskSplashImmune"))
                else Vector(head) ++ procLog
              TurnResult(hero.copy(equipment = newEquipment), guarded, lines ++ qhLog, Outcome.Continue, endsRound = false)

            // Фляга очищения: всё вредное с героя долой — раны, дебафы, срез защиты.
            case FlaskEffect.Cleanse =>
              val cleaned = usedBattle.effects.copy(
                heroBurn = None, heroPoison = None, heroBleed = None,
                heroStunnedTurns = 0, heroGroundedTurns = 0, heroColdDefenceCut = 0)
              TurnResult(hero.copy(equipment = newEquipment), usedBattle.copy(effects = cleaned),
                Vector(content.text("battle.flaskCleanse")) ++ qhLog, Outcome.Continue, endsRound = false)

            // Дымная фляга: мобы вне пары на несколько раундов теряют из виду и
            // героя, и друг друга. Запас в один тик — см. BattleEffects.heroSmokeTurns.
            case FlaskEffect.Smoke(rounds) =>
              TurnResult(hero.copy(equipment = newEquipment),
                usedBattle.copy(effects = usedBattle.effects.copy(heroSmokeTurns = rounds + 1)),
                Vector(content.format("battle.flaskSmoke", "rounds" -> rounds.toString)) ++ qhLog,
                Outcome.Continue, endsRound = false)

            // Вампирская фляга: следующие удары по HP лечат (см. playerStrike).
            case FlaskEffect.Vampiric(hits, pct) =>
              TurnResult(hero.copy(equipment = newEquipment),
                usedBattle.copy(effects = usedBattle.effects.copy(heroVampiricHits = hits)),
                Vector(content.format("battle.flaskVampiric", "hits" -> hits.toString, "pct" -> pct.toString)) ++ qhLog,
                Outcome.Continue, endsRound = false)

            // Фляги яда и крови: оружие смазано на несколько раундов (см. playerStrike).
            case FlaskEffect.PoisonCoat(rounds) =>
              TurnResult(hero.copy(equipment = newEquipment),
                usedBattle.copy(effects = usedBattle.effects.copy(heroPoisonCoatTurns = rounds)),
                Vector(content.format("battle.flaskPoisonCoat", "rounds" -> rounds.toString)) ++ qhLog,
                Outcome.Continue, endsRound = false)
            case FlaskEffect.BleedCoat(rounds) =>
              TurnResult(hero.copy(equipment = newEquipment),
                usedBattle.copy(effects = usedBattle.effects.copy(heroBleedCoatTurns = rounds)),
                Vector(content.format("battle.flaskBleedCoat", "rounds" -> rounds.toString)) ++ qhLog,
                Outcome.Continue, endsRound = false)
          }
        }
      case _ =>
        cont(hero, battle, content.text("battle.noFlask"))
    }
  }

  /** Применение зелья пояса. Зеркалит [[flaskTurn]]: проверки пусто/уже-пили, трата
    * заряда, затем эффект по типу зелья ([[applyPotion]]). */
  private def beltTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] = {
    val belt = hero.equipment.belt
    belt.details match {
      case b: ItemDetails.Belt if b.charges <= 0 =>
        cont(hero, battle, content.text("battle.beltEmpty"))
      case _: ItemDetails.Belt if battle.consumableUsedThisRound =>
        cont(hero, battle, content.text("battle.consumableAlreadyUsed"))
      case b: ItemDetails.Belt =>
        quickHandsRoll(hero).map { case (consumed, qhLog) =>
          val equipment = hero.equipment.copy(belt = belt.copy(details = b.spent))
          val battle1   = battle.copy(consumableUsedThisRound = consumed)
          val (newStats, newBattle, msg) = applyPotion(hero, battle1, b.potion, nowMs)
          TurnResult(hero.copy(equipment = equipment, fightStats = newStats), newBattle, Vector(msg) ++ qhLog,
            Outcome.Continue, endsRound = false)
        }
      case _ =>
        cont(hero, battle, content.text("battle.noBelt"))
    }
  }

  /** Эффект конкретного зелья пояса. Мгновенные (лечение/металл/энергия) меняют
    * статы героя; временные (атака/защита/уворот) кладут баф на 5 ходов;
    * яд/реген ставят соответствующий статус-эффект. Возвращает новые статы героя,
    * обновлённый бой и строку эффекта — запись выполняет вызывающий (commit). */
  private def applyPotion(
      hero: Hero,
      battle: SoloPveBattle,
      potion: PotionKind,
      nowMs: Long
  ): (FightStats, SoloPveBattle, String) = {
    val buffed = battle.heroBattleState.applyTo(hero.effectiveFightStats(nowMs))
    potion match {
      case PotionKind.Healing =>
        val maxHp = hero.effectiveMaxHp(nowMs)
        // «Целитель» множит лечение зелья на 1.1, дальше — общий расчёт.
        val full  = (maxHp * 25 / 100L * hero.passives.healMult).toLong.max(1L)
        val (newHp, healed, healedBattle, cutLine) = activeHeal(hero, battle, full, nowMs)
        val line  = content.format("battle.beltHeal",
                      "healed" -> healed.toString, "hp" -> newHp.toString, "max" -> maxHp.toString)
        (hero.fightStats.copy(hp = newHp), healedBattle, (line +: cutLine.toVector).mkString("\n"))

      case PotionKind.Metal =>
        val maxArmor = hero.effectiveMaxArmor(nowMs)
        val restore  = (maxArmor * 20 / 100L).max(1L)
        val newArmor = (hero.fightStats.armor + restore).min(maxArmor)
        val gained   = newArmor - hero.fightStats.armor
        (hero.fightStats.copy(armor = newArmor), battle,
          content.format("battle.beltMetal", "armor" -> gained.toString, "cur" -> newArmor.toString, "max" -> maxArmor.toString))

      case PotionKind.Poison =>
        (hero.fightStats, battle.copy(effects = battle.effects.copy(heroPoisonousAttacks = true)),
          content.text("battle.beltPoison"))

      case PotionKind.Regeneration =>
        (hero.fightStats, battle.copy(effects = battle.effects.copy(heroRegen = Some(Regen.onDrink))),
          content.format("battle.beltRegen", "pct" -> Regen.OnDrink.toString))

      case PotionKind.Evasion =>
        (hero.fightStats,
          battle.copy(heroBattleState = battle.heroBattleState.add(
            Buff(0L, 0L, 0L, dodgePct = 10L, defencePct = 0L, turnsLeft = Some(ItemDetails.Belt.BuffRounds)))),
          content.format("battle.beltEvasion", "rounds" -> ItemDetails.Belt.BuffRounds.toString))

      case PotionKind.Energy =>
        val maxEn     = hero.maxEnergy(nowMs)
        val restore   = (maxEn * 25 / 100L).max(1L)
        val newEnergy = (hero.fightStats.energy + restore).min(maxEn)
        val gained    = newEnergy - hero.fightStats.energy
        (hero.fightStats.copy(energy = newEnergy), battle,
          content.format("battle.beltEnergy", "energy" -> gained.toString, "cur" -> newEnergy.toString, "max" -> maxEn.toString))

      case PotionKind.Attack =>
        val bonus = (buffed.atk * 5 / 100L).max(1L)
        (hero.fightStats,
          battle.copy(heroBattleState = battle.heroBattleState.add(
            Buff(atk = bonus, 0L, 0L, dodgePct = 0L, defencePct = 0L, turnsLeft = Some(ItemDetails.Belt.BuffRounds)))),
          content.format("battle.beltAttack", "atk" -> bonus.toString, "rounds" -> ItemDetails.Belt.BuffRounds.toString))

      case PotionKind.Defence =>
        val bonus = (buffed.defence * 10 / 100L).max(1L)
        (hero.fightStats,
          battle.copy(heroBattleState = battle.heroBattleState.add(
            Buff(0L, 0L, defence = bonus, dodgePct = 0L, defencePct = 0L, turnsLeft = Some(ItemDetails.Belt.BuffRounds)))),
          content.format("battle.beltDefence", "defence" -> bonus.toString, "rounds" -> ItemDetails.Belt.BuffRounds.toString))
    }
  }

  // ── Бегство ─────────────────────────────────────────────────────────────────

  /** Экран подтверждения бегства (чистая навигация, состояние не меняется). */
  private def flee(user: User, renderer: Renderer): Task[StateType] =
    renderer
      .show(user, content.screen("battle.fleeConfirm"))
      .as(StateType.Battle)

  /** Подтверждённое бегство: моб делает один удар (без каста/тиков/регена, как и
    * прежде). Умер герой — Death; иначе — Fled (бой очистит commit). */
  private def fleeTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    for {
      buffedEff <- ZIO.succeed(effWithAir(hero, battle, nowMs))
      monster   = battle.toMonster
      // Мобы вне пары могут не выпустить: по 5% за каждого. Окружили — бой
      // продолжается, как будто бегства и не было.
      surroundPct = GroupState.SurroundPctPerFreeMob * battle.group.others.size
      surrounded <- chanceRoll(surroundPct > 0L, surroundPct)
      hitRoll <- Random.nextIntBetween(1, 101)
      // «Сверкающий» дают +25% к уклонению именно при бегстве.
      result <-
        if (surrounded)
          ZIO.succeed(TurnResult(hero, battle,
            Vector(content.format("battle.group.surround", "race" -> monster.race.genitivePlural)), Outcome.Continue))
        // Напротив никого — бить в спину некому.
        else if (battle.unpaired) ZIO.succeed(TurnResult(hero, battle, Vector.empty, Outcome.Fled))
        else if (hitRoll > playerDodgeChance(hero, battle, nowMs, fleeing = true)) {
          for {
            spread <- Random.nextLongBetween(80L, 121L)
            rawDamage = (monster.fightStats.atk * spread / 100L).max(1L)
            reduction = heroDamageReduction(hero, battle, buffedEff, monster.fightStats.atk, nowMs)
            baseReduced = (rawDamage * (1.0 - reduction)).toLong.max(1L)
            // «Непробиваемый»: 20% шанс срезать урон обычной атаки вдвое.
            impenTriggered <- chanceRoll(hero.passives.hasImpenetrable, PassiveKind.Impenetrable.TriggerPct)
            reducedDamage =
              if (impenTriggered) (baseReduced * (100L - PassiveKind.Impenetrable.ReductionPct) / 100L).max(1L)
              else baseReduced
            // Удар в спину при бегстве разбирается тем же расчётом, что и обычная
            // атака в бою: та же баффовая броня, тот же порог 6 «Каменного стража».
            parting     = bossDamageTaken(hero, battle, reducedDamage)
            (newHp, newArmor) = MonsterSkill.applyPhysicalDamage(battle, hero, parting)
            hero1       = hero.copy(fightStats = hero.fightStats.copy(hp = newHp, armor = newArmor))
            partTrauma <- hitTrauma(hero, hero1, nowMs)
            (hero2, traumaLine) = partTrauma
            mobLine     = content.format("battle.mobHit", "damage" -> parting.toString, "monster" -> battle.monsterName)
            lines       = Vector(mobLine) ++ traumaLine.toVector
          } yield
            if (newHp <= 0) TurnResult(hero2, battle, lines, Outcome.Death)
            else TurnResult(hero2, battle, lines, Outcome.Fled)
        } else
          ZIO.succeed(TurnResult(hero, battle, Vector.empty, Outcome.Fled))
    } yield result

  // ── Группа ──────────────────────────────────────────────────────────────────

  /** Активный моб пал, а в группе есть ещё: в паре встаёт следующий по номеру,
    * бой продолжается. Павший ждёт выдачи добычи в `slain`. Новичок в этом
    * раунде уже не бьёт — он только что шагнул вперёд. */
  private def promoteAfterKill(res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Victory) res
    else res.battle.promoteNext match {
      case None       => res
      case Some(next) =>
        val fallen = res.battle.monsterName
        val line =
          if (next.group.paired) content.format("battle.group.fell", "monster" -> fallen, "next" -> next.monsterName)
          else content.format("battle.group.fellNoStep", "monster" -> fallen)
        res.copy(battle = next, outcome = Outcome.Continue, log = res.log :+ line)
    }

  // ── Союзники ────────────────────────────────────────────────────────────────

  /** Союзники ходят после ответа активного моба, по позициям. Каждый бьёт врага
    * напротив, а если там пусто — ближайшего в досягаемости (сперва моба в паре
    * с героем), стихией своего оружия; сверху — случайное умение из тех, на что
    * хватает энергии и что к месту. Добитый уходит в павшие — и это засчитывается
    * герою. Энергия копится в конце хода. */
  private def allyPhase(res: TurnResult): Task[TurnResult] =
    if (res.outcome != Outcome.Continue || !res.endsRound || res.battle.group.allies.isEmpty) ZIO.succeed(res)
    else
      ZIO.foldLeft(res.battle.group.allies.sortBy(_.position))((res.battle, Vector.empty[String])) {
        case ((b, log), a0) =>
          b.group.allies.find(_.kind == a0.kind).filter(_.alive) match {
            case None    => ZIO.succeed((b, log))
            case Some(a) => allyTurn(a, b).map { case (b2, lines) => (b2, log ++ lines) }
          }
      }.map { case (b, log) =>
        val outcome = if (b.monsterCurrentHp <= 0L) Outcome.Victory else Outcome.Continue
        res.copy(battle = b, outcome = outcome, log = res.log ++ log)
      }

  /** Кого бьёт союзник: напротив, затем моб в паре с героем, затем сосед. */
  private def allyTarget(a: BattleAlly, battle: SoloPveBattle): Option[Int] =
    List(a.position, battle.group.heroPos, a.position - 1, a.position + 1).distinct
      .filter(p => math.abs(p - a.position) <= GroupState.Reach)
      .find(p => battle.monsterAt(p).exists(_.alive))

  private def allyTurn(a: BattleAlly, battle: SoloPveBattle): Task[(SoloPveBattle, Vector[String])] =
    for {
      // 1) обычная атака по цели
      struck <- allyTarget(a, battle) match {
        case None      => ZIO.succeed(AllyBlow(battle, None, 0L, slew = false))
        case Some(pos) => allyHits(a, battle, pos, a.stats.atk, crushing = false)
      }
      b1 = struck.battle
      // 2) случайное умение из тех, что по карману и к месту
      a1 = b1.group.allies.find(_.kind == a.kind).getOrElse(a)
      target2 = allyTarget(a1, b1)
      options = AllySkill.values.toList.filter { s =>
        s.cost(a1.lvl) <= a1.energy && (s match {
          case AllySkill.QuickStrike | AllySkill.CrushingStrike => target2.isDefined
          case AllySkill.HealingFlask    => a1.hp < a1.stats.hp
          case AllySkill.EmergencyRepair => a1.armor < a1.stats.armor
        })
      }
      casted <-
        if (options.isEmpty) ZIO.succeed((AllyBlow(b1, None, 0L, slew = false), Vector.empty[String]))
        else Random.nextIntBetween(0, options.size).flatMap { i =>
          val skill = options(i)
          val paid  = b1.copy(group = b1.group.updateAlly(a1.kind)(x => x.copy(energy = x.energy - skill.cost(a1.lvl))))
          skill match {
            case AllySkill.QuickStrike =>
              allyHits(a1, paid, target2.get, (a1.stats.atk * AllySkill.QuickStrikeFactor).toLong.max(1L),
                crushing = false, sure = true).map(_ -> Vector.empty[String])
            case AllySkill.CrushingStrike =>
              allyHits(a1, paid, target2.get, (a1.stats.atk * AllySkill.CrushingStrikeFactor).toLong.max(1L),
                crushing = true, sure = true).map(_ -> Vector.empty[String])
            case AllySkill.HealingFlask =>
              val heal = (a1.stats.hp * AllySkill.HealPct / 100L).max(1L)
              val newHp = (a1.hp + heal).min(a1.stats.hp)
              ZIO.succeed((AllyBlow(paid.copy(group = paid.group.updateAlly(a1.kind)(_.copy(hp = newHp))), None, 0L, slew = false),
                Vector(content.format("battle.ally.healingFlask", "name" -> a1.name, "hp" -> (newHp - a1.hp).toString))))
            case AllySkill.EmergencyRepair =>
              val fix = (a1.stats.armor * AllySkill.RepairPct / 100L).max(1L)
              val newArmor = (a1.armor + fix).min(a1.stats.armor)
              ZIO.succeed((AllyBlow(paid.copy(group = paid.group.updateAlly(a1.kind)(_.copy(armor = newArmor))), None, 0L, slew = false),
                Vector(content.format("battle.ally.emergencyRepair", "name" -> a1.name, "armor" -> (newArmor - a1.armor).toString))))
          }
        }
      (blow2, aidLog) = casted
      b2 = blow2.battle
      // 3) энергия копится
      b3 = b2.copy(group = b2.group.updateAlly(a.kind)(x =>
             x.copy(energy = (x.energy + x.kind.energyRegen(x.lvl)).min(x.stats.energy))))
      // Лог — одной строкой на цель, с суммарным уроном удара и умения; проки и
      // раны считаются, но экран не засоряют. Промах — только если не попало ничем.
      blows   = List(struck, blow2).filter(_.target.isDefined)
      byName  = blows.groupBy(_.target.get).view.mapValues(_.map(_.damage).sum).toMap
      order   = blows.map(_.target.get).distinct
      hitLog  = order.flatMap { m =>
        val dmg = byName(m)
        if (dmg > 0L) Some(content.format("battle.ally.hit", "name" -> a.name, "monster" -> m, "damage" -> dmg.toString))
        else Some(content.format("battle.ally.miss", "name" -> a.name, "monster" -> m))
      }.toVector
      slewLog = blows.filter(_.slew).map(b => content.format("battle.ally.slew", "name" -> a.name, "monster" -> b.target.get)).toVector
    } yield (b3, hitLog ++ aidLog ++ slewLog)

  /** Удар союзника по мобу на месте `pos`: бросок на попадание против уклонения
    * моба, разброс ±20%, защита моба (кроме дробящего — тот идёт прямо в HP),
    * сопротивление минибосса стихии, грани стихии по броне и HP, прок стихии
    * 30%. Моб вне пары правится через временную пару. Добитый — в павшие. */
  private def allyHits(
      a: BattleAlly, battle: SoloPveBattle, pos: Int, raw: Long, crushing: Boolean, sure: Boolean = false
  ): Task[AllyBlow] = {
    val active = pos == battle.group.activePos
    val idx    = battle.group.idxOf(pos)
    val tmp    = if (active) battle else battle.withActive(battle.group.others(idx))
    val elem   = a.kind.element
    for {
      // Умения (`sure`) не промахиваются — как и у героя; бросок не тратится.
      hitRoll <- if (sure) ZIO.succeed(101) else Random.nextIntBetween(1, 101)
      dodge    = mobDodgeChance(a.stats.accuracy, tmp)
      out <-
        if (hitRoll <= dodge) ZIO.succeed(AllyBlow(battle, Some(tmp.monsterName), 0L, slew = false))
        else for {
          spread <- Random.nextLongBetween(80L, 121L)
          base    = (raw * spread / 100L).max(1L)
          resist  = tmp.boss.map(_.damageTakenMult(elem)).getOrElse(1.0)
          dmg     = (base * resist).toLong.max(1L)
          (armorDmg, hpDmg) = if (crushing) (0L, dmg) else {
            val cut       = (dmg * (1.0 - BattleState.defenceReduction(effectiveMonsterDefence(tmp), 0L, a.stats.atk))).toLong.max(1L)
            val rawArmor  = math.min(tmp.monsterCurrentArmor, cut)
            val rawHp     = cut - rawArmor
            val aDmg      = math.min(tmp.monsterCurrentArmor, (rawArmor * elem.armorMult).toLong).max(0L)
            val lightning = if (elem == Element.Lightning) (aDmg * Element.Lightning.ArmorToHpFrac).toLong else 0L
            (aDmg, ((rawHp * elem.hpMult).toLong + lightning).max(0L))
          }
          newHp    = (tmp.monsterCurrentHp - hpDmg).max(0L)
          hit      = tmp.copy(monsterCurrentHp = newHp, monsterCurrentArmor = tmp.monsterCurrentArmor - armorDmg)
          dealt    = armorDmg + hpDmg
          // Прок стихии считается молча — строки его в лог союзника не идут.
          procRoll <- if (newHp > 0L) Random.nextIntBetween(1, 101) else ZIO.succeed(100)
          procced  = if (newHp > 0L && procRoll <= Element.ProcChancePct) applyProcs(hit, Set(elem))._1 else hit
          guarded  = procced.withEffects(procced.effects)
          // обратно: активный — как есть, слот — в строй; добитый слот — в павшие
          result =
            if (active) AllyBlow(guarded, Some(tmp.monsterName), dealt, slew = false)
            else {
              val restored = battle.copy(group = battle.group.copy(others = battle.group.others.updated(idx, guarded.activeSlot)))
              if (guarded.monsterCurrentHp <= 0L) AllyBlow(restored.sideFallen(idx), Some(tmp.monsterName), dealt, slew = true)
              else AllyBlow(restored, Some(tmp.monsterName), dealt, slew = false)
            }
        } yield result
    } yield out
  }

  /** Моб вне пары бьёт союзника напротив: бросок против уклонения союзника, урон
    * с разбросом, защита союзника, стихия удара моба по граням, броня — потом HP.
    * Обнулённый союзник уходит по свитку. Умений против союзников мобы не
    * применяют — только удар. */
  private def mobStrikesAlly(a: BattleAlly, tmp: SoloPveBattle): Task[(SoloPveBattle, Vector[String])] =
    for {
      hitRoll <- Random.nextIntBetween(1, 101)
      dodge    = BattleState.dodgeChance(0L, a.stats.evasion, a.stats.defence, tmp.monsterStats.accuracy)
      out <-
        if (hitRoll <= dodge) ZIO.succeed((tmp, Vector(content.format("battle.ally.mobMiss", "monster" -> tmp.monsterName, "name" -> a.name))))
        else for {
          spread <- Random.nextLongBetween(80L, 121L)
          raw      = (monsterAttack(tmp) * spread / 100L).max(1L)
          reduced  = (raw * (1.0 - BattleState.damageReduction(a.stats.defence, 0L, tmp.monsterStats.atk, 0L))).toLong.max(1L)
          (armorMult, hpMult) = powderSplit(tmp).getOrElse((1.0, 1.0))
          rawArmor = math.min(a.armor, reduced)
          armorDmg = (rawArmor * armorMult).toLong.min(a.armor).max(0L)
          hpDmg    = ((reduced - rawArmor) * hpMult).toLong.max(0L)
          hurt     = a.copy(armor = a.armor - armorDmg, hp = (a.hp - hpDmg).max(0L))
          line     = content.format("battle.ally.mobHit", "monster" -> tmp.monsterName, "name" -> a.name, "damage" -> (armorDmg + hpDmg).toString)
          result   =
            if (hurt.alive) (tmp.copy(group = tmp.group.updateAlly(a.kind)(_ => hurt)), Vector(line))
            else (tmp.copy(group = tmp.group.withoutAlly(a.kind)),
                  Vector(line, content.format("battle.ally.scroll", "name" -> a.name)))
        } yield result
    } yield out

  /** Отряд после боя: состояние союзников из боя, ушедшие по свитку — в отлучку. */
  private def squadAfterBattle(hero: Hero, battle: SoloPveBattle, nowMs: Long): pangea.model.squad.Squad = {
    // Позиции могли поменяться за бой (Таран, «Переместиться») — вместе с героем.
    val synced = battle.group.allies.foldLeft(hero.squad.copy(heroPos = battle.group.heroPos)) { (s, a) =>
      s.update(a.kind)(old => a.toAlly.copy(hiredUntil = old.hiredUntil))
    }
    battle.group.alliesGone.flatMap(AllyKind.withNameOption).foldLeft(synced)((s, k) => s.sentAway(k, nowMs))
  }

  /** Ход мобов вне пары. Сосед героя (номер 2) достаёт его сбоку: обычная
    * атака, а поверх — умение, как у моба в паре; лечащее умение он отдаёт
    * раненому соседу, если сам цел. Мобы дальше по строю героя не достают —
    * копят энергию и лечат соседей. Раны на них (яд, кровь, огонь) тикают как у
    * всех; кого добило — уходит в павшие. */
  private def sideMobsPhase(res: TurnResult, nowMs: Long): Task[TurnResult] =
    if (res.outcome != Outcome.Continue || !(res.battle.isGroup || res.battle.unpaired || res.battle.group.heroDown) || !res.endsRound) ZIO.succeed(res)
    else {
      val n = res.battle.group.others.size
      val smokeLine = if (res.battle.effects.heroInSmoke) Vector(content.text("battle.flaskSmokeHolds")) else Vector.empty[String]
      // Павший выше по строю смыкает ряды: следующий моб стоит уже на его индексе.
      ZIO.foldLeft((0 until n).toList)((res.hero, res.battle, Vector.empty[String], 0)) {
        case ((h, b, log, fallen), i) =>
          val idx = i - fallen
          // Герой пал прямо сейчас — мобы замирают; лежит с прошлого раунда — бьют отряд.
          if ((h.fightStats.hp <= 0 && !b.group.heroDown) || idx >= b.group.others.size) ZIO.succeed((h, b, log, fallen))
          else freeMobTurn(h, b, idx, nowMs).map { case (h2, b2, lines) =>
            (h2, b2, log ++ lines, fallen + (b.group.others.size - b2.group.others.size))
          }
      }.flatMap { case (h, b, log, _) =>
        // Активный вне пары ходит последним — как моб вне пары; при лежащем
        // герое так же ходит и тот, что в паре: отвечать ему некому.
        if (b.group.paired && !b.group.heroDown) ZIO.succeed((h, b, log))
        else if (h.fightStats.hp <= 0 && !b.group.heroDown) ZIO.succeed((h, b, log))
        else unpairedActiveTurn(h, b, nowMs).map { case (h2, b2, lines) => (h2, b2, log ++ lines) }
      }.map { case (h, b, log) =>
        val outcome =
          if (h.fightStats.hp <= 0 && !b.group.heroDown) Outcome.Death
          else if ((b.unpaired || b.group.heroDown) && b.monsterCurrentHp <= 0) Outcome.Victory   // истёк ранами — дальше промоут
          else Outcome.Continue
        res.copy(hero = h, battle = b, outcome = outcome, sideLog = res.sideLog ++ smokeLine ++ log)
      }
    }

  /** Один моб вне пары, `idx` — его индекс в `others`; достаёт героя, если его
    * место — соседнее. */
  private def freeMobTurn(hero: Hero, battle: SoloPveBattle, idx: Int, nowMs: Long): Task[(Hero, SoloPveBattle, Vector[String])] = {
    val slot = battle.group.others(idx)
    val pos  = battle.group.posOf(idx)
    // Временный бой: этот моб — «активный», геройская половина эффектов та же;
    // его временные эффекты тикают здесь (общий tickBuffs крутит только пару).
    val tmp0 = battle.withActive(slot)
    val tmp  = tmp0.copy(effects = tickSlotEffects(tmp0.effects))
    freeTurnOf(hero, battle, tmp, pos, nowMs).map { case (h2, b2, ticked, log3) =>
      // Обратно в строй — или в павшие, если раны добили. Союзники правились во
      // временном бою (удар по стоящему напротив) — забираем их оттуда.
      val afterSlot = ticked.activeSlot
      val merged = b2.copy(effects = ticked.effects.withMonsterPart(b2.effects),
                           group = b2.group.copy(allies = ticked.group.allies, alliesGone = ticked.group.alliesGone))
      val placed = merged.copy(group = merged.group.copy(others = merged.group.others.updated(idx, afterSlot)))
      if (afterSlot.alive) (h2, placed, log3)
      else (h2, placed.sideFallen(idx), log3 :+ content.format("battle.group.sideFell", "monster" -> afterSlot.name))
    }
  }

  /** Активный моб, стоящий не напротив героя: ходит как моб вне пары, прямо в
    * полях (его временные эффекты уже оттикали в `tickBuffs`). Истёк ранами —
    * остаётся в полях с нулём HP, промоут дальше по цепочке. */
  private def unpairedActiveTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[(Hero, SoloPveBattle, Vector[String])] =
    freeTurnOf(hero, battle, battle, battle.group.activePos, nowMs).map { case (h2, b2, ticked, log3) =>
      (h2, ticked.copy(group = ticked.group.copy(others = b2.group.others)), log3)
    }

  /** Ход моба вне пары, развёрнутого в поля `tmp` на месте `pos`: удар по
    * союзнику напротив либо сбоку по герою (если достаёт), умение, энергия,
    * тик ран. Возвращает героя, `battle` с возможным лечением соседа и `tmp`
    * после хода; собрать это обратно — дело вызывающего. */
  private def freeTurnOf(
      hero: Hero, battle: SoloPveBattle, tmp: SoloPveBattle, pos: Int, nowMs: Long
  ): Task[(Hero, SoloPveBattle, SoloPveBattle, Vector[String])] = {
    // В дыму (дымная фляга) мобы вне пары не видят ни героя, ни друг друга:
    // ни удара сбоку, ни лечения соседу — только раны тикают и энергия копится.
    val smoke       = battle.effects.heroInSmoke
    // Напротив стоит союзник — моб занят им и до героя не тянется.
    val facing      = if (smoke) None else battle.group.allyAt(pos).filter(_.alive)
    // Лежащего героя мобы не добивают — его смерть решится исходом боя.
    val reachesHero = !smoke && facing.isEmpty && battle.group.inReach(pos) && !battle.group.heroDown
    for {
      // 1) удар сбоку — только если достаёт; союзника напротив — вместо героя
      struck <-
        if (facing.isDefined) mobStrikesAlly(facing.get, tmp).map { case (t2, lines) => (hero, t2, lines) }
        else if (!reachesHero) ZIO.succeed((hero, tmp, Vector.empty[String]))
        else for {
          hitRoll <- Random.nextIntBetween(1, 101)
          dodge    = playerDodgeChance(hero, tmp, nowMs)
          out <-
            if (hitRoll > dodge)
              mobStrike(hero, tmp, effWithAir(hero, tmp, nowMs), nowMs).map { st =>
                val line = content.format("battle.group.sideHit", "monster" -> tmp.monsterName, "damage" -> st.damage.toString)
                (hero.copy(fightStats = hero.fightStats.copy(hp = st.newHp, armor = st.newArmor)), st.battle,
                  Vector(line) ++ st.extraLines)
              }
            else ZIO.succeed((hero, tmp, Vector(content.format("battle.group.sideMiss", "monster" -> tmp.monsterName))))
        } yield out
      (h0, t1, log0) = struck
      sideTrauma <- hitTrauma(hero, h0, nowMs)
      (h1, sideTraumaLine) = sideTrauma
      log1 = log0 ++ sideTraumaLine.toVector
      // 2) умение: раненому соседу — лечение, себе — что по карману, герою — урон
      //    (минибосс вне пары идёт по своему кругу)
      casted <-
        if (smoke || h1.fightStats.hp <= 0 || t1.monsterCurrentHp <= 0 || t1.effects.monsterSkillBlockedTurns > 0)
          ZIO.succeed((h1, t1, battle, Vector.empty[String]))
        else allyAid(h1, t1, battle, pos, nowMs).flatMap {
          case Some((t2, b2, line)) => ZIO.succeed((h1, t2, b2, Vector(line)))
          case None =>
            if (reachesHero) (if (t1.boss.isDefined) bossTurnCast(h1, t1, nowMs) else mobSkillCast(h1, t1, nowMs)).map {
              case (t2, h2, line) => (h2, t2, battle, if (line.isEmpty) Vector.empty else Vector(line))
            }
            else selfAid(h1, t1, nowMs).map { case (t2, line) => (h1, t2, battle, if (line.isEmpty) Vector.empty else Vector(line)) }
        }
      (h2raw, t2, b2, log2raw) = casted
      castSideTrauma <- hitTrauma(h1, h2raw, nowMs)
      (h2, castSideLine) = castSideTrauma
      log2 = log2raw ++ castSideLine.toVector
      // 3) конец хода этого моба: энергия и тик ран
      regen      = t2.boss.map(_.energyRegen(t2.monsterLvl)).getOrElse(MonsterEnergy.regen(t2.monsterLvl, t2.rarity))
      withEnergy = t2.copy(monsterCurrentEnergy = (t2.monsterCurrentEnergy + regen).min(t2.monsterStats.energy))
      (ticked, dotLine, _) = tickMonsterEffects(withEnergy, withEnergy.monsterName, h2.sets.burnGrowthMult)
      log3 = log1 ++ log2 ++ (if (dotLine.isEmpty) Vector.empty else Vector(dotLine))
    } yield (h2, b2, ticked, log3)
  }

  /** Временные эффекты на мобе вне пары тикают сами: общий `tickBuffs` крутит
    * только активную пару. */
  private def tickSlotEffects(e: BattleEffects): BattleEffects = e.copy(
    mobAirBoostTurns         = (e.mobAirBoostTurns - 1).max(0),
    monsterSkillBlockedTurns = (e.monsterSkillBlockedTurns - 1).max(0),
    monsterWeakenedTurns     = (e.monsterWeakenedTurns - 1).max(0),
    monsterDefenceDebuff     = e.monsterDefenceDebuff.flatMap(_.ticked)
  )

  /** Помощь соседу: если у моба по карману лечение или починка, сам он цел, а
    * сосед по строю (место ±1) ранен — умение уходит соседу, цена списывается
    * с лекаря. Сосед на месте героя — активный моб. */
  private def allyAid(
      hero: Hero, caster: SoloPveBattle, battle: SoloPveBattle, myPos: Int, nowMs: Long
  ): Task[Option[(SoloPveBattle, SoloPveBattle, String)]] = {
    val race       = Race.withName(caster.monsterRace)
    val affordable = MonsterSkill.values.filter(sk =>
      sk.availableTo(race) && sk.cost(caster.monsterLvl) <= caster.monsterCurrentEnergy)
    // Кандидаты: соседи по местам; тот, что в полях боя, — активный.
    val neighbours: List[Either[Unit, Int]] =
      List(myPos - 1, myPos + 1).flatMap { p =>
        if (p == battle.group.activePos) List(Left(()))
        else if (battle.group.occupied(p)) List(Right(battle.group.idxOf(p)))
        else Nil
      }
    def target(n: Either[Unit, Int]): SoloPveBattle = n match {
      case Left(_)  => battle
      case Right(i) => battle.withActive(battle.group.others(i))
    }
    val heals   = affordable.contains(MonsterSkill.HealingFlask) && !MonsterSkill.HealingFlask.applicable(caster)
    val repairs = affordable.contains(MonsterSkill.EmergencyRepair) && !MonsterSkill.EmergencyRepair.applicable(caster)
    val pick: Option[(MonsterSkill, Either[Unit, Int])] =
      (if (heals) neighbours.find(n => MonsterSkill.HealingFlask.applicable(target(n))).map(MonsterSkill.HealingFlask -> _) else None)
        .orElse(if (repairs) neighbours.find(n => MonsterSkill.EmergencyRepair.applicable(target(n))).map(MonsterSkill.EmergencyRepair -> _) else None)
    ZIO.succeed(pick.map { case (skill, n) =>
      val tgt    = target(n)
      val cast   = skill.cast(tgt, hero, nowMs)
      val paid   = caster.copy(monsterCurrentEnergy = (caster.monsterCurrentEnergy - skill.cost(caster.monsterLvl)).max(0L))
      val (line, healed) =
        if (skill == MonsterSkill.HealingFlask)
          (content.format("battle.group.healedAlly", "monster" -> caster.monsterName,
            "hp" -> (cast.battle.monsterCurrentHp - tgt.monsterCurrentHp).toString), cast.battle)
        else
          (content.format("battle.group.repairedAlly", "monster" -> caster.monsterName,
            "armor" -> (cast.battle.monsterCurrentArmor - tgt.monsterCurrentArmor).toString), cast.battle)
      val updated = n match {
        case Left(_)  => healed.copy(group = battle.group)                          // активный
        case Right(i) => battle.copy(group = battle.group.copy(others = battle.group.others.updated(i, healed.activeSlot)))
      }
      (paid, updated, line)
    })
  }

  /** Моб, который никого не достаёт, тратит энергию только на себя. */
  private def selfAid(hero: Hero, caster: SoloPveBattle, nowMs: Long): Task[(SoloPveBattle, String)] = {
    val race = Race.withName(caster.monsterRace)
    val own  = List(MonsterSkill.HealingFlask, MonsterSkill.EmergencyRepair).filter(sk =>
      sk.availableTo(race) && sk.applicable(caster) && sk.cost(caster.monsterLvl) <= caster.monsterCurrentEnergy)
    ZIO.succeed(own.headOption match {
      case None     => (caster, "")
      case Some(sk) =>
        val paid = caster.copy(monsterCurrentEnergy = (caster.monsterCurrentEnergy - sk.cost(caster.monsterLvl)).max(0L))
        val cast = sk.cast(paid, hero, nowMs)
        (cast.battle, cast.line)
    })
  }

  /** Конец раунда группы. Ход, который ходом не был (`turn` вернул бой без
    * изменений), сюда не доходит. Порядок: подкрепление → перемешивание каждый
    * четвёртый раунд → отложенный Таран, чтобы перемешивание его не съело. */
  private def endRound(before: SoloPveBattle, res: TurnResult): Task[TurnResult] =
    if (res.outcome != Outcome.Continue || !res.endsRound || res.battle == before || res.battle.boss.isDefined) ZIO.succeed(res)
    else {
      val b0 = res.battle.copy(group = res.battle.group.copy(round = res.battle.group.round + 1))
      for {
        // подкрепление: сородич первого моба, редкость как обычно (сюжетный бой — нет)
        comes <- chanceRoll(b0.group.aliveCount < GroupState.MaxMonsters && b0.story.isEmpty, GroupState.ReinforcementChancePct)
        withMore <-
          if (!comes) ZIO.succeed((b0, Vector.empty[String]))
          else for {
            seed  <- Random.nextLong
            race   = Race.withName(b0.reinforcementRace)
            (m, _) = MonsterGenerator.generateOfRace(res.hero.dungeonLevel, race, Rng(seed))
            pct   <- Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
            slot   = MonsterSlot(m.lvl, m.race.entryName, m.rarity.entryName, m.fightStats, m.fightStats.hp,
                       m.fightStats.armor, m.marked, MonsterEnergy.startEnergy(m.lvl, m.rarity, pct), BattleEffects.empty)
            more   = b0.withReinforcement(slot)
            joined = more.copy(group = more.group.copy(originRace = Some(b0.reinforcementRace)))
          } yield (joined, Vector(content.text("battle.group.reinforcement")))
        (b1a, log1a) = withMore
        // призыв: в конце первого раунда легендарные и мифические зовут сородичей
        // (сюжетный бой — нет); кому не хватило места — в очередь за строем
        summoned <-
          if (b1a.group.round != 1 || b1a.story.isDefined) ZIO.succeed((b1a, Vector.empty[String]))
          else ZIO.foldLeft(b1a.monstersInOrder.filter(m => BattleState.summons(Rarity.withName(m.rarity))))((b1a, Vector.empty[String])) {
            case ((b, log), caller) => summonKin(b, caller, res.hero.dungeonLevel).map { case (b2, line) => (b2, log :+ line) }
          }
        (b1b, log1b) = summoned
        // очередь: освободилось место — из-за спин выходят следующие
        (b1, entered) = b1b.admitQueued
        log1 = log1a ++ log1b ++ entered.map(m => content.format("battle.group.fromQueue", "monster" -> m.name)).toVector
        // перемешивание: каждый четвёртый раунд, если есть кого мешать. Группу
        // берём у УЖЕ перемешанного боя — иначе новый активный встанет поверх
        // старого строя, один моб пропадёт, а другой задвоится.
        shuffled <-
          if (!b1.isGroup || b1.group.round % GroupState.ShufflePeriod != 0) ZIO.succeed((b1, Vector.empty[String]))
          else Random.shuffle(b1.monstersInOrder.indices.toList).map { order =>
            val mixed = b1.reorderMonsters(order)
            val b     = mixed.copy(group = mixed.group.copy(pendingMove = None))
            val line  =
              if (b.group.paired) content.format("battle.group.shuffle", "monster" -> b.monsterName)
              else content.text("battle.group.shuffleNoPair")
            (b, Vector(line))
          }
        (b2, log2) = shuffled
        // Таран: в пару встаёт тот, кого таранили
        // Таран: герой шагает на место, куда таранил; мобы остаются где стояли
        rammed = b2.group.pendingMove match {
          case Some(pos) if b2.group.hasMonster(pos) =>
            val moved = b2.moveHeroTo(pos)
            // Таран бьёт только мобов; союзник, стоявший напротив таранённого,
            // уступает позицию герою и встаёт на его прежнюю.
            val b = moved.copy(group = moved.group.copy(allies = moved.group.allies.map(a =>
              if (a.position == pos) a.copy(position = b2.group.heroPos) else a)))
            (b.copy(group = b.group.copy(pendingMove = None)),
             Vector(content.format("battle.group.ram", "pos" -> pos.toString, "monster" -> b.monsterName)))
          case _ => (b2.copy(group = b2.group.copy(pendingMove = None)), Vector.empty[String])
        }
        (b3, log3) = rammed
      } yield res.copy(battle = b3, sideLog = res.sideLog ++ log1 ++ log2 ++ log3)
    }

  /** Призыв сородичей: легендарный зовёт 2–3 третьего–четвёртого ранга,
    * мифический — 1–3 первого–третьего. Той же расы, уровня этажа, со
    * стартовой энергией как у подкрепления. Кому не хватило места — в очередь. */
  private def summonKin(battle: SoloPveBattle, caller: MonsterSlot, dungeonLevel: Int): Task[(SoloPveBattle, String)] = {
    val (nMin, nMax, ranks) = Rarity.withName(caller.rarity) match {
      case Rarity.Legendary => (GroupState.LegendarySummonMin, GroupState.LegendarySummonMax, List(Rarity.Rare, Rarity.Mythical))
      case _                => (GroupState.MythicalSummonMin, GroupState.MythicalSummonMax, List(Rarity.Common, Rarity.Uncommon, Rarity.Rare))
    }
    val race = Race.withName(caller.race)
    for {
      n   <- Random.nextIntBetween(nMin, nMax + 1)
      kin <- ZIO.foreach((1 to n).toList) { _ =>
               for {
                 i   <- Random.nextIntBetween(0, ranks.size)
                 pct <- Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
                 m    = MonsterGenerator.generateOfRaceAndRarity(dungeonLevel, race, ranks(i))
               } yield MonsterSlot(m.lvl, m.race.entryName, m.rarity.entryName, m.fightStats, m.fightStats.hp,
                        m.fightStats.armor, m.marked, MonsterEnergy.startEnergy(m.lvl, m.rarity, pct), BattleEffects.empty)
             }
    } yield {
      val joined = kin.foldLeft(battle)(_ admit _)
      val waiting = joined.group.queue.size - battle.group.queue.size
      val line = content.format("battle.group.summon", "monster" -> caller.name, "kin" -> kin.map(_.name).mkString(", "))
      (joined, if (waiting > 0) line + " " + content.format("battle.group.summonQueued", "n" -> waiting.toString) else line)
    }
  }

  /** Строки группового экрана: кто с кем в паре, у кого сколько осталось. С
    * отрядом строй двухрядный: слева герой и союзники по позициям, справа мобы. */
  private def groupLines(battle: SoloPveBattle): Vector[String] = {
    val queued = if (battle.group.queue.isEmpty) Vector.empty
                 else Vector(content.format("battle.group.queueLine", "n" -> battle.group.queue.size.toString))
    formationLines(battle) ++ queued
  }

  private def formationLines(battle: SoloPveBattle): Vector[String] =
    if (battle.group.allies.isEmpty && battle.group.paired)
      battle.placesInOrder.map {
        case (pos, Some(m)) =>
          val key = if (pos == battle.group.heroPos) "battle.group.lineHero" else "battle.group.lineFree"
          content.format(key, "n" -> pos.toString, "monster" -> m.name, "hp" -> m.hpPct.toString, "armor" -> m.armorPct.toString)
        case (pos, None) =>
          content.format("battle.group.lineEmpty", "n" -> pos.toString)
      }.toVector
    else
      (1 to battle.group.rows).toVector.map { pos =>
        val left =
          if (pos == battle.group.heroPos) content.text("battle.group.sideHero")
          else battle.group.allyAt(pos).map(a => content.format("battle.group.sideAlly",
                 "name" -> a.name, "hp" -> a.hpPct.toString, "armor" -> a.armorPct.toString))
            .getOrElse(content.text("battle.group.sideNone"))
        val right = battle.monsterAt(pos).map(m => content.format("battle.group.sideMonster",
            "monster" -> m.name, "hp" -> m.hpPct.toString, "armor" -> m.armorPct.toString))
          .getOrElse(content.text("battle.group.sideNone"))
        content.format("battle.group.lineRow", "n" -> pos.toString, "left" -> left, "right" -> right)
      }

  // ── Победа ──────────────────────────────────────────────────────────────────

  /** Награды за победу: РОЛЛ + ВСЕ ЗАПИСИ в БД (опыт, лут в scene_data, очистка
    * боя, куб Азата, путь вглубь). Ничего не показывает — сообщения собирает
    * [[showVictory]] по возвращённому [[VictoryOutcome]]. Разделение сделано,
    * чтобы победа была ЗАФИКСИРОВАНА до первого сетевого вызова (см. commit). */
  private def applyVictory(
      user: User,
      hero: Hero,
      battle: SoloPveBattle
  ): Task[VictoryOutcome] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      azat <- loadAzat(user)
      // Недельное благословение Азата: +10% опыта/серебра, +10% редкости, +5% доп. дроп.
      blessed = azat.blessingActive(now)
      // Все павшие этого боя в порядке гибели; в бою 1 на 1 — один моб.
      fallen = battle.group.slain :+ battle.slainActive
      // Сюжетный бой: ни опыта, ни добычи по таблицам — только то, что положил
      // сюжет. Налёт на деревню мурлоков — исключение: награда как за обычный
      // бой, только этаж для неё — уровень героя (мурлоки его уровня).
      raid       = battle.story.contains(MurlocQuest.RaidStory)
      storyFight = battle.story.isDefined && !raid
      killLevel  = if (raid) hero.lvl else hero.dungeonLevel.toLong
      // У минибосса своя награда за уровень босса, а не по этажу и редкости.
      // Группа — опыт одной суммой за всех.
      baseExp = if (storyFight) 0L
                else battle.boss
                  .map(_.expReward(battle.monsterLvl))
                  .getOrElse(fallen.map(m => (killLevel * Rarity.withName(m.rarity).factor).toLong.max(1L)).sum)
                  .max(1L)
      expGained = if (storyFight) 0L else if (blessed) (baseExp * (100L + BattleState.BlessingBonusPct) / 100L).max(1L) else baseExp
      leveled = hero.gainExp(expGained)
      // лут катаем чистым ядром; начисление (инвентарь/серебро) — в LootState
      seed <- Random.nextLong
      // У минибосса дроп свой и всегда есть; обычная таблица лута не катается.
      // Группа — своя добыча с каждого павшего, по порядку гибели.
      (perMonster, rngAfter) = battle.boss match {
        case _ if storyFight => (List(battle.monsterName -> List.empty[LootGenerator.LootDrop]), Rng(seed))
        case Some(e) =>
          // Этаж встречи нужен клыку волка: трофей считается по нему.
          val (d, r) = LootGenerator.rollMiniBoss(e, battle.monsterLvl, hero.lvl, Rng(seed),
            floorLvl = hero.dungeonLevel.toLong)
          (List(battle.monsterName -> d), r)
        case None =>
          fallen.foldLeft((List.empty[(String, List[LootGenerator.LootDrop])], Rng(seed))) { case ((acc, rng), m) =>
            val (d, r) = LootGenerator.roll(
              Rarity.withName(m.rarity),
              Race.withName(m.race),
              killLevel,
              rng,
              gearChanceBonusPct = hero.gems.gearDropBonusPct,
              rarityBumpPct = if (blessed) BattleState.BlessingBonusPct else 0L
            )
            (acc :+ (m.name -> d), r)
          }
      }
      // «Таксидермист»/«Ювелир» дают отдельные доп. дропы поверх основного лута —
      // с каждого павшего.
      (withExtras, rngAfter2) = fallen.zip(perMonster).foldLeft((List.empty[(String, List[LootGenerator.LootDrop])], rngAfter)) {
        case ((acc, rng), (m, (name, d))) =>
          val (extra, r) = LootGenerator.rollPassiveDrops(
            hero.passives.hasTaxidermist,
            hero.passives.hasJeweler,
            Rarity.withName(m.rarity),
            Race.withName(m.race),
            killLevel,
            rng
          )
          (acc :+ (name -> (d ++ extra)), r)
      }
      // Благословение: 5% шанс дополнительной экипировки после боя.
      (blessingGear, _) =
        if (blessed) LootGenerator.rollBlessingExtraGear(BattleState.BlessingExtraDropPct, battle.rarity, killLevel, rngAfter2)
        else (Option.empty[pangea.model.item.Item], rngAfter2)
      lootByMonster = withExtras.map { case (name, drops) =>
        LootState.MonsterLoot(
          monsterName = name,
          items       = drops.flatMap(_.itemOpt),
          silvers     = drops.collect { case LootGenerator.LootDrop.Silver(a, _) => a * silverScalePct(blessed) / 100L },
          doubloons   = drops.collect { case LootGenerator.LootDrop.Doubloons(a) => a }.sum
        )
      }
      // Коллектор из «Письма Марисе» оставляет ровно 500 серебра и 10 дублонов.
      storyLoot = battle.story.filter(_ == MarisaQuest.CollectorStory).map(_ =>
        LootState.MonsterLoot(battle.monsterName, Nil, List(MarisaQuest.CollectorSilver), MarisaQuest.CollectorDoubloons))
      first = storyLoot.getOrElse(lootByMonster.head)
      // routing события (returnState/eventData) кладётся в scene_data ДО боя
      // (напр. цепочка «мобы с сокровищем»); переносим его в добычу, чтобы экран
      // добычи знал, куда вернуться. Для обычного боя scene_data пуст → None.
      prev <- heroDao
        .readSceneData(user.userId)
        .map(_.flatMap(_.as[LootState.LootData].toOption))
      // Сотый убитый: старейшина мурлоков ждёт героя после добычи — если та не
      // ведёт в другую сцену (тогда он выйдет на ближайшем осмотре этажа).
      kills = hero.kills + fallen.size.toLong
      elderWaits <- if (kills >= NpcQuest.MurlocElderKill) MurlocQuest.markPending(heroDao, user.userId, now)
                    else ZIO.succeed(false)
      // После добычи с элементаля игрок идёт осматривать ЕГО логово. Гнилого Джо
      // это не касается — осматривать после него нечего.
      lootReturn = if (battle.boss.exists(_.race == Race.Elemental))
                     Some(StateType.ElementalSearch)
                   else prev.flatMap(_.returnState).orElse(Option.when(elderWaits)(StateType.MurlocElder))
      // Первый экран добычи — первый павший; остальные ждут своей очереди. Имя
      // моба над добычей показываем только в группе.
      lootData = LootState.LootData(
        items = first.items ++ blessingGear.toList,
        silvers = first.silvers,
        doubloons = first.doubloons,
        returnState = lootReturn,
        eventData = prev.flatMap(_.eventData),
        monsterName = Option.when(fallen.size > 1)(first.monsterName),
        queue = lootByMonster.tail,
        won = true
      )
      // Первый поверженный легендарный моб роняет квестовый «Неактивный куб Азата»
      // (если куба ещё нет и он не был куплен). Хранится флагом в azat_data.
      cubeDropped = fallen.exists(_.rarity == Rarity.Legendary.entryName) && azat.cubeAbsent
      _ <- ZIO.when(cubeDropped)(saveAzat(user, azat.copy(cube = CubeStatus.FoundInactive)))
      _ <- heroDao.clearActiveBattle(user.userId)
      // Задание Густаво: павшие идут в счёт, пока действует его зелье.
      _ <- NpcQuestLog.onVictory(heroDao, user.userId, fallen.size, GustavoState.potionActive(hero, now))
      // Вампирская фляга пьёт кровь павших: с каждого — шанс на глоток.
      flaskRefill <- vampiricRefill(user, hero, fallen.size)
      // Счёт убитых за всю жизнь: пятидесятый оставляет письмо Марисе (сотый
      // зовёт старейшину мурлоков — см. выше).
      _ <- heroDao.updateKills(user.userId, kills)
      letterFound <- if (hero.kills < NpcQuest.MarisaLetterKill && kills >= NpcQuest.MarisaLetterKill)
                       MarisaQuest.giveLetter(heroDao, inventoryRepo, itemRepo, user.userId, hero)
                     else ZIO.succeed(false)
      _ <- heroDao.updateExpAndLevel(
        user.userId,
        leveled.exp,
        leveled.lvl,
        leveled.upgradePoints
      )
      _ <- heroDao.writeSceneData(user.userId, lootData.asJson)
      // победа над «Отмеченным тьмой» на текущем этаже открывает путь вглубь
      newMaxDungeon = math.min(150, hero.dungeonLevel + 1)
      unlocksDarkness =
        fallen.exists(_.marked) && newMaxDungeon > hero.maxDungeonLevel
      _ <- ZIO.when(unlocksDarkness)(
        heroDao.updateMaxDungeonLevel(user.userId, newMaxDungeon)
      )
    } yield VictoryOutcome(
      monsterName       = if (battle.boss.isDefined) battle.monsterName else fallen.map(_.name).mkString(", "),
      expGained         = expGained,
      newLvl            = Option.when(leveled.lvl > hero.lvl)(leveled.lvl),
      unlocksDarkness   = unlocksDarkness,
      cubeDropped       = cubeDropped,
      elementalDefeated = battle.boss.exists(_ != MiniBoss.WhiteWolf),
      wolfDefeated      = battle.boss.contains(MiniBoss.WhiteWolf),
      slainCount        = fallen.size,
      letterFound       = letterFound,
      flaskRefill       = flaskRefill
    )

  /** Вампирская фляга: за каждого павшего — бросок на +1 заряд (FlaskRates.VampiricRefillPct),
    * но не выше полной. Бросок не тратится, если фляга не вампирская или полна.
    * Возвращает (сколько долила, зарядов стало, максимум), если долила. */
  private def vampiricRefill(user: User, hero: Hero, slain: Int): Task[Option[(Int, Int, Int)]] =
    hero.equipment.flask.details match {
      case f @ ItemDetails.Flask(FlaskEffect.Vampiric(_, _), charges, max) if charges < max =>
        ZIO.foldLeft((1 to slain).toList)(charges) { (cur, _) =>
          chanceRoll(cur < max, FlaskRates.VampiricRefillPct).map(hit => if (hit) cur + 1 else cur)
        }.flatMap { after =>
          if (after == charges) ZIO.succeed(None)
          else heroDao.updateEquipment(user.userId,
                 hero.equipment.copy(flask = hero.equipment.flask.copy(details = f.withCharges(after))))
                 .as(Some((after - charges, after, max)))
        }
      case _ => ZIO.succeed(None)
    }

  /** Серебро с благословением Азата — на его бонус больше. */
  private def silverScalePct(blessed: Boolean): Long =
    if (blessed) 100L + BattleState.BlessingBonusPct else 100L

  /** Показ итогов победы — вызывается ПОСЛЕ того, как [[applyVictory]] всё
    * записал. Падение любого сообщения уже не откатывает бой. */
  private def showVictory(
      user: User,
      outcome: VictoryOutcome,
      renderer: Renderer
  ): Task[Unit] =
    for {
      _ <- renderer.show(
        user,
        Screen(
          content.format(
            if (outcome.expGained == 0L) "battle.victoryNoExp"
            else if (outcome.slainCount > 1) "battle.group.victory" else "battle.victory",
            "monster" -> outcome.monsterName,
            "exp"     -> outcome.expGained.toString
          ),
          Nil
        )
      )
      _ <- ZIO.when(outcome.letterFound)(
        renderer.show(user, Screen(content.text("marisa.letterFound") + "\n\n" +
          content.format("marisa.questItemAdded", "item" -> QuestItemKind.MarisaLetter.displayName), Nil))
      )
      _ <- ZIO.when(outcome.unlocksDarkness)(
        renderer.show(
          user,
          Screen(content.text("battle.darknessConquered"), Nil)
        )
      )
      _ <- ZIO.foreachDiscard(outcome.newLvl)(lvl =>
        renderer.show(user, Screen(s"Вы получили новый уровень $lvl!", Nil))
      )
      _ <- ZIO.when(outcome.cubeDropped)(
        renderer.show(user, Screen(content.text("battle.cubeFound"), Nil))
      )
      // Победа над минибоссом: своя реплика перед экраном добычи.
      _ <- ZIO.when(outcome.elementalDefeated)(
        renderer.show(user, Screen(content.text("battle.elemental.victory"), Nil))
      )
      _ <- ZIO.when(outcome.wolfDefeated)(
        renderer.show(user, Screen(content.text("battle.wolf.victory"), Nil))
      )
      _ <- ZIO.foreachDiscard(outcome.flaskRefill) { case (gained, charges, max) =>
        renderer.show(user, Screen(content.format("battle.flaskBlood",
          "gained" -> gained.toString, "charges" -> charges.toString, "max" -> max.toString), Nil))
      }
    } yield ()

  private def loadAzat(user: User): Task[AzatState] =
    ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      .flatMap(now => AzatData.load(heroDao, user.userId, now))

  private def saveAzat(user: User, azat: AzatState): Task[Unit] =
    heroDao.writeAzatData(user.userId, azat.asJson)

  // ── Экран боя ───────────────────────────────────────────────────────────────

  /** Экран боя; в группе перед ним — строй, чтобы с порога было видно, кто
    * стоит против героя (после хода строй идёт в сводке раунда). */
  private def showScreen(user: User, renderer: Renderer): Task[Unit] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      _ <- ZIO.when(battle.group.hasFormation)(renderer.show(user, Screen(groupLines(battle).mkString("\n"), Nil)))
      _ <- renderer.show(
        user,
        buildBattleScreen(hero, battle, hero.effectiveMaxHp(now), now)
      )
    } yield ()

  private def buildBattleScreen(
      hero: Hero,
      battle0: SoloPveBattle,
      maxHp: Long,
      nowMs: Long
  ): Screen = {
    // Напротив пусто — шансы героя считаем против последней его цели (кто в
    // полях, тот в этот момент дерётся не с ним).
    val battle    = if (battle0.group.paired) battle0 else battle0.group.lastTarget.map(battle0.withActive).getOrElse(battle0)
    val buffedEff = effWithAir(hero, battle, nowMs)
    val playerDodgePct  = playerDodgeChance(hero, battle, nowMs).toInt
    val monsterDodgePct = mobDodgeChance(buffedEff.accuracy, battle).toInt
    val mobHitPct       = 100 - playerDodgePct
    val heroHitPct      = 100 - monsterDodgePct
    val reductionPct = (BattleState.damageReduction(
      protection = buffedEff.defence,
      defenderInt = hero.effectiveBaseStats(nowMs).int,
      attackerInt = battle.monsterStats.atk,
      bonusPct = battle.heroBattleState.reductionBonusPct
    ) * 100.0).toInt
    // Защита моба показывается так же, как своя, — процентом снижения урона, и
    // уже с учётом пробития героя: сырое число защиты игроку ни о чём не говорит,
    // ведь один и тот же моб против сильного героя держит хуже.
    val monsterReductionPct = (monsterDefenceCut(hero, battle, buffedEff, nowMs) * 100.0).toInt
    // Энергия моба видна игроку: по ней читается, когда прилетит умение, и по ней
    // же видно работу Молнии. Шанс каста больше не показываем — его нет.
    val mobEnergy    = battle.monsterCurrentEnergy.max(0L)
    val mobMaxEnergy = battle.monsterStats.energy.max(0L)
    val maxEnergy     = hero.maxEnergy(nowMs)
    // Статус-эффекты рядом со статами своей сущности: яд у HP моба (-урон/ход),
    // реген у HP/брони героя (+восстановление/ход). Реген HP = зелье регенерации
    // (затухающее) + пассивка «Целебный» (4% макс.HP); реген брони =
    // «Самовосстанавливающийся» (4% макс.брони). Показываем суммарной припиской.
    val maxArmor = hero.effectiveMaxArmor(nowMs)
    // Сводка DoT у HP моба: яд 🟢 / кровотечение 🔴 / горение 🔥 (урон/ход).
    val monsterMaxHp = battle.monsterStats.hp
    val monsterPoison = {
      val parts = List(
        battle.effects.monsterPoison.map(p => s"🟢-${monsterDotDamage(battle, p.damageOn(monsterMaxHp))}"),
        battle.effects.monsterBleed.map(b => s"🔴-${monsterDotDamage(battle, b.damageOn(monsterMaxHp))}"),
        battle.effects.monsterBurn.map(bn => s"🔥-${bn.damageOn(monsterMaxHp)}")
      ).flatten
      if (parts.isEmpty) "" else " (" + parts.mkString(" ") + ")"
    }
    val beltHpRegen    = battle.effects.heroRegen.map(_.healOn(maxHp)).getOrElse(0L)
    // Реген HP/ход = зелье + «Целебный» (% макс.HP) + черепа в снаряжении (промилле макс.HP).
    val hpRegenTotal   = beltHpRegen + maxHp * hero.passives.hpRegenPct / 100L + maxHp * hero.gems.hpRegenPerMille / 1000L
    val heroRegen      = if (hpRegenTotal > 0) s" (+$hpRegenTotal)" else ""
    val armorRegen     = maxArmor * hero.passives.armorRegenPct / 100L
    val heroArmorRegen = if (armorRegen > 0) s" (+$armorRegen)" else ""
    val text = content.format(
      if (battle0.group.paired) "battle.enter.text" else "battle.enter.noPair",
      "monster"         -> battle.monsterName,
      "monsterRace"     -> battle.toMonster.race.toString,
      "monsterHp"       -> battle.monsterCurrentHp.toString,
      "monsterMax"      -> battle.monsterStats.hp.toString,
      "monsterPoison"   -> monsterPoison,
      "heroRegen"       -> heroRegen,
      "monsterArmor"    -> battle.monsterCurrentArmor.toString,
      "monsterMaxArmor" -> battle.monsterStats.armor.toString,
      "monsterAtk"      -> battle.monsterStats.atk.toString,
      "monsterReduction" -> s"$monsterReductionPct%",
      "mobHit"          -> s"$mobHitPct%",
      "monsterDodge"    -> s"$monsterDodgePct%",
      "heroHit"         -> s"$heroHitPct%",
      "heroDodge"       -> s"$playerDodgePct%",
      "heroHp"          -> hero.fightStats.hp.toString,
      "heroMax"         -> maxHp.toString,
      "heroArmorRegen"  -> heroArmorRegen,
      "heroArmor"       -> hero.fightStats.armor.min(maxArmor).toString,
      "heroMaxArmor"    -> maxArmor.toString,
      "heroAtk"        -> buffedEff.atk.toString,
      "heroReduction"  -> s"$reductionPct%",
      "heroEnergy"     -> hero.fightStats.energy.min(maxEnergy).toString,
      "heroMaxEnergy"  -> maxEnergy.toString,
      "monsterEnergy"    -> mobEnergy.toString,
      "monsterMaxEnergy" -> mobMaxEnergy.toString,
      "flaskCharges"   -> BattleState.flaskCharges(hero).toString
    )
    // Готовый навык синий, если хватает энергии на каст, и красный — если нет.
    val skillButtons = battle.skillSlots.collect {
      case slot if slot.cooldown <= 0 =>
        val enoughEnergy = hero.fightStats.energy >= slot.skill.energyCost(hero)
        pangea.engine.Choice(
          id = s"Skill_${slot.itemId}",
          label = slot.skill.label,
          color =
            if (enoughEnergy) pangea.engine.ChoiceColor.Primary
            else pangea.engine.ChoiceColor.Negative,
          row = Some(0)
        )
    }
    // Ряды кнопок: row 0 — активные навыки; row 1 — Атака; row 2 — Фляга и Пояс
    // (Пояс — только если несёт зелье); row 3 — Сбежать. Места под заглушки (доп.
    // слот, стиль атаки, сменить цель) пока скрыты.
    val flaskCharges = BattleState.flaskCharges(hero)
    val flaskButton = pangea.engine.Choice(
      "UseFlask",
      s"🧪 Фляга ($flaskCharges)",
      color =
        if (flaskCharges <= 0) pangea.engine.ChoiceColor.Negative
        else pangea.engine.ChoiceColor.Primary,
      row = Some(2)
    )
    val beltButton = BattleState.beltPotion(hero).map { belt =>
      pangea.engine.Choice(
        "UseBelt",
        s"👝 Пояс (${belt.charges})",
        color =
          if (belt.charges <= 0) pangea.engine.ChoiceColor.Negative
          else pangea.engine.ChoiceColor.Primary,
        row = Some(2)
      )
    }
    // Бить некого (напротив пусто, соседей нет) — вместо атаки «Переместиться»
    // (поменяться местами с союзником), а без союзников — «Ждать».
    val attackButton =
      if (battle0.group.attackTargets.nonEmpty) pangea.engine.Choice("Attack", "Атака", row = Some(1))
      else if (battle0.group.allies.exists(_.alive)) pangea.engine.Choice("Move", content.text("battle.group.moveLabel"), row = Some(1))
      else pangea.engine.Choice("Wait", content.text("battle.group.waitLabel"), row = Some(1))
    val mainButtons =
      (List(attackButton, flaskButton)) ++
        beltButton.toList ++
        List(
          pangea.engine.Choice(
            "Flee",
            "Сбежать",
            color = pangea.engine.ChoiceColor.Negative,
            row = Some(3)
          )
        )
    Screen(text, skillButtons ++ mainButtons)
  }

  private def getHero(user: User): Task[Hero] =
    heroDao
      .getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def getBattle(user: User): Task[SoloPveBattle] =
    heroDao
      .readActiveBattle(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No active battle for user ${user.userId}"))
      .flatMap(json => ZIO.fromEither(json.as[SoloPveBattle]))

  /** Уклонение игрока от удара моба, в процентах [5;95]: защита игрока в знаменателе,
    * точность моба ×1.5 (снижена «Сливающимся» на 10%, см. [[HeroPassives.enemyAccuracyMult]] —
    * это правит саму точность атакующего в формуле, а не добавляет уклонение герою напрямую).
    * Поверх базового шанса добавляются бонус боевых бафов (зелье уворота) и плоские
    * бонусы пассивок — «Быстрые ноги» всегда, а «Сверкающий» — только при бегстве
    * (`fleeing`). Потолок 95% абсолютный. */
  private def playerDodgeChance(hero: Hero, battle: SoloPveBattle, nowMs: Long, fleeing: Boolean = false): Double = {
    // Буст Воздуха (+10% уклонения на airBoostTurns ходов) входит в effWithAir.
    val buffed          = effWithAir(hero, battle, nowMs)
    val agi             = hero.effectiveBaseStats(nowMs).agi
    val passiveBonus    = hero.passives.dodgeBonusPct + (if (fleeing) hero.passives.fleeDodgeBonusPct else 0L)
    // Скованный холодом элементаль бьёт менее точно — срез в п.п. от его точности.
    val chillCut        = if (battle.effects.chilled) MiniBoss.FireElemental.ChilledAccuracyCutPct else 0L
    // Горящему Джо тоже не до прицеливания (у кого пламя точность не сбивает —
    // срез нулевой, см. MiniBoss.burnAccuracyCutPct).
    val burnCut         =
      if (battle.effects.monsterBurn.isEmpty) 0L
      else battle.boss.map(_.burnAccuracyCutPct).getOrElse(0L)
    // Прок Воздуха у самого моба поднимает ему точность на те же 10%.
    val airBoost = if (battle.effects.mobAirBoostTurns > 0) Element.Air.ProcBonusPct else 0L
    val monsterAccuracy =
      (battle.monsterStats.accuracy * hero.passives.enemyAccuracyMult *
        (100L + airBoost - chillCut - burnCut).max(0L) / 100.0).toLong
    (BattleState.dodgeChance(agi, buffed.evasion, buffed.defence, monsterAccuracy)
      + battle.heroBattleState.dodgeBonus + passiveBonus).min(95.0).max(5.0)
  }

  // Уклонение моба от удара игрока — та же формула: у моба нет ловкости (agi = 0),
  // в знаменателе только точность игрока ×1.5. Защита сюда БОЛЬШЕ НЕ входит: она
  // отвечает лишь за процентное снижение урона (см. monsterDefenceCut), иначе
  // одна цифра работала бы дважды — и в уклонении, и в защите.
  private def mobDodgeChance(heroAccuracy: Long, battle: SoloPveBattle): Double = {
    // Тот же прок Воздуха добавляет мобу и уклонения; инстинкт волка — свои 5%.
    val airPct      = if (battle.effects.mobAirBoostTurns > 0) Element.Air.ProcBonusPct else 0L
    val instinctPct = if (battle.effects.mobInstinct) MiniBoss.WhiteWolf.InstinctBoostPct else 0L
    val evasion     = battle.monsterStats.evasion * (100L + airPct + instinctPct) / 100L
    BattleState.dodgeChance(0L, evasion, 0L, heroAccuracy)
  }
}

object BattleState {

  /** Исход хода — определяет переход и терминальные действия в [[BattleState.commit]]. */
  sealed trait Outcome
  object Outcome {
    case object Continue extends Outcome
    case object Victory  extends Outcome
    case object Death    extends Outcome
    case object Fled     extends Outcome
  }

  /** Уже ЗАПИСАННЫЙ итог победы — всё, что осталось показать игроку.
    * `newLvl` непуст, если ход дал новый уровень. */
  final case class VictoryOutcome(
      // В групповом бою — имена всех павших через запятую.
      monsterName: String,
      expGained: Long,
      newLvl: Option[Long],
      unlocksDarkness: Boolean,
      cubeDropped: Boolean,
      // Победа над минибоссом ведёт не в обычную добычу, а в осмотр логова.
      elementalDefeated: Boolean = false,
      // Белый волк: своя реплика, логова у него нет.
      wolfDefeated: Boolean = false,
      // Сколько мобов легло в этом бою: больше одного — групповая реплика.
      slainCount: Int = 1,
      // Пятидесятый убитый оставил письмо Марисе — отдельное сообщение после победы.
      letterFound: Boolean = false,
      // Вампирская фляга напилась крови: сколько долила, стало, максимум.
      flaskRefill: Option[(Int, Int, Int)] = None
  )

  /** Результат чистого вычисления хода: итоговый герой и бой (для персиста),
    * накопленный лог сообщений (склеивается и показывается один раз) и исход. */
  /** Итог обычной атаки моба по герою — см. `BattleState.mobStrike`. */
  final case class MobStrike(newHp: Long, newArmor: Long, damage: Long, extraLines: List[String], battle: SoloPveBattle)

  /** Удар союзника: бой после него, кого бил, сколько снял, добил ли соседа. */
  final case class AllyBlow(battle: SoloPveBattle, target: Option[String], damage: Long, slew: Boolean)

  /** Кто зовёт сородичей в конце первого раунда. */
  def summons(rarity: Rarity): Boolean = rarity == Rarity.Legendary || rarity == Rarity.Mythical

  /** Раунд боя без героя — каждые полминуты. */
  val SquadTickMs: Long = 30L * 1000L
  val SquadTickAction: String = """{"action":"SquadTick"}"""
  /** С каким HP герой приходит в себя, когда отряд добил всех без него. */
  val DownReviveHp: Long = 1L

  final case class TurnResult(
      hero: Hero,
      battle: SoloPveBattle,
      log: Vector[String],
      outcome: Outcome,
      // Что делали мобы вне пары: удары сбоку, лечение врага, подкрепление,
      // перемешивание. Идёт игроку ОТДЕЛЬНЫМ сообщением после лога раунда.
      sideLog: Vector[String] = Vector.empty,
      // Был ли это ход, за которым идёт конец раунда: атака, умение, бегство.
      // Глоток фляги или зелья и «ничего не случилось» (умение не готово, фляга
      // пуста) раунд не завершают — мобы вне пары не ходят, подкрепление не идёт.
      endsRound: Boolean = true
  )

  /** Текущее число зарядов надетой фляги (0, если фляга не надета). */
  def flaskCharges(hero: Hero): Int = hero.equipment.flask.details match {
    case f: ItemDetails.Flask => f.charges
    case _                    => 0
  }

  /** Зелье надетого пояса (если пояс несёт зелье) — для кнопки «Пояс» и её зарядов. */
  def beltPotion(hero: Hero): Option[ItemDetails.Belt] = hero.equipment.belt.details match {
    case b: ItemDetails.Belt => Some(b)
    case _                   => None
  }

  /** Бонус благословения Азата (в %): к опыту, серебру и редкости добычи. */
  val BlessingBonusPct: Long = AzatState.BlessingBonusPct
  /** Шанс (в %) дополнительной экипировки после боя при благословении. */
  val BlessingExtraDropPct: Long = AzatState.BlessingExtraDropPct

  /** Из payload UserAction достаём itemId, если action имеет вид `Skill_<id>`.
    */
  def parseSkillAction(ua: pangea.service.state.UserAction): Option[Long] = {
    val key = ua.payload
      .flatMap(p =>
        io.circe.jawn
          .decode[Map[String, String]](p)
          .toOption
          .flatMap(_.get("action"))
      )
      .getOrElse("")
    key match {
      case s"Skill_$rest" => rest.toLongOption
      case _              => None
    }
  }

  /** Номер цели умения в строю (с нуля), если кнопка его несла. */
  def parseTarget(ua: pangea.service.state.UserAction): Option[Int] =
    ua.payload.flatMap(p =>
      io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("target")).flatMap(_.toIntOption))

  def parseAction(ua: pangea.service.state.UserAction): Option[String] =
    ua.payload.flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  /** Лечит ли умение или чинит броню — такое можно отдать союзнику. */
  def supports(effect: Skill.Effect): Boolean = effect match {
    case Skill.Effect.Heal | Skill.Effect.RepairArmor | Skill.Effect.GuardRepair(_, _) => true
    case _                                                                             => false
  }

  /** Бьёт ли умение по врагу — таким в группе нужна цель. */
  def dealsDamage(effect: Skill.Effect): Boolean = effect match {
    case Skill.Effect.Damage(_) | Skill.Effect.Sweep(_) | Skill.Effect.Whirl | Skill.Effect.FanBleed(_) | Skill.Effect.Knockback |
         Skill.Effect.BleedDamage(_) | Skill.Effect.WeakSpotStrike | Skill.Effect.BloodHarvest => true
    case Skill.Effect.Heal | Skill.Effect.RepairArmor | Skill.Effect.GuardRepair(_, _) | Skill.Effect.WarCry(_, _) => false
  }

  /** Нужен ли умению хоть один враг в досягаемости: удар — по цели, клич — по
    * тем, кто рядом; без них ход не тратится. */
  def needsEnemy(effect: Skill.Effect): Boolean = effect match {
    case Skill.Effect.WarCry(_, _) => true
    case other                     => dealsDamage(other)
  }

  /** На сколько тиков Боевой клич запирает умения мобов вокруг: гаснет один
    * каст — ближайший (двойка по той же причине, что и у ComboSkillBlockTurns). */
  val WarCrySkillBlockTurns: Int = 2

  /** Шанс уклонения защищающегося юнита от удара атакующего, в процентах, зажат
    * в [5, 95]: 100 * (agi + evasion) / (agi + evasion + defence * 1 +
    * attackerAccuracy * 1.5) Единая логика «попадания по юниту» для обеих
    * сторон: положительные параметры (числитель) — ловкость и уклонение
    * защищающегося; отрицательные (знаменатель) — защита защищающегося (×1) и
    * точность атакующего (×1.5). Атакующий попадает, если бросок 1..100 больше
    * уклонения.
    */
  /** Процентное снижение урона, наносимого защищающемуся юниту: reduction = (P
    * + Iₚ) / (P + Iₚ + Iₑ × 2), затем к итогу ПРИБАВЛЯЕТСЯ bonusPct п.п. —
    * процентный баф «Заслона» бьёт по ИТОГОВОМУ снижению, а не по защите. Всё
    * вместе зажато сверху 0.7. Где `P` — защита защитника, `Iₚ` — его интеллект,
    * `Iₑ` — интеллект атакующего, `bonusPct` — прибавка в п.п. к итогу. Применяется
    * к чистому урону до брони. Соотношение «×2 для интеллекта атакующего» — из ТЗ.
    * Возврат — доля [0; 0.7].
    */
  def damageReduction(
      protection: Long,
      defenderInt: Long,
      attackerInt: Long,
      bonusPct: Long
  ): Double = {
    val numer   = (protection + defenderInt).toDouble
    val denom   = numer + attackerInt * 2.0
    val raw     = if (denom <= 0.0) 0.0 else numer / denom
    val boosted = raw + bonusPct / 100.0
    boosted.max(0.0).min(0.7)
  }

  /** Мощь бойца — его ударный потенциал: у героя это сила втрое плюс атака (ровно
    * то, из чего складывается удар), у моба — просто атака. Стоит в знаменателе
    * снижения урона у того, кого он бьёт, поэтому считается одинаково для обеих
    * сторон — так же будет и в бою двух героев. */
  def power(str: Long, atk: Long): Long = str * 3L + atk

  /** Пробитие защиты: интеллект и ловкость с двойным весом, десятая часть Мощи и
    * стат «пробитие» с предметов (пока его никто не даёт). Растёт линейно по
    * уровню — в одном темпе с защитой мобов, поэтому доля срезанной защиты не
    * уезжает от первого уровня к последнему. У мобов пробития нет вовсе: защиту
    * героя они не пробивают. */
  def pierce(int: Long, agi: Long, power: Long, pierceStat: Long): Long =
    2L * int + 2L * agi + power / 10L + pierceStat

  /** Снижение урона защитой: пробитие сначала съедает саму защиту, и уже её
    * остаток идёт в общую формулу. Интеллекта у мобов нет, поэтому в числителе
    * только защита. */
  def defenceReduction(defence: Long, pierce: Long, attackerPower: Long): Double =
    damageReduction(
      protection  = (defence - pierce).max(0L),
      defenderInt = 0L,
      attackerInt = attackerPower,
      bonusPct    = 0L
    )

  /** Травма от удара моба: удар должен снять больше `HitTraumaMinPct` потолка
    * HP, чтобы вообще считаться; тогда шанс `HitTraumaChancePct`; удар сверх
    * `HitTraumaCrushPct` потолка — сокрушительный, шанс `HitTraumaCrushChancePct`. */
  val HitTraumaMinPct: Long         = 5L
  val HitTraumaChancePct: Long      = 1L
  val HitTraumaCrushPct: Long       = 50L
  val HitTraumaCrushChancePct: Long = 50L

  /** Сколько процентов ПОТОЛКА брони срезает комбо Молния+Холод. */
  val ComboArmorCutPct: Long = 10L

  /** На сколько тиков комбо запирает умение моба. Гаснет один каст — тот, что был
    * бы ответом на комбо. Двойка, а не единица, потому что тик буфов идёт в начале
    * хода моба и сразу съедает один заряд. */
  val ComboSkillBlockTurns: Int = 2

  def dodgeChance(
      agi: Long,
      evasion: Long,
      defence: Long,
      attackerAccuracy: Long
  ): Double = {
    val positive = (agi + evasion).toDouble
    val denom    = positive + defence * 1.0 + attackerAccuracy * 1.5
    val raw      = if (denom <= 0.0) 0.0 else 100.0 * positive / denom
    raw.max(5.0).min(95.0)
  }
}
