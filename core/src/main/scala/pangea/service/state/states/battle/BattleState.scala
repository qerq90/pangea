package pangea.service.state.states.battle

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.generator.loot.LootGenerator
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{Aura, AuraRates, BattleAlly, BattleEffects, Bleed, Buff, Burn, Element, Formation, GroupState, HealedWounds, MonsterSlot, Nature, Poison, RaceTactics, Regen, SoloPveBattle, SkillSlotState, TimedDefenceDebuff}
import pangea.model.cave.CaveGenerator
import pangea.model.item.ItemSet
import pangea.model.squad.AllySkill
import pangea.model.hero.{Achievement, AzatState, CubeStatus, Hero, WeaponCoat, WeaponDust}
import pangea.model.item.QuestItemKind
import pangea.model.quest.{BoardKind, NpcQuest}
import pangea.model.item.{BeltSlot, BrewEffect, BrewKind, BrewRates, FlaskEffect, FlaskRates, GripRates, Item, ItemDetails, ItemType, PassiveKind, PotionKind, RefillSource, DivineKind, DivineRates}
import pangea.model.monster.{MiniBoss, Race, Rarity}
import pangea.model.stats.FightStats
import pangea.model.stats.{ParamsBuff, StatBoost}
import pangea.model.trauma.{Trauma, TraumaRoll}
import pangea.model.schedule.TaskKind
import pangea.model.skill.{MonsterEnergy, MonsterSkill, Skill}
import pangea.model.state.StateType
import pangea.model.user.{User, UserId}
import pangea.service.state.states.LootState
import pangea.service.state.MurlocQuest
import pangea.service.state.states.gustavo.GustavoState
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.dao.arena.ArenaDao
import pangea.model.arena.{ArenaAlly, ArenaRates, ArenaSide}
import pangea.service.state.states.arena.ArenaBattle
import pangea.service.state.states.battle.BattleState.{ArenaOwnActions, Victim}
import pangea.service.schedule.Scheduler
import pangea.service.state.{AzatData, BoardProgress, MarisaQuest, NpcQuestLog, State, UserAction, KillLogData}
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
  scheduler:     Scheduler = Scheduler.none,
  // Арена: строка боя двух игроков. Без неё бой на арену не попадает — в
  // лабиринте она не нужна.
  arenaDao:      Option[ArenaDao] = None
) extends State {

  import BattleState.{AllyBlow, MobStrike, Outcome, TurnResult, VictoryOutcome}

  /** Чистое вычисление одного хода: снимок состояния → результат хода. */
  private type Turn = (Hero, SoloPveBattle, Long) => Task[TurnResult]

  private val branch = new Branch(
    routes = Map(
      "Attack"      -> Target.Run((u, ua, r) => attackRoute(u, BattleState.parseTarget(ua), r)),
      "Wait"        -> Target.Run((u, _, r) => resolve(u, r, content.text("arena.turn.wait"))(waitTurn)),
      "Move"        -> Target.Run((u, _, r) => moveRoute(u, r)),
      "MoveTo"      -> Target.Run((u, ua, r) => resolve(u, r, content.text("arena.turn.move"))(moveTurn(BattleState.parseTarget(ua)))),
      "SquadTick"   -> Target.Run((u, _, r) => squadTick(u, r)),
      "UseFlask"    -> Target.Run((u, _, r) => resolve(u, r, content.text("arena.turn.flask"))(flaskTurn)),
      // Пояс ход не тратит: он присылает список расходников, а применяет уже
      // кнопка внутри того сообщения.
      "Belt"        -> Target.Run((u, _,  r) => showBelt(u, r)),
      "BeltUse"     -> Target.Run((u, ua, r) => beltUse(u, ua, r)),
      "UseDivine"    -> Target.Run((u, _, r) => resolve(u, r, content.text("arena.turn.divine"))(divineTurn)),
      "BeltPick"     -> Target.Run((u, ua, r) => beltPick(u, ua, r)),
      "ThrowMix"     -> Target.Run((u, _, r) => resolve(u, r, content.text("arena.turn.mix"))(mixTurn)),
      "Miasma"      -> Target.Run((u, _, r) => toggleMiasma(u, r)),
      "Flee"        -> Target.Run((u, _, r) => flee(u, r)),
      "ConfirmFlee" -> Target.Run((u, _, r) => resolve(u, r, content.text("arena.turn.flee"))(fleeTurn)),
      "CancelFlee"  -> Target.Run((u, _, r) => showScreen(u, r).as(StateType.Battle)),
      "CancelTarget" -> Target.Run((u, _, r) => showScreen(u, r).as(StateType.Battle)),
      // Арена зовёт к экрану: соперник сходил, бой кончился или минута вышла.
      "ArenaPoke"    -> Target.Run((u, _, r) => arenaPoke(u, r)),
      "ArenaTurn"    -> Target.Run((u, _, r) => arenaTimeout(u, r))
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
      if (battle.group.heroDown) showDown(user, battle, renderer).unit
      else startingRat(user, battle, renderer) *> showScreen(user, renderer)
    }

  /** «Крыса», порог 12: если в начале боя при герое нет ни одной крысы, одна
    * приходит сразу, без броска. «Начало боя» — пока не кончился первый раунд:
    * уход за снаряжением и возврат на том же раунде второй крысы не приводят, а
    * павшую в бою она уже не заменяет. */
  private def startingRat(user: User, battle: SoloPveBattle, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap { hero =>
      // На арене крыса на старте выходит вместе с героем (см. ArenaBattle.withStartingRat).
      if (!hero.sets.startsWithRat || battle.group.round > 0 || battle.group.hasRatAlly || battle.arena.isDefined) ZIO.unit
      else callRat(hero, battle) match {
        case None => ZIO.unit
        case Some((joined, line)) =>
          heroDao.writeActiveBattle(user.userId, joined.asJson) *>
            renderer.show(user, Screen(line, Nil)).unit
      }
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
          else
            // На арене ходят по очереди: пока соперник думает, кнопки молчат.
            // Зов арены и просроченный ход — не в счёт, они и двигают бой.
            arenaWaits(user, battle, BattleState.parseAction(ua)).flatMap {
              case true  => renderer.show(user, Screen(content.text("arena.notYourTurn"), Nil)).as(StateType.Battle)
              case false => branch.act(user, ua, renderer)
            }
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
  private def resolve(user: User, renderer: Renderer, note: String)(turn: Turn): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      state  <- resolveLoaded(user, renderer, hero, battle, now, note)(turn)
    } yield state

  private def resolveLoaded(user: User, renderer: Renderer, hero: Hero, battle: SoloPveBattle, now: Long,
                            note: String)(turn: Turn): Task[StateType] =
    for {
      // Щит, поднятый в прошлом раунде, своё отстоял, и кинжал наизготовку тоже:
      // их снимает первое действие нового раунда. Посреди раунда — после пояса,
      // Блока или Контрудара — они ещё держатся (см. Skill.Block, Counterstrike).
      struck <- turn(hero, if (battle.midRound) battle else battle.copy(heroBlockPct = 0L, heroCounterReady = false), now)
      // «Упырь» (порог 12): пир — только за тех, кого уложил САМ герой, и тут
      // же, в бою. Считаем до хода союзников: их добычу упырь не ест.
      feasted = ghoulFeast(battle, struck, now)
      // «Некромант» (порог 12): добитый героем может встать и пойти за ним.
      result <- necroRise(battle, feasted)
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
      // Добил союзник — Джо встаёт так же, как от удара героя.
      promoted2 = promoteAfterKill(joeRises(allied))
      sided   <- sideMobsPhase(promoted2, now)
      // Арена: раны на чужих бойцах тикают в конце хода того, кто их наложил.
      wounded  = arenaWounds(sided)
      // Активный вне пары мог истечь ранами на своём ходу — тогда следующий
      // (а Джо — встаёт).
      promoted3 = promoteAfterKill(joeRises(wounded))
      // Аура держит всё поле: нежить крепнет, живые чахнут. Добитых ею разбираем
      // тем же порядком, что павших от ран.
      // Раны союзников тикают там же, где и прочие: после всех ходов раунда.
      allyTicked = tickAllyEffects(promoted3)
      prefs   <- BattleState.prefs(heroDao, user.userId)
      fogged   = auraTick(allyTicked, prefs.miasma)
      promoted4 = promoteAfterKill(fogged)
      // Охрана вся полегла — стрелкам некого прикрывать: они бросают башни и
      // уходят, бой на этом кончается победой.
      abandoned = towersAlone(promoted4)
      ended   <- endRound(battle, abandoned)
      // «Крыса» (порог 6): раз в раунд у героя из-под ног может выскочить своя.
      ratted  <- ratCall(hero, ended)
      // Напротив героя пусто, а свободный моб появился — он шагает к герою.
      pulled  = pullFreeStep(ratted)
      // Мобы своё прошли — теперь отряд: кому некого бить, идёт к врагу.
      advanced = squadStep(pulled)
      // Подсказка про поглощённый удар идёт последней строкой раунда — уже после
      // всего, что в нём случилось.
      hinted  = plainSteelHint(hero, advanced)
      // Раунд не кончился — герой ещё в нём (см. SoloPveBattle.midRound).
      marked   = hinted.copy(battle = hinted.battle.copy(midRound = hinted.outcome == Outcome.Continue && !hinted.endsRound))
      state  <- commit(user, marked, now, renderer, hero, note)
    } yield state

  /** Крыса из-под ног героя встаёт в строй на его стороне — если там есть
    * свободное место. Она тот же моб, что водится в канализации: обычная на
    * пороге 6, чумная на пороге 12 ([[HeroSets.summonsPlagueRats]]), уровнем в
    * самого героя. `home = 0` помечает её как пришедшую не из отряда — после боя
    * по этому признаку она в отряд и попадает (см. [[squadAfterBattle]]).
    *
    * Возвращает бой и строку для лога, либо None, если встать некуда. */
  private def callRat(hero: Hero, battle: SoloPveBattle): Option[(SoloPveBattle, String)] =
    battle.group.nearestFreeAllyPlace.map { pos =>
      val ally = BattleState.ratAlly(hero, pos)
      (battle.copy(group = battle.group.copy(allies = battle.group.allies :+ ally)),
       content.format("battle.ratCall", "name" -> ally.name, "n" -> pos.toString))
    }

  /** «Крыса», порог 6: раз в раунд из-под ног героя может выскочить крыса. Пока
    * герой лежит, звать её некому; бросок без набора не тратится. */
  private def ratCall(hero: Hero, res: TurnResult): Task[TurnResult] =
    chanceRoll(
      res.outcome == Outcome.Continue && res.endsRound && !res.battle.group.heroDown &&
        hero.sets.ratSummonChancePct > 0,
      hero.sets.ratSummonChancePct
    ).map {
      case false => res
      case true  => callRat(hero, res.battle).fold(res) { case (joined, line) =>
        res.copy(battle = joined, log = res.log :+ line)
      }
    }

  /** Башни стоят, пока есть кого прикрывать. Не осталось живой охраны — бой
    * кончен: стрелки уходят, сами башни в добычу и опыт не идут. */
  private def towersAlone(res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Continue) res
    else if (res.battle.livingGuards.nonEmpty) res
    else if (!(res.battle.activeSlot :: res.battle.group.others).exists(s =>
               s.alive && BattleState.isTowerSlot(s))) res
    else res.copy(outcome = Outcome.Victory, log = res.log :+ content.text("battle.towersAbandoned"))

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
      renderer: Renderer,
      before: Hero,
      note: String
  ): Task[StateType] =
    raw.battle.arena match {
      case Some(ref) => arenaCommit(user, ref, raw, nowMs, renderer, before, note)
      case None      => dungeonCommit(user, raw, nowMs, renderer, before)
    }

  /** Остальное, что ход поменял в герое, — и что тоже пишется только здесь:
    * бафы (отвар с пояса, лопнувший пузырь), травмы (снятые костоправным),
    * быстрые отдыхи от бодрящего сбора и, пока бой идёт, пыль со смазкой на
    * оружии — покрытие живёт до конца боя, и о нём должен знать не только
    * текущий ход. Пишем лишь то, что изменилось против героя до хода. */
  private def persistExtras(user: User, res: TurnResult, before: Hero, nowMs: Long): Task[Unit] =
    ZIO.when(res.hero.statBoosts != before.statBoosts)(
      heroDao.updateStatBoosts(user.userId, res.hero.statBoosts)) *>
      ZIO.when(res.hero.traumaNames != before.traumaNames || res.hero.traumaUntil != before.traumaUntil)(
        heroDao.updateTrauma(user.userId, res.hero.traumaUntil, res.hero.traumaNames)) *>
      ZIO.when(res.instantRests > 0)(
        AzatData.load(heroDao, user.userId, nowMs).flatMap(azat =>
          heroDao.writeAzatData(user.userId,
            azat.copy(instantRests = azat.instantRests + res.instantRests).asJson))) *>
      ZIO.when(res.outcome == Outcome.Continue && res.hero.weaponDust != before.weaponDust)(
        heroDao.updateWeaponDust(user.userId, res.hero.weaponDust)).unit

  /** Бой кончился: пыль на оружии осыпается — покрытие на один бой, чем бы он
    * ни кончился, — а опустевшие ячейки пояса добирают из сумки то же, чем
    * были полны. Здесь только запись; что легло на пояс, говорит
    * [[showRefilled]] — показы идут после всех записей хода. */
  private def afterBattle(user: User, hero: Hero): Task[List[String]] =
    ZIO.when(!hero.weaponDust.isEmpty)(heroDao.updateWeaponDust(user.userId, WeaponDust.empty)) *>
      refillBelt(user, hero)

  private def showRefilled(user: User, names: List[String], renderer: Renderer): Task[Unit] =
    ZIO.when(names.nonEmpty)(renderer.show(user, Screen(content.format("battle.belt.refilled",
      "names" -> names.distinct.mkString(", ")), Nil))).unit

  private def dungeonCommit(
      user: User,
      raw: TurnResult,
      nowMs: Long,
      renderer: Renderer,
      before: Hero
  ): Task[StateType] = {
    // Пузырьковый нектар: смерть, которой не будет. Считаем до persistHero,
    // чтобы спасённый герой сохранился вместе с остальным исходом.
    val res = bubbleRescue(raw, nowMs)
    val persistHero =
      heroDao.updateEquipmentAndFightStats(user.userId, res.hero.equipment, res.hero.fightStats) *>
        persistExtras(user, res, before, nowMs)
    // Отряд: что с союзниками стало и кто ушёл по свитку — по концу боя.
    // Крыса, выскочившая из-под ног (`home = 0`), после боя остаётся при герое —
    // даже если до этого отряда у него не было вовсе.
    val persistSquad =
      ZIO.when(res.hero.squad.nonEmpty || res.battle.group.alliesGone.nonEmpty ||
               res.battle.group.allies.exists(_.home <= 0))(
        heroDao.updateSquad(user.userId, squadAfterBattle(res.hero, res.battle, nowMs)))
    val msg     = res.log.mkString("\n")
    val showLog = ZIO.when(msg.nonEmpty)(renderer.show(user, Screen(msg, Nil)))
    // Групповая сводка: что делали мобы вне пары и кто где стоит. Строй
    // показываем, только пока бой идёт, — после него он не нужен. А вот что
    // случилось сбоку, говорим при любом исходе: иначе союзник, которого
    // унесло свитком тем же ходом, каким кончился бой, пропадал бы молча.
    val standing  = if (res.outcome == Outcome.Continue && res.battle.group.hasFormation) groupLines(res.battle) else Vector.empty
    val groupMsg  = (res.sideLog ++ standing).mkString("\n")
    val showGroup = ZIO.when(groupMsg.nonEmpty)(renderer.show(user, Screen(groupMsg, Nil)))
    res.outcome match {
      // Приговорённый дотерпел свои раунды и ушёл: добычи с него нет, но и
      // герой цел — событие просто кончилось.
      case Outcome.Continue if res.battle.escapesAfter > 0 && res.battle.group.round >= res.battle.escapesAfter =>
        (persistHero *> persistSquad *> afterBattle(user, res.hero) <* heroDao.clearActiveBattle(user.userId))
          .uninterruptible.flatMap { refilled =>
            showLog *> showGroup *> showRefilled(user, refilled, renderer) *>
              renderer.show(user, Screen(content.format("battle.preyEscaped", "monster" -> res.battle.monsterName), Nil)) *>
              ZIO.when(res.battle.group.heroDown)(
                renderer.show(user, Screen(content.text("battle.squad.heroUpEscaped"), Nil)))
          }
            .as(StateType.Dungeon)
      case Outcome.Continue =>
        (persistHero *> heroDao.writeActiveBattle(user.userId, res.battle.asJson)).uninterruptible *>
          showLog *> showGroup *>
          mixCount(res.hero).flatMap(mixes =>
            renderer.show(user, buildBattleScreen(res.hero, res.battle, res.hero.effectiveMaxHp(nowMs), nowMs, mixes)))
            .as(StateType.Battle)
      case Outcome.Victory =>
        for {
          written <- (persistHero *> persistSquad *> afterBattle(user, res.hero) <*>
                       applyVictory(user, res.hero, res.battle)).uninterruptible
          (refilled, outcome) = written
          _       <- showLog
          // Что случилось сбоку в этот же ход — до итогов победы: иначе
          // союзник, которого унесло свитком, пропал бы молча.
          _       <- showGroup
          _       <- showRefilled(user, refilled, renderer)
          _       <- showVictory(user, outcome, renderer)
        } yield StateType.Loot
      case Outcome.Death if res.battle.group.allies.exists(_.alive) && !res.battle.group.heroDown =>
        // Отряд ещё на ногах: герой падает, но не умирает — бой идёт без него,
        // раунд за раундом по таймеру, а смерть отложена до исхода.
        val fell = res.battle.copy(group = res.battle.group.copy(heroDown = true))
        val (down, turned) = fell.leaveDownedHero
        (persistHero *> heroDao.writeActiveBattle(user.userId, down.asJson)).uninterruptible *>
          showLog *> showGroup *>
          scheduler.schedule(user.userId, nowMs + BattleState.SquadTickMs, TaskKind.SquadFight, StateType.Battle, BattleState.SquadTickAction) *>
          renderer.show(user, Screen(content.text("battle.squad.heroDown") + turnsAwayLine(down, turned), Nil)) *>
          showDown(user, down, renderer)
      case Outcome.Death =>
        (persistHero *> persistSquad *> afterBattle(user, res.hero)).uninterruptible.flatMap { refilled =>
          showLog *> showGroup *> showRefilled(user, refilled, renderer) *>
            renderer.show(user, Screen(content.text("battle.death"), Nil)) *>
            failQuestBehind(user, renderer).as(StateType.Death)
        }
      case Outcome.Fled =>
        // Сюжетный бой идёт не в лабиринте — бежать из него в город.
        val to = if (res.battle.story.isDefined) StateType.GlobalMap else StateType.Dungeon
        (persistHero *> persistSquad *> afterBattle(user, res.hero) <* heroDao.clearActiveBattle(user.userId))
          .uninterruptible.flatMap { refilled =>
            showLog *> showGroup *> showRefilled(user, refilled, renderer) *>
              renderer.show(user, Screen(content.text("battle.fled"), Nil)) *>
              failQuestBehind(user, renderer).as(to)
          }
    }
  }

  /** За каким заданием герой пришёл в этот бой: видно по роутингу добычи — он
    * говорит, куда бой вернётся, а значит и откуда герой здесь. Караван и
    * пещеры ходят своими состояниями, воры — экраном поживы. Пещеру, канализацию
    * и пещеру некроманта различает сама сцена. */
  private def questBehind(user: User): Task[Option[BoardKind]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[LootState.LootData].toOption).flatMap { loot =>
      loot.returnState.flatMap {
        case StateType.ThievesSpoils => Some(BoardKind.Thieves)
        case StateType.Caravan       => Some(BoardKind.CaravanRout)
        case StateType.MonsterCave   =>
          loot.eventData.flatMap(_.as[pangea.model.cave.CaveScene].toOption)
            .map(pangea.service.state.states.events.cave.MonsterCaveState.questOf)
            .orElse(Some(BoardKind.CaveClear))
        case _ => None
      }
    })

  /** Герой отступил от квестовой цели — сбежал или пал. Цель на этом потеряна:
    * объявление пропадает с доски. Сделанное задание (`done`) это не трогает —
    * охрану он уже перебил, а плату берут у доски. */
  private def failQuestBehind(user: User, renderer: Renderer): Task[Unit] =
    questBehind(user).flatMap {
      case None       => ZIO.unit
      case Some(kind) =>
        BoardProgress.markFailed(heroDao, user.userId, kind).flatMap { failed =>
          // Цель потеряна — возвращаться к ней некуда: роутинг добычи стираем,
          // иначе он остался бы висеть и повёл бы героя к пустой сцене.
          ZIO.when(failed)(
            heroDao.writeSceneData(user.userId, Json.Null) *>
              renderer.show(user, Screen(content.text("questBoard.failed"), Nil))).unit
        }
    }

  // ── Бой на арене ────────────────────────────────────────────────────────────

  /** Не ход ли это соперника. Зов арены и просроченный ход проходят всегда:
    * первый показывает свежий экран, второй бьёт за того, кто задумался. */
  private def arenaWaits(user: User, battle: SoloPveBattle, action: Option[String]): Task[Boolean] =
    (battle.arena, arenaDao) match {
      case (Some(ref), Some(dao)) if !ArenaOwnActions.contains(action.getOrElse("")) =>
        dao.byId(ref.fightId).map(_.exists(f => !f.isTurn(user.userId)))
      case _ => ZIO.succeed(false)
    }

  /** Итог хода на арене. Добычи, опыта и смерти здесь нет: победа — это
    * сообщение обоим, поражение — единица HP и пустая броня. Ход, который
    * ничего не изменил (неготовое умение, пустая фляга), сопернику не уходит. */
  private def arenaCommit(
      user: User,
      ref: pangea.model.battle.ArenaRef,
      raw: TurnResult,
      nowMs: Long,
      renderer: Renderer,
      before: Hero,
      note: String
  ): Task[StateType] = {
    val res         = raw
    val persistHero =
      heroDao.updateEquipmentAndFightStats(user.userId, res.hero.equipment, res.hero.fightStats) *>
        persistExtras(user, res, before, nowMs)
    val msg         = res.log.mkString("\n")
    val showLog     = ZIO.when(msg.nonEmpty)(renderer.show(user, Screen(msg, Nil)))
    res.outcome match {
      // Ход раунда не кончил — сопернику он не уходит, но бой всё равно пишем:
      // расходник с пояса мог оставить в нём баф, копии или отметку «за пояс
      // в этом раунде уже брались».
      case Outcome.Continue if !res.endsRound =>
        (persistHero *> heroDao.writeActiveBattle(user.userId, res.battle.asJson)).uninterruptible *>
          showLog *> showArenaScreen(user, res.hero, res.battle, nowMs, renderer).as(StateType.Battle)

      case Outcome.Continue =>
        for {
          _ <- (persistHero *> heroDao.writeActiveBattle(user.userId, res.battle.asJson)).uninterruptible
          _ <- showLog
          _ <- passTurn(user, ref, res, before, note, nowMs, renderer)
        } yield StateType.Battle

      // Герой пал, а его отряд ещё на ногах: сторона падает только целиком.
      // Он лежит, отряд бьётся за него — ход уходит сопернику.
      case Outcome.Death if res.battle.group.allies.exists(_.alive) =>
        val down = res.copy(battle = res.battle.copy(group = res.battle.group.copy(heroDown = true)),
                            outcome = Outcome.Continue)
        for {
          _ <- (persistHero *> heroDao.writeActiveBattle(user.userId, down.battle.asJson)).uninterruptible
          _ <- showLog
          _ <- renderer.show(user, Screen(content.text("arena.down"), Nil))
          _ <- passTurn(user, ref, down, before, note, nowMs, renderer)
        } yield StateType.Battle

      // Соперник со всем отрядом обнулён — он проиграл, а герой уходит с песка
      // своим ходом. Лежал, пока отряд добивал, — встаёт с единицей HP. Пыль и
      // пояс — как после любого боя (см. afterBattle).
      case Outcome.Victory =>
        val standing = if (res.hero.fightStats.hp <= 0L) ArenaBattle.beaten(res.hero) else res.hero
        (heroDao.updateEquipmentAndFightStats(user.userId, standing.equipment, standing.fightStats) *>
          persistExtras(user, res, before, nowMs) *> afterBattle(user, standing)).uninterruptible.flatMap { refilled =>
          showLog *> showRefilled(user, refilled, renderer) *>
            endArena(user, ref, winner = true, arenaSummary(note, before, res, None), nowMs, renderer)
        }

      // Обнулили героя или он сбежал — поражение. Смерти арена не знает.
      case Outcome.Death | Outcome.Fled =>
        val beaten = if (res.outcome == Outcome.Death) ArenaBattle.beaten(res.hero) else res.hero
        (heroDao.updateEquipmentAndFightStats(user.userId, beaten.equipment, beaten.fightStats) *>
          persistExtras(user, res, before, nowMs) *> afterBattle(user, beaten)).uninterruptible.flatMap { refilled =>
          showLog *> showRefilled(user, refilled, renderer) *>
            endArena(user, ref, winner = false, arenaSummary(note, before, res, None), nowMs, renderer)
        }
    }
  }

  /** Ход уходит сопернику: своё складываем в строку боя, ему шлём зов к
    * экрану, себе снимаем минуту ожидания. */
  private def passTurn(user: User, ref: pangea.model.battle.ArenaRef, res: TurnResult,
                       before: Hero, note: String, nowMs: Long, renderer: Renderer): Task[Unit] =
    arenaDao match {
      case None => ZIO.unit
      case Some(dao) =>
        dao.byId(ref.fightId).flatMap {
          case None => ZIO.unit // строки нет — бой уже кончился, ход слать некому
          case Some(fight) =>
            val foeUser = UserId(ref.foeUser)
            val mine = fight.sideOf(user.userId).map(ArenaBattle.harvest(_, res.battle))
            for {
              // HP и броню соперника пишем ему самому: правда о бойце — в герое.
              foeHero <- heroDao.getHeroByUserId(foeUser)
              _       <- ZIO.foreachDiscard(foeHero) { h =>
                           val hurt = ArenaBattle.foeAfter(h, res.battle)
                           heroDao.updateFightStats(foeUser, hurt.fightStats)
                         }
              // Соперник читает не чужие «вы ударили», а сводку: что применили
              // и чем это для него и его отряда обернулось.
              lines = arenaSummary(note, before, res, foeHero,
                        fight.sideOf(foeUser).map(_.allies).getOrElse(Nil))
              // Сбитого с мысли, отброшенного и пригвождённого запоминаем на нём
              // самом: в своём зеркале он этого не видит.
              foeSide = fight.sideOf(foeUser).map(ArenaBattle.foeAfterTurn(_, res.battle))
              moves   = arenaMoves(fight.sideOf(user.userId), fight.sideOf(foeUser), res.battle)
              told    = foeSide.foldLeft(mine.fold(fight)(fight.withSide))(_ withSide _).withLog(lines ++ moves)
              updated = told.seenBy(user.userId).passTurn(foeUser, nowMs)
              _ <- dao.update(updated, nowMs)
              _ <- scheduler.cancel(user.userId, TaskKind.ArenaTurn)
              _ <- wake(foeUser, nowMs)
              _ <- scheduler.schedule(foeUser, nowMs + ArenaRates.TurnMs, TaskKind.ArenaTurn,
                     StateType.Battle, BattleState.ArenaTurnAction)
              _ <- renderer.show(user, Screen(content.format("arena.passed",
                     "name" -> res.battle.monsterName), Nil))
            } yield ()
        }
    }

  /** Бой кончился: строка уходит, соперника зовём узнать итог. */
  private def endArena(user: User, ref: pangea.model.battle.ArenaRef, winner: Boolean,
                       lastLines: List[String], nowMs: Long, renderer: Renderer): Task[StateType] =
    for {
      // Победа: соперника с песка тоже надо поднять — единица HP и ни клочка
      // брони. Сам он об этом узнает, когда арена позовёт его к экрану.
      _ <- ZIO.when(winner) {
             val foe = UserId(ref.foeUser)
             heroDao.getHeroByUserId(foe).flatMap(h =>
               ZIO.foreachDiscard(h)(x => heroDao.updateFightStats(foe, ArenaBattle.beaten(x).fightStats)))
           }
      _ <- heroDao.clearActiveBattle(user.userId)
      _ <- scheduler.cancel(user.userId, TaskKind.ArenaTurn)
      // Строку не рвём: соперник ещё не знает, чем кончилось, — прочитает и
      // сотрёт её сам (см. ArenaState).
      _ <- ZIO.foreachDiscard(arenaDao) { dao =>
             dao.byId(ref.fightId).flatMap {
               case None => ZIO.unit
               case Some(fight) =>
                 val me  = user.userId
                 val end = fight.withLog(lastLines).seenBy(me).copy(
                   status = pangea.model.arena.ArenaStatus.Finished,
                   turn   = None,
                   winner = Some(if (winner) me else UserId(ref.foeUser)))
                 dao.update(end, nowMs)
             }
           }
      _ <- wake(UserId(ref.foeUser), nowMs)
      _ <- renderer.show(user, Screen(content.text(if (winner) "arena.won" else "arena.lost"), Nil))
    } yield StateType.Arena

  /** Позвать игрока к экрану: он сам себя не обновит — ходит-то соперник. */
  private def wake(who: UserId, nowMs: Long): Task[Unit] =
    scheduler.schedule(who, nowMs, TaskKind.ArenaPoke, StateType.Battle, BattleState.ArenaPokeAction).unit

  /** Зов арены: соперник сходил, или бой кончился, пока герой ждал. */
  private def arenaPoke(user: User, renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- heroDao.readActiveBattle(user.userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      res <- battle.flatMap(b => b.arena.map(b -> _)) match {
        case None => ZIO.succeed(StateType.Battle)
        case Some((_, ref)) =>
          arenaDao match {
            case None => ZIO.succeed(StateType.Battle)
            case Some(dao) =>
              dao.byId(ref.fightId).flatMap {
                // Строки нет — бой кончился и прочитан: уводим героя с песка.
                case None =>
                  heroDao.clearActiveBattle(user.userId) *>
                    renderer.show(user, Screen(content.text("arena.over"), Nil, hideKeyboard = true))
                      .as(StateType.Arena)
                case Some(fight) =>
                  // Что соперник успел сделать, пока герой ждал.
                  if (fight.finished) finishArena(user, fight, dao, now, renderer)
                  else tellFoeTurn(user, fight, dao, now, renderer) *>
                         refreshArena(user, hero, fight, now, renderer)
              }
          }
      }
    } yield res

  /** Перерисовать бой по свежей строке арены: свой ход — кнопки, чужой —
    * строка ожидания. */
  private def refreshArena(user: User, hero: Hero, fight: pangea.model.arena.ArenaFight,
                           nowMs: Long, renderer: Renderer): Task[StateType] =
    (fight.sideOf(user.userId), fight.foeOf(user.userId)) match {
      case (Some(me), Some(foe)) =>
        for {
          foeHero <- heroDao.getHeroByUserId(foe.userId).map(_.getOrElse(hero))
          fresh    = ArenaBattle.assemble(fight, me, foe, hero, foeHero, nowMs)
          _       <- heroDao.writeActiveBattle(user.userId, fresh.asJson)
          // Чужой ход — кнопки прячем: жать их всё равно нельзя, а соблазн есть.
          // Свой, а герой лежит — бьётся отряд, и ход уходит сам.
          out     <- if (fight.isTurn(user.userId) && fresh.group.heroDown)
                       resolveLoaded(user, renderer, hero, fresh, nowMs, content.text("arena.turn.down"))(downTurn)
                     else if (fight.isTurn(user.userId)) showArenaScreen(user, hero, fresh, nowMs, renderer).as(StateType.Battle)
                     else (showArenaPlaces(user, fresh, renderer) *>
                            renderer.show(user, Screen(content.format("arena.waitFoe", "name" -> foe.name),
                              Nil, hideKeyboard = true))).as(StateType.Battle)
        } yield out
      case _ => ZIO.succeed(StateType.Battle).unit.as(StateType.Battle)
    }

  /** Бой кончился: показываем итог тому, кто его ещё не читал, и уводим с
    * песка. Строку стираем, когда её прочитали обе стороны, — иначе соперник
    * остался бы с экраном боя и без объяснений. */
  private def finishArena(user: User, fight: pangea.model.arena.ArenaFight, dao: ArenaDao,
                          nowMs: Long, renderer: Renderer): Task[StateType] = {
    val fresh = fight.seenBy(user.userId)
    val tell  = ZIO.when(fight.unseenFor(user.userId))(
      renderer.show(user, Screen(fight.lastLog.mkString("\n"), Nil)) *>
        renderer.show(user, Screen(
          content.text(if (fight.winner.contains(user.userId)) "arena.won" else "arena.lost"),
          Nil, hideKeyboard = true)))
    heroDao.clearActiveBattle(user.userId) *> tell *>
      (if (fresh.readByAll) dao.delete(fight.id) else dao.update(fresh, nowMs))
        .as(StateType.Arena)
  }

  /** Сводка хода для соперника: что применили и чем это для него кончилось.
    * Полные тексты умений написаны от первого лица («ваш рёв раскатывается»)
    * — в чужом логе они и длинны, и читаются как свои. Здесь только суть:
    * название да числа. */
  private def arenaSummary(note: String, before: Hero, res: TurnResult, foeBefore: Option[Hero],
                           foeAllies: List[ArenaAlly] = Nil): List[String] = {
    val what = if (note.isEmpty) content.text("arena.turn.acted") else note
    val dealt = foeBefore.map { h =>
      val after = ArenaBattle.foeAfter(h, res.battle)
      (h.fightStats.hp - after.fightStats.hp).max(0L) + (h.fightStats.armor - after.fightStats.armor).max(0L)
    }.getOrElse(0L)
    val alliesHurt = ArenaBattle.alliesHurt(foeAllies, res.battle)
    val hpBack    = (res.hero.fightStats.hp - before.fightStats.hp).max(0L)
    val armorBack = (res.hero.fightStats.armor - before.fightStats.armor).max(0L)
    val hpLost    = (before.fightStats.hp - res.hero.fightStats.hp).max(0L)
    List(
      Some(content.format("arena.turn.did", "what" -> what)),
      Option.when(dealt > 0L)(content.format("arena.turn.dealt", "n" -> dealt.toString)),
      Option.when(alliesHurt > 0L)(content.format("arena.turn.alliesHurt", "n" -> alliesHurt.toString)),
      Option.when(hpBack > 0L || armorBack > 0L)(content.format("arena.turn.healed",
        "hp" -> hpBack.toString, "armor" -> armorBack.toString)),
      // Своё же горение или яд соперник тоже видит — иначе чужое HP «само» падает.
      Option.when(hpLost > 0L)(content.format("arena.turn.selfHurt", "n" -> hpLost.toString))
    ).flatten
  }

  /** Что ход сделал с местами — для соперника: кто куда встал и надолго ли он
    * пригвождён. Строй он увидит и сам, но сводка должна объяснять, почему
    * соперник вдруг далеко. */
  private def arenaMoves(me: Option[ArenaSide], foe: Option[ArenaSide], battle: SoloPveBattle): List[String] =
    List(
      me.filter(_.pos != battle.group.heroPos).map(_ =>
        content.format("arena.turn.moved", "n" -> battle.group.heroPos.toString)),
      foe.filter(_.pos != battle.group.activePos).map(_ =>
        content.format("arena.turn.pushedYou", "n" -> battle.group.activePos.toString)),
      foe.filter(_.rooted < battle.effects.monsterRootedTurns).map(_ =>
        content.format("arena.turn.pinnedYou", "n" -> battle.effects.monsterRootedTurns.toString))
    ).flatten

  /** Пересказать сопернику чужой ход — один раз: прочитанное помечаем. */
  private def tellFoeTurn(user: User, fight: pangea.model.arena.ArenaFight, dao: ArenaDao,
                          nowMs: Long, renderer: Renderer): Task[Unit] =
    if (!fight.unseenFor(user.userId)) ZIO.unit
    else {
      val who   = fight.foeOf(user.userId).map(_.name).getOrElse("")
      val lines = content.format("arena.foeTurn", "name" -> who) + "\n" + fight.lastLog.mkString("\n")
      renderer.show(user, Screen(lines, Nil)) *> dao.update(fight.seenBy(user.userId), nowMs)
    }

  private def showArenaScreen(user: User, hero: Hero, battle: SoloPveBattle,
                              nowMs: Long, renderer: Renderer): Task[Unit] =
    mixCount(hero).flatMap(mixes =>
      showArenaPlaces(user, battle, renderer) *>
        renderer.show(user, buildBattleScreen(hero, battle, hero.effectiveMaxHp(nowMs), nowMs, mixes)))

  /** Строй на арене виден всегда, даже вплотную: от расстояния зависит, кто
    * кого достаёт и бьёт ли лук в полную силу. */
  private def showArenaPlaces(user: User, battle: SoloPveBattle, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(groupLines(battle).mkString("\n"), Nil))

  /** Минута вышла: бьём за зевнувшего обычной атакой — и ход уходит дальше.
    * Достать соперника нечем — он выжидает: ход уходит всё равно. Герой лежит
    * — бьётся его отряд. */
  private def arenaTimeout(user: User, renderer: Renderer): Task[StateType] =
    heroDao.readActiveBattle(user.userId).map(_.flatMap(_.as[SoloPveBattle].toOption)).flatMap {
      case Some(b) if b.arena.isDefined && b.group.heroDown =>
        resolve(user, renderer, content.text("arena.turn.down"))(downTurn)
      case Some(b) if b.arena.isDefined =>
        getHero(user).flatMap { hero =>
          if (heroTargets(hero, b).nonEmpty)
            renderer.show(user, Screen(content.text("arena.timeout"), Nil)) *> attackRoute(user, None, renderer)
          else
            renderer.show(user, Screen(content.text("arena.timeoutWait"), Nil)) *>
              resolve(user, renderer, content.text("arena.turn.wait"))(waitTurn)
        }
      case _ => ZIO.succeed(StateType.Battle)
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
        if (target.isEmpty && heroTargets(hero, battle).size > 1)
          renderer.show(user, attackTargetScreen(hero, battle)).as(StateType.Battle)
        else resolveLoaded(user, renderer, hero, battle, now, content.text("arena.turn.attack"))(attackTurn(target))
    } yield state

  /** Базовая атака по месту `target` (без цели — по единственной досягаемой):
    * по мобу в паре — с его ответом; по соседу — через подмену в полях, после
    * чего отвечает моб в паре, а если пара пуста — ход тихо кончается и мобы
    * ходят сами. Бить некого — ход не тратится. */
  private def attackTurn(target: Option[Int])(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] = {
    val targets = heroTargets(hero, battle)
    target.orElse(targets.headOption) match {
      case None                                 => cont(hero, battle, content.text("battle.group.noTarget"))
      case Some(pos) if !targets.contains(pos)  => cont(hero, battle, content.text("battle.group.targetGone"))
      case Some(pos) if pos == battle.group.heroPos => playerStrike(hero, battle.rememberTarget(pos), nowMs, Vector.empty, Set.empty)
      case Some(pos) => strikeSide(hero, battle, pos, nowMs, Vector.empty, Set.empty).flatMap(closeTurn(_, nowMs, Set.empty))
    }
  }

  /** Арена: герой лежит, а отряд бьётся — в его ход действует только отряд,
    * раны на чужих тикают, и ход уходит сопернику. Самого героя не трогают:
    * ни тиков, ни ответа. */
  private def downTurn(hero: Hero, battle: SoloPveBattle, @annotation.unused nowMs: Long): Task[TurnResult] =
    ZIO.succeed(TurnResult(hero, battle.copy(group = battle.group.copy(heroDown = true)),
      Vector(content.text("arena.downTurn")), Outcome.Continue))

  /** «Ждать»: бить некого — герой пропускает удар, раунд идёт своим чередом. */
  private def waitTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    closeTurn(TurnResult(hero, battle, Vector(content.text("battle.group.waited")), Outcome.Continue), nowMs, Set.empty)

  /** «Переместиться»: с кем из союзников поменяться местами. */
  private def moveRoute(user: User, renderer: Renderer): Task[StateType] =
    getBattle(user).flatMap { battle =>
      // Шаг ровно один — влево или вправо, как у мобов. Соседнее место может
      // быть пустым: тогда герой просто переходит на него.
      val choices = BattleState.stepTargets(battle).zipWithIndex.map { case (pos, i) =>
        val label = battle.group.allyAt(pos).filter(_.alive) match {
          case Some(a) => content.format("battle.group.moveTo", "n" -> pos.toString, "name" -> a.name)
          case None    => content.format("battle.group.moveToFree", "n" -> pos.toString)
        }
        pangea.engine.Choice("MoveTo", pangea.engine.Choice.fit(label),
          data = Map("target" -> pos.toString), row = Some(i / BattleState.AlliesPerRow))
      }
      val cancel = pangea.engine.Choice("CancelTarget", content.text("battle.group.cancelTarget"),
        color = pangea.engine.ChoiceColor.Negative,
        row = Some((choices.size + BattleState.AlliesPerRow - 1) / BattleState.AlliesPerRow))
      renderer.show(user, Screen(content.text("battle.group.moveWhom"), choices :+ cancel)).as(StateType.Battle)
    }

  /** Герой делает шаг на соседнее место. Стоял там союзник — они меняются
    * местами. Сам союзник сходит с места, только когда бить ему некого: в
    * конце раунда он идёт к врагу (см. `SoloPveBattle.squadAdvance`).
    * Стоял напротив моб — герой теперь с ним в паре, и тот отвечает; ход на
    * этом кончается. */
  private def moveTurn(target: Option[Int])(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    target.filter(BattleState.stepTargets(battle).contains) match {
      case None => cont(hero, battle, content.text("battle.group.targetGone"))
      case Some(pos) =>
        val from    = battle.group.heroPos
        val ally    = battle.group.allyAt(pos).filter(_.alive)
        val swapped = ally.fold(battle)(a =>
          battle.copy(group = battle.group.updateAlly(a.position)(_.copy(position = from))))
        val moved   = if (swapped.group.hasMonster(pos)) swapped.moveHeroTo(pos)
                      else swapped.copy(group = swapped.group.copy(heroPos = pos))
        val line    = ally match {
          case Some(a) => content.format("battle.group.moved", "name" -> a.name, "n" -> pos.toString)
          case None    => content.format("battle.group.stepped", "n" -> pos.toString)
        }
        closeTurn(TurnResult(hero, moved, Vector(line), Outcome.Continue), nowMs, Set.empty)
    }

  /** Раунд без героя (он обнулён): союзники бьют, мобы отвечают им — ударом и
    * умениями, минибоссы идут по своему кругу; раны тикают, аура держит поле,
    * Гнилой Джо встаёт. Конец раунда — как обычно, но героя никто не трогает, а
    * кому некого бить, идёт к врагу: мобы к отряду, отряд к мобам. Мобов не осталось — герой
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
          // Место напротив союзника могло освободиться и позже — смотрим каждый раунд.
          moved    <- ZIO.succeed(battle.leaveDownedHero)
          (walked, turnedTo) = moved
          allied   <- allyPhase(TurnResult(hero, walked,
                        Vector(turnsAwayLine(walked, turnedTo)).filter(_.nonEmpty), Outcome.Continue))
          // Гнилой Джо встаёт, кто бы его ни добил: союзник — тоже.
          promoted  = promoteAfterKill(joeRises(allied))
          // Раунд идёт и без героя: временное на поле сходит за свои раунды, как
          // в его ход (tickBuffs). Иначе пригвождённый (📌) так и стоял бы
          // пригвождённым до конца боя, а дым не рассеивался бы вовсе.
          ticked    = promoted.copy(battle = promoted.battle.tickBuffs())
          sided    <- sideMobsPhase(ticked, now)
          // Истёк ранами на своём ходу — тоже смерть, и от неё Джо тоже встаёт.
          promoted2 = promoteAfterKill(joeRises(sided))
          // Дальше — как в обычном раунде: раны на союзниках тикают, аура держит
          // поле. Свою ауру лежачий не держит — держать её нечем; чужая (миазмы
          // Некроманта) работает как всегда.
          allyTicked = tickAllyEffects(promoted2)
          fogged     = promoteAfterKill(auraTick(allyTicked, on = false))
          // Охрану доломали союзники, пока герой лежал, — башням некого
          // прикрывать, и бой кончается так же, как если бы её добил он сам.
          abandoned = towersAlone(fogged)
          ended    <- endRound(battle, abandoned)
          // Конец раунда: кому некого бить, идёт к врагу — мобы к отряду, отряд
          // к мобам. Каждый раунд, даже если в этом никто никого не задел: иначе
          // такой раунд ничем бы не кончался, и следующий тоже — бой стоял бы.
          res       = if (ended.outcome != Outcome.Continue) ended
                      else ended.copy(battle = ended.battle.closeInOnSquad._1.squadAdvance._1)
          state    <- res.outcome match {
            case Outcome.Victory =>
              val revived = res.hero.copy(fightStats = res.hero.fightStats.copy(hp = BattleState.DownReviveHp))
              commit(user, res.copy(hero = revived, log = res.log :+ content.text("battle.squad.heroUp")), now, renderer, hero, "")
            case _ if res.battle.group.allies.forall(!_.alive) =>
              commit(user, res.copy(outcome = Outcome.Death, log = res.log :+ content.text("battle.squad.allGone")), now, renderer, hero, "")
            // Приговорённый дотерпел свои раунды и ушёл — и пока герой лежал тоже:
            // добычи нет, а герой приходит в себя, как после победы отряда.
            case _ if res.battle.escapesAfter > 0 && res.battle.group.round >= res.battle.escapesAfter =>
              val revived = res.hero.copy(fightStats = res.hero.fightStats.copy(hp = BattleState.DownReviveHp))
              commit(user, res.copy(hero = revived), now, renderer, hero, "")
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

  /** Экран героя, лежащего без сил: только строй. Кнопок нет и не нужно —
    * жать лежачему нечего, бой идёт своей чередой по таймеру. Клавиатуру
    * убираем явно: иначе так и висели бы кнопки боя, которые всё равно ничего
    * не делают. */
  private def showDown(user: User, battle: SoloPveBattle, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(
      content.text("battle.squad.downScreen") + "\n\n" + groupLines(battle).mkString("\n"),
      Nil, hideKeyboard = true)).as(StateType.Battle)

  /** Строка о том, что моб ушёл от лежащего к союзнику; пустая, если не ушёл. */
  private def turnsAwayLine(battle: SoloPveBattle, to: Option[Int]): String =
    to.flatMap(pos => battle.group.allyAt(pos)).fold("")(ally =>
      "\n" + content.format("battle.squad.turnsAway",
        "monster" -> battle.monsterName, "name" -> ally.name))

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
  private def respond(hero: Hero, battle: SoloPveBattle, nowMs: Long, log: Vector[String],
                      skip: Set[Long], vampHeal: Long = 0L): Task[TurnResult] =
    // На арене напротив живой игрок: он не отвечает тут же, а бьёт своим
    // ходом. Ход героя кончается тиками — и уходит сопернику.
    if (battle.arena.isDefined) quietPhase(hero, battle, nowMs, log, skip, vampHeal)
    else if (battle.group.paired) monsterPhase(hero, battle, nowMs, log, skip, vampHeal)
    else quietPhase(hero, battle, nowMs, log, skip, vampHeal)

  /** Конец раунда без моба в паре: тик бафов и кулдаунов, раны и реген героя,
    * энергия. Мобы ходят потом сами — как мобы вне пары. */
  private def quietPhase(hero: Hero, battle: SoloPveBattle, nowMs: Long, log: Vector[String],
                         skip: Set[Long], vampHeal: Long = 0L): Task[TurnResult] = {
    // Минибосс, стоящий вне пары (Некромант за спинами своих), круга не теряет:
    // колдует он тем же порядком, где бы ни стоял. Тому, кто ушёл из пары своим
    // же шагом, этот ход не достаётся — он уже сходил в фазе пары.
    val casted =
      if (battle.boss.exists(_.holdsPosition) && battle.monsterCurrentHp > 0L)
        bossTurnCast(hero, battle, nowMs, Victim.Hero)
      else ZIO.succeed((battle, hero, ""))
    casted.map { case (afterCast, castHero, castLine) =>
      val ticked = afterCast.tickBuffs(skip)
      val (tickedHero, tickedBattle, heroEffectLine) = tickHeroEffects(castHero, ticked, nowMs)
      val alive = tickedHero.fightStats.hp > 0
      val fed   = if (alive) endOfRoundHero(tickedHero, nowMs) else tickedHero
      val lines = (log ++ Vector(castLine).filter(_.nonEmpty) ++
                    Vector(heroEffectLine).filter(_.nonEmpty)) ++ vampLine(vampHeal)
      TurnResult(fed, tickedBattle, lines, if (alive) Outcome.Continue else Outcome.Death)
    }
  }

  /** Конец раунда при живом герое: кому из отряда некого бить, шагает к
    * ближайшему врагу (`SoloPveBattle.squadAdvance`) — так же, как пока герой
    * лежит. Идут после мобов: те уже подтянулись к герою, и отряд идёт к тем,
    * кто остался вне досягаемости. На арене бойцы шагают только своим ходом —
    * за них здесь не ходят. */
  private def squadStep(res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Continue || !res.endsRound || res.battle.group.heroDown || res.battle.arena.isDefined) res
    else res.copy(battle = res.battle.squadAdvance._1)

  /** Кто сам дошёл до места напротив героя, тот и встаёт с ним в пару (конец
    * раунда). Издалека никого не тянем: павший в паре замены не подставляет, и
    * место перед героем враги занимают сами, шаг за шагом. */
  private def pullFreeStep(res: TurnResult): TurnResult =
    // На арене соперник ходит сам и только своим ходом: за него здесь не шагают.
    if (res.outcome != Outcome.Continue || !res.endsRound || res.battle.group.heroDown || res.battle.arena.isDefined) res
    else {
      // Сперва тот, кто в полях боя, делает свой шаг, потом смотрим, не дошёл ли
      // кто-нибудь до места напротив героя.
      val stepped = res.battle.stepActiveTowardsHero.getOrElse(res.battle)
      val paired  = stepped.engageArrived.getOrElse(stepped)
      if (paired.group.paired && !res.battle.group.paired)
        res.copy(battle = paired,
          sideLog = res.sideLog :+ content.format("battle.group.steps", "monster" -> paired.monsterName))
      else res.copy(battle = paired)
    }

  /** Базовая атака после умения, когда пары нет: по ближайшей досягаемой
    * цели, а если бить некого — ход кончается тихо. */
  private def followUp(hero: Hero, battle: SoloPveBattle, nowMs: Long, log: Vector[String], skip: Set[Long]): Task[TurnResult] =
    heroTargets(hero, battle).sortBy(p => math.abs(p - battle.group.heroPos)).headOption match {
      case None      => quietPhase(hero, battle, nowMs, log, skip)
      case Some(pos) => strikeSide(hero, battle, pos, nowMs, log, skip).flatMap(closeTurn(_, nowMs, skip))
    }

  /** Базовая атака игрока. `log` — уже накопленные строки хода (например, строка
    * применённого скилла, после которого идёт базовая атака). `skipSlots`
    * пробрасывается в `monsterPhase → tickBuffs`, чтобы только что использованный
    * слот не тикался в этом же ходу (см. SoloPveBattle.tickBuffs).
    */
  /** `isRepeat` — это переигранный промах от «Охотника» (порог 6). Такой заход
    * не повторяется ещё раз — не чаще раза за раунд. */
  private def playerStrike(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      log: Vector[String],
      skip: Set[Long],
      isRepeat: Boolean = false,
      // false — только удар, без ответа моба: вызывающий сам решит, кто отвечает
      // (удар по соседу возвращает героя в пару, и отвечает моб оттуда).
      retaliate: Boolean = true,
      // true — удар без промаха: броска попадания нет (Контрудар).
      sureHit: Boolean = false
  ): Task[TurnResult] =
    for {
      buffedEff <- ZIO.succeed(effWithAir(hero, battle, nowMs))
      hitRoll <- if (sureHit) ZIO.succeed(101) else Random.nextIntBetween(1, 101)
      mobDodge   = mobDodgeChance(buffedEff.accuracy, battle)
      // Попадание считается как hitRoll(1..100) > dodge, т.е. фактический шанс =
      // 100 - floor(dodge). Округляем ТАК ЖЕ, как экран боя (см. buildBattleScreen),
      // иначе при дробном dodge сообщение расходится с экраном на 1.
      heroHitPct = 100 - mobDodge.toInt
      result <-
        if (hitRoll > mobDodge) {
          for {
            spread <- Random.nextLongBetween(80L, 121L)
            // Без оружия — когда пусты обе руки.
            noWeapon = hero.equipment.unarmed
            weaponMod: Double = if (noWeapon) 0.5 else 1.0
            // Из лука вблизи — на пятую часть слабее (см. rangedCloseMod).
            closeMod = BattleState.rangedCloseMod(hero, battle)
            // «Разбойник» (+5%) множит итоговый урон. Усиление стихии сюда НЕ
            // входит: оно сдвигает грани урона по броне/HP в splitElementalDamage.
            // Пыль, севшая неудачно (всполох магии или четвёртая горсть), режет
            // весь урон героя на четверть — ровно этот бой (см. WeaponDust).
            // «Мерзавец» бьёт на 5% сильнее — и получает на 5% больше (см. mobStrike).
            damage =
              (((hero.effectiveBaseStats(nowMs).str * 3L + buffedEff.atk) * spread / 100L) *
                weaponMod * closeMod * hero.passives.finalDamageMult * hero.weaponDust.damageMult *
                Achievement.damagePct(hero) / 100.0).toLong
                .max(1L)
            // Стихии оружия модифицируют раздельно урон по броне и по HP
            // (см. splitElementalDamage). Без стихий поведение прежнее.
            // Сопротивление минибосса стихии оружия — до разбивки по броне/HP.
            resisted = (damage * bossResistance(hero, battle)).toLong.max(1L)
            // Защита моба режет удар ДО разбивки по броне/HP, иначе поехали бы
            // грани стихий (см. splitElementalDamage).
            // На арене соперник мог поднять щит своим ходом — он режет и удар героя.
            guarded = BattleState.shieldCut(battle.foeBlockPct,
                        (resisted * (1.0 - monsterDefenceCut(hero, battle, buffedEff, nowMs))).toLong.max(1L))
            // Предел босса за раунд: Крысиный король отдаёт не больше пятой
            // части запаса, сколько бы герой ни вложил в удар и умение.
            capped = capBossDamage(battle, guarded)
            (armorDmg, hpDmg) = splitElementalDamage(battle.monsterCurrentArmor, capped, hero)
            // В лог идёт то, что РЕАЛЬНО ушло в цель: сырое число сбивало с толку
            // там, где сопротивление режет удар в разы (каменного элементаля голая
            // сталь берёт на 20%, и «357 урона» превращались в 71 снятой брони).
            attackLine = content.format(
              "battle.hit",
              "damage"  -> (armorDmg + hpDmg).toString,
              "monster" -> battle.monsterName
            ) + capLine(guarded, capped).map("\n" + _).getOrElse("")
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
            // «Крыса» (порог 4): шанс, что удар по HP ещё и отравит. Бросок без
            // набора не тратится — детерминизм боевых тестов.
            setPoisons <- chanceRoll(
                            hero.sets.poisonOnHitChancePct > 0 && hpDmg > 0 && newHp > 0,
                            hero.sets.poisonOnHitChancePct)
            effectsRat =
              if (!setPoisons) effectsBled
              else effectsBled.copy(monsterPoison = Some(
                effectsBled.monsterPoison.map(p => Poison(p.pct + hero.sets.poisonPct))
                  .getOrElse(Poison(hero.sets.poisonPct))))
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
            effectsFed = if (!flaskBite) effectsRat else effectsRat.copy(heroVampiricHits = effectsRat.heroVampiricHits - 1)
            hitBattle = noteBossDamage(battle, armorDmg + hpDmg)
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
            log1 = log :+ attackLine
            log2 =
              if (poisonsNow || gemPoisons || coatPoisons || setPoisons)
                log1 :+ content.format("battle.poisonApplied", "monster" -> battle.monsterName)
              else log1
            // Сам вампиризм о себе не говорит: его число уезжает в конец раунда и
            // там складывается с чужой кровью в одну строку (см. vampLine). Если
            // раунд кончился раньше хода моба, строку дописываем здесь же.
            log4 = (log2 ++ elemLog) ++ Vector(thornsLine).filter(_.nonEmpty)
            fed  = log4 ++ vampLine(vampGained)
            r <-
              if (!retaliate) ZIO.succeed(TurnResult(thornedHero, thornedBattle, fed, Outcome.Continue))
              else if (thornedBattle.monsterCurrentHp <= 0) ZIO.succeed(victoryByHero(thornedHero, thornedBattle, fed, nowMs, skip))
              else if (thornedHero.fightStats.hp <= 0) ZIO.succeed(TurnResult(thornedHero, thornedBattle, fed, Outcome.Death))
              else respond(thornedHero, thornedBattle, nowMs, log4, skip, vampGained)
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
              // На арене соперник мог ждать промаха с парирующим кинжалом.
              else arenaCounter(hero, battle, nowMs).flatMap { case (h, b, lines) =>
                if (h.fightStats.hp <= 0) ZIO.succeed(TurnResult(h, b, missLog ++ lines, Outcome.Death))
                else respond(h, b, nowMs, missLog ++ lines, skip)
              }
            }
        }
    } yield result

  /** Контрудар соперника на арене: герой промахнулся, а соперник держал
    * парирующий кинжал наготове — и тут же бьёт в ответ наверняка. Сила удара —
    * его собственная, посчитанная при сборке зеркала (см.
    * ArenaBattle.counterPower); защита и броня героя режут её как обычный удар.
    * Вне арены силы нет — и броска тоже. */
  private def arenaCounter(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[(Hero, SoloPveBattle, Vector[String])] =
    if (battle.foeCounterPower <= 0L) ZIO.succeed((hero, battle, Vector.empty))
    else
      Random.nextLongBetween(80L, 121L).map { spread =>
        val raw       = (battle.monsterCounterPower * spread / 100L).max(1L)
        val reduction = heroDamageReduction(hero, battle, effWithAir(hero, battle, nowMs), battle.monsterStats.atk, nowMs)
        val damage    = (raw * (1.0 - reduction)).toLong.max(1L)
        val (newHp, newArmor) = MonsterSkill.applyPhysicalDamage(battle, hero, damage)
        (hero.copy(fightStats = hero.fightStats.copy(hp = newHp, armor = newArmor)),
         battle.copy(monsterCounterPower = 0L),
         Vector(content.format("battle.counterFoe", "monster" -> battle.monsterName, "damage" -> damage.toString)))
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

  /** «Упырь» (порог 12): каждый убитый враг — пир, и герой восстанавливает часть
    * HP и брони ТУТ ЖЕ, в бою, а не по его исходу. Считаем по числу павших за
    * ход: в группе их может лечь несколько, и за каждого положена своя доля.
    * Проценты считаются от эффективных потолков, восстановленное не может
    * опустить текущее значение (если оно почему-то выше потолка). */
  private def ghoulFeast(before: SoloPveBattle, res: TurnResult, nowMs: Long): TurnResult = {
    // Павшие в строю попадают в `slain`; тот, кто остался в полях с нулём HP
    // (последний в бою), в этот список не идёт — считаем его отдельно.
    val inLine = res.battle.group.slain.size - before.group.slain.size
    val active = if (res.battle.monsterCurrentHp <= 0L && before.monsterCurrentHp > 0L) 1 else 0
    val kills  = (inLine + active).max(0)
    if (kills <= 0 || !res.hero.sets.feastsOnKill) res
    else {
      val hero     = res.hero
      val maxHp    = hero.effectiveMaxHp(nowMs)
      val maxArmor = hero.effectiveMaxArmor(nowMs)
      val newHp    = (hero.fightStats.hp + maxHp * hero.sets.feastHpPct * kills / 100L).min(maxHp).max(hero.fightStats.hp)
      val newArmor = (hero.fightStats.armor + maxArmor * hero.sets.feastArmorPct * kills / 100L).min(maxArmor).max(hero.fightStats.armor)
      val hpGained    = newHp - hero.fightStats.hp
      val armorGained = newArmor - hero.fightStats.armor
      if (hpGained <= 0 && armorGained <= 0) res
      else res.copy(
        hero = hero.copy(fightStats = hero.fightStats.copy(hp = newHp, armor = newArmor)),
        log  = res.log :+ content.format("battle.ghoulFeast",
                 "hp" -> hpGained.toString, "armor" -> armorGained.toString))
    }
  }

  /** «Некромант» (порог 12): кого герой добил сам, тот может встать и пойти за
    * ним. Считаем так же, как пир упыря, — по разнице павших до и после хода, и
    * до хода союзников: их добычу некромант не поднимает.
    *
    * Восставший выходит в строй сразу, с [[ItemSet.Necromancer.RiseHpPct]]
    * процентами HP и без брони (её сбили, пока он был жив), а после боя уходит с
    * героем на двое суток. Статы — как у поднятого с алтаря, по тому же трофею, и
    * имя своё: названный врагом остаётся названным.
    *
    * Не поднимаются те, в кого поднимать нечего: элементали — чистая стихия,
    * сооружения — железо, Гнилой Джо и так нежить, а Крысиный король — не тело,
    * а сборище крыс. Белый волк встаёт как по клыку. */
  private def necroRise(before: SoloPveBattle, res: TurnResult): Task[TurnResult] =
    // На арене павшие не встают за героем: соперник и его отряд — не добыча.
    if (res.hero.sets.riseChancePct <= 0L || res.battle.arena.isDefined) ZIO.succeed(res)
    else {
      val fresh  = res.battle.group.slain.drop(before.group.slain.size)
      val active = Option.when(res.battle.monsterCurrentHp <= 0L && before.monsterCurrentHp > 0L)(
        before.slainActive -> before.boss)
      val fallen = fresh.map(_ -> Option.empty[MiniBoss]) ++ active.toList
      ZIO.foldLeft(fallen)(res) { case (acc, (slain, boss)) =>
        BattleState.riseForm(slain, boss) match {
          case None       => ZIO.succeed(acc)
          case Some(form) =>
            chanceRoll(true, acc.hero.sets.riseChancePct).map {
              case false => acc
              case true  =>
                acc.battle.group.nearestFreeAllyPlace.fold(acc) { pos =>
                  val hp = (form.stats.hp * ItemSet.Necromancer.RiseHpPct / 100L).max(1L)
                  val ally = BattleAlly(pangea.model.squad.AllyKind.Undead, pos,
                    hp = hp, armor = 0L, energy = form.stats.energy,
                    stats = form.stats, lvl = form.lvl, undead = Some(form), home = 0)
                  acc.copy(
                    battle = acc.battle.copy(group = acc.battle.group.copy(
                      allies = acc.battle.group.allies :+ ally)),
                    log = acc.log :+ content.format("battle.necroRise",
                      "name" -> form.name, "n" -> pos.toString))
                }
            }
        }
      }
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
  private def bossHit(battle: SoloPveBattle, hero: Hero, damage0: Long): (Long, Long) = {
    // «Некромант» (порог 10): мёртвая плоть занимается охотнее — огненный удар
    // по ней весомее. Прочие стихии мертвецу всё равно.
    val damage =
      if (powderElement(battle).contains(Element.Fire)) (damage0 * hero.sets.fireTakenMult).toLong.max(1L)
      else damage0
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
        // Пламя шипов по герою под холодом: лёд тает, огонь не занимается.
        val thawed = battle.effects.coldOnHero
        val burned =
          if (thawed) battle.copy(effects = battle.effects.copy(heroColdDefenceCut = 0))
          else battle.copy(effects = battle.effects.copy(heroBurn = Some(
            battle.effects.heroBurn.map(_.reignited).getOrElse(Burn(MiniBoss.FireElemental.ThornsBurnPct)))))
        (hurt, burned, content.format("battle.elemental.thorns", "damage" -> thorns.toString) +
          (if (thawed) "\n" + content.text("battle.elements.thawHero") else ""))
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
      nowMs: Long,
      victim: Victim
  ): Task[(SoloPveBattle, Hero, String)] =
    battle.boss match {
      case Some(MiniBoss.StoneElemental) => stoneTurnCast(hero, battle, nowMs, victim)
      case Some(MiniBoss.RottenJoe)      => joeTurnCast(hero, battle, nowMs, victim)
      case Some(MiniBoss.WhiteWolf)      => wolfTurnCast(hero, battle, nowMs, victim)
      case Some(MiniBoss.RatKing)        => ratKingTurnCast(hero, battle)
      case Some(MiniBoss.Necromancer)    => necromancerTurnCast(hero, battle)
      case _                             => fireTurnCast(hero, battle, nowMs, victim)
    }

  /** Сколько сейчас HP у того, по кому бьёт приём, — чтобы понять, дошло ли до
    * тела (кровь и травмы вешают только тогда). */
  private def victimHp(v: Victim, hero: Hero, battle: SoloPveBattle): Long = v match {
    case Victim.Hero      => hero.fightStats.hp
    case Victim.Ally(pos) => battle.group.allyAt(pos).map(_.hp).getOrElse(0L)
  }

  /** Сколько тому, по кому бьёт приём, не хватает до полного здоровья. */
  private def victimMissingHp(v: Victim, hero: Hero, battle: SoloPveBattle, nowMs: Long): Long = v match {
    case Victim.Hero      => (hero.effectiveMaxHp(nowMs) - hero.fightStats.hp).max(0L)
    case Victim.Ally(pos) => battle.group.allyAt(pos).map(a => (a.stats.hp - a.hp).max(0L)).getOrElse(0L)
  }

  /** Круг Некроманта: воскрешение → мистический шаг → созидание.
    *
    * Воскрешение поднимает ВСЮ его павшую нежить — целой, с полным HP и бронёй,
    * — и забирает её из добычи: за одного и того же мертвеца дважды не платят
    * (так же, как за крыс, съеденных королём). Шаг уводит его за спины своих —
    * на ближайшее свободное место, перед которым стоит его мертвец, и никогда
    * не ближе к герою. Он и так за спинами своих — стоит, где стоял, молча;
    * прятаться не за кем или некуда — пропуск со словами. Шаг на месте ничего
    * не стоит. Созидание поднимает нового мертвеца той же расы, что пещера,
    * третьего или четвёртого ранга.
    *
    * Не хватило энергии — способность не применяется, но очередь сдвигается. */
  private def necromancerTurnCast(
      hero: Hero,
      battle: SoloPveBattle
  ): Task[(SoloPveBattle, Hero, String)] = {
    val necro  = MiniBoss.Necromancer
    val lvl    = battle.monsterLvl
    val energy = battle.monsterCurrentEnergy
    val next   = nextTurn(battle, necro)

    def spend(cost: Long, b: SoloPveBattle): SoloPveBattle = b.copy(monsterCurrentEnergy = energy - cost)

    battle.bossTurn match {
      // 1) Воскрешение: все его павшие встают целыми и уходят из добычи.
      case 0 =>
        val cost  = necro.ReviveCostPerLvl * lvl
        val fallen = battle.group.slain.filter(s => s.undead || s.race == Race.Undead.entryName)
        if (energy < cost || fallen.isEmpty) ZIO.succeed((next, hero, ""))
        else {
          val raised = fallen.foldLeft(spend(cost, next)) { (b, s) =>
            val m = MonsterGenerator.generateOfRaceAndRarity(
              s.lvl.toInt, Race.withName(s.race), Rarity.withName(s.rarity))
            val slot = MonsterSlot(s.lvl, s.race, s.rarity, m.fightStats, m.fightStats.hp,
              m.fightStats.armor, s.marked, 0L, BattleEffects.empty, undead = true)
            b.admit(slot)
          }
          // Из добычи они уходят: поднятый второй раз платы не стоит.
          val cleared = raised.copy(group = raised.group.copy(
            slain = raised.group.slain.filterNot(s => s.undead || s.race == Race.Undead.entryName)))
          ZIO.succeed((cleared, hero, content.format("battle.necro.revive", "n" -> fallen.size.toString)))
        }

      // 2) Мистический шаг: за спины своих. Кто и так за ними, тот не уходит.
      case 1 =>
        val cost = necro.StepCostPerLvl * lvl
        if (battle.necromancerCovered) ZIO.succeed((next, hero, ""))
        else if (battle.effects.monsterRooted) ZIO.succeed((next, hero, content.text("battle.necro.pinned")))
        else battle.necromancerHideaway match {
          case None       => ZIO.succeed((next, hero, content.text("battle.necro.noHide")))
          case Some(_) if energy < cost => ZIO.succeed((next, hero, ""))
          case Some(pos)  =>
            val moved = spend(cost, next).stepActiveTo(pos)
            ZIO.succeed((moved, hero, content.format("battle.necro.step", "n" -> pos.toString)))
        }

      // 3) Созидание: поднимает нового мертвеца.
      case _ =>
        val cost = necro.CreateCostPerLvl * lvl
        if (energy < cost || !battle.hasRoom) ZIO.succeed((next, hero, ""))
        else
          for {
            pick   <- Random.nextIntBounded(necro.CreateRarities.size)
            rarity  = necro.CreateRarities(pick)
            // Поднимает он смертных — тех, кто в этой пещере жил. Боссовая раса
            // сюда не годится: мобов такой расы генератор не знает.
            race    = battle.group.originRace.flatMap(Race.withNameOption)
                        .filter(Race.mortals.contains)
                        .orElse(battle.group.others.flatMap(s => Race.withNameOption(s.race))
                                  .find(Race.mortals.contains))
                        .getOrElse(Race.Orc)
            pct    <- Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
            m       = MonsterGenerator.generateOfRaceAndRarity(lvl.toInt, race, rarity)
            slot    = MonsterSlot(m.lvl, race.entryName, rarity.entryName, m.fightStats,
                        m.fightStats.hp, m.fightStats.armor, m.marked,
                        MonsterEnergy.startEnergy(m.lvl, rarity, pct), BattleEffects.empty, undead = true)
            joined  = spend(cost, next).admit(slot)
          } yield (joined, hero, content.format("battle.necro.create", "name" -> slot.name))
    }
  }

  /** Круг Крысиного короля: призыв → пропуск → объединение. Ход он берёт раз в
    * раунд, поэтому здесь же обнуляется счётчик урона, который герой успел по
    * нему положить (см. [[capBossDamage]]). */
  private def ratKingTurnCast(
      hero: Hero,
      battle: SoloPveBattle
  ): Task[(SoloPveBattle, Hero, String)] = {
    val king   = MiniBoss.RatKing
    val lvl    = battle.monsterLvl
    val energy = battle.monsterCurrentEnergy
    val next   = nextTurn(battle, king).copy(bossTaken = 0L)

    def spend(cost: Long, b: SoloPveBattle): SoloPveBattle = b.copy(monsterCurrentEnergy = energy - cost)

    battle.bossTurn match {
      // 1) Призыв крыс: из труб лезут ещё две-три, уровнем в саму канализацию.
      case 0 =>
        val cost = king.SummonCostPerLvl * lvl
        if (energy < cost || !battle.hasRoom) ZIO.succeed((next, hero, ""))
        else
          for {
            rolled <- Random.nextIntBetween(king.SummonMin, king.SummonMax + 1)
            // Больше, чем влезет в строй, он не зовёт: очередь за спинами
            // копилась бы весь бой и вываливалась разом.
            n      = rolled.min(battle.group.freePlaces.size).max(1)
            seeds <- ZIO.foreach(List.fill(n)(()))(_ => Random.nextLong)
            pcts  <- ZIO.foreach(seeds)(_ =>
                       Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L))
            ratLvl = battle.minionLvl.max(1L).toInt
            slots  = seeds.zip(pcts).map { case (seed, pct) =>
                       val (rarity, _) = CaveGenerator.rollQuestRarity(Rng(seed))
                       val m = MonsterGenerator.generateOfRaceAndRarity(ratLvl, Race.Animal, rarity)
                       MonsterSlot(m.lvl, m.race.entryName, m.rarity.entryName, m.fightStats,
                         m.fightStats.hp, m.fightStats.armor, m.marked,
                         MonsterEnergy.startEnergy(m.lvl, m.rarity, pct), BattleEffects.empty)
                     }
            joined = slots.foldLeft(spend(cost, next))((b, s) => b.admit(s))
          } yield (joined, hero, content.text("battle.ratKing.summon"))

      // 2) Пропуск хода.
      case 1 => ZIO.succeed((next, hero, ""))

      // 3) Объединение: он поглощает своего — слабейшего из тех, кто рядом.
      case _ =>
        val cost = king.MergeCostPerLvl * lvl
        val prey = battle.group.others.zipWithIndex.filter(_._1.alive).sortBy(_._1.currentHp).headOption
        prey match {
          case Some((slot, idx)) if energy >= cost =>
            val grown = spend(cost, next).copy(
              monsterStats = battle.monsterStats.copy(
                hp    = battle.monsterStats.hp + slot.stats.hp,
                armor = battle.monsterStats.armor + slot.stats.armor,
                atk   = battle.monsterStats.atk + king.MergeAtkPerLvl * lvl),
              monsterCurrentHp    = battle.monsterCurrentHp + slot.stats.hp,
              monsterCurrentArmor = battle.monsterCurrentArmor + slot.stats.armor,
              // Съеденная в павшие не идёт: её не убивали, и добычи за неё нет.
              group = battle.group.devour(idx))
            ZIO.succeed((grown, hero, content.text("battle.ratKing.merge")))
          case _ => ZIO.succeed((next, hero, ""))
        }
    }
  }

  /** Сколько из этого удара дойдёт до минибосса с пределом: Крысиный король —
    * ком из крыс и разваливается по одной, больше
    * [[MiniBoss.roundDamageCapPct]] % своего максимума HP за раунд он не теряет.
    * Предел общий на удар с руки и умение: счётчик обнуляется, когда босс берёт
    * свой ход. Возвращает урезанный урон и бой с подросшим счётчиком. */
  private def capBossDamage(battle: SoloPveBattle, damage: Long): Long =
    battle.boss.map(_.roundDamageCapPct).filter(_ > 0L) match {
      case None      => damage
      case Some(pct) =>
        val cap = (battle.monsterStats.hp * pct / 100L).max(1L)
        damage.min((cap - battle.bossTaken).max(0L))
    }

  /** Записать в счёт раунда то, что по боссу с пределом реально прошло. */
  private def noteBossDamage(battle: SoloPveBattle, dealt: Long): SoloPveBattle =
    if (battle.boss.exists(_.roundDamageCapPct > 0L)) battle.copy(bossTaken = battle.bossTaken + dealt)
    else battle

  /** Строка о том, что предел сработал: ком потерял очередную крысу. */
  private def capLine(before: Long, after: Long): Option[String] =
    Option.when(after < before)(content.text("battle.ratKing.capped"))

  /** Круг огненного: всплеск → сфера → щит → пропуск. */
  private def fireTurnCast(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      victim: Victim
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
          val raw        = (atk * MiniBoss.FireElemental.SplashDamageFactor).toLong.max(1L)
          val (h, b1, l) = hurtVictim(victim, hero, spend(cost, next), raw)
          // Пламя не складывается, а разгорается заново — и на герое, и на том,
          // кто стоит перед элементалем вместо него.
          val (burned, thawed) = reigniteVictim(victim, b1, MiniBoss.FireElemental.SplashBurnPct)
          ZIO.succeed((burned, h,
            content.format("battle.elemental.fireSplash", "damage" -> l.toString) +
              (if (thawed) "\n" + thawLine(victim, b1) else "")))
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
          else chargeBurst(hero, spend(cost, next).copy(bossCharges = 0), atk, nowMs, MiniBoss.FireElemental, victim)
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
      kind: MiniBoss,
      victim: Victim
  ): Task[(SoloPveBattle, Hero, String)] = {
    val stone    = kind == MiniBoss.StoneElemental
    val statPct  = if (stone) MiniBoss.StoneElemental.BurstHeroStatPct else MiniBoss.FireElemental.BurstHeroStatPct
    val traumaAt = if (stone) MiniBoss.StoneElemental.BurstTraumaChancePct else MiniBoss.FireElemental.BurstTraumaChancePct
    val textKey  = if (stone) "battle.elemental.boulderBurst" else "battle.elemental.orbBurst"
    // Залп считается от запаса того, в кого летит: у союзника свои потолки.
    val (maxHp, maxArmor) = victim match {
      case Victim.Hero      => (hero.effectiveMaxHp(nowMs), hero.effectiveMaxArmor(nowMs))
      case Victim.Ally(pos) => battle.group.allyAt(pos).fold((0L, 0L))(a => (a.stats.hp, a.stats.armor))
    }
    val raw      = atk * 2L + (maxHp * statPct / 100L) + (maxArmor * statPct / 100L)
    val hpBefore = victimHp(victim, hero, battle)
    val (hit, struck, damage) = hurtVictim(victim, hero, battle, raw)
    val toHp     = (hpBefore - victimHp(victim, hit, struck)).max(0L)
    // Каменная россыпь сбивает с ног: защита и уклонение героя срезаны на 3 хода.
    // Союзника сбивать нечем — у него таких счётчиков нет, ему достаётся только удар.
    val after =
      if (!stone || victim != Victim.Hero) struck
      else struck.copy(effects = struck.effects.copy(heroStunnedTurns = MiniBoss.StoneElemental.BurstDebuffTurns))
    val burstLine = content.format(textKey, "damage" -> damage.toString)
    val line =
      if (!stone || victim != Victim.Hero) burstLine
      else burstLine + "\n" + content.format("battle.elemental.boulderStun",
             "pct" -> MiniBoss.StoneElemental.BurstDebuffPct.toString,
             "turns" -> MiniBoss.StoneElemental.BurstDebuffTurns.toString)
    // Травма — геройская беда: союзник её не получает.
    if (toHp <= 0 || victim != Victim.Hero || hit.fightStats.hp <= 0) ZIO.succeed((after, hit, line))
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
      nowMs: Long,
      victim: Victim
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
          ZIO.succeed((poisonVictim(victim, spend(cost, next), joe.StenchPoisonPct),
            hero, content.text("battle.joe.stench")))
        }

      // 2) Широкий удар: три четверти атаки, и с шансом 5% — травма, если дошло до HP.
      case 1 =>
        val cost = joe.SweepCostPerLvl * lvl
        if (energy < cost) ZIO.succeed((next, hero, ""))
        else {
          val raw      = (monsterAttack(battle) * joe.SweepDamageFactor).toLong.max(1L)
          val hpBefore = victimHp(victim, hero, battle)
          val (hurt, spent, dealt) = hurtVictim(victim, hero, spend(cost, next), raw)
          val line     = content.format("battle.joe.sweep", "damage" -> dealt.toString)
          // Травму оставляет только удар по герою: союзник ран не копит.
          if (victim != Victim.Hero || victimHp(victim, hurt, spent) >= hpBefore || hurt.fightStats.hp <= 0)
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
      nowMs: Long,
      victim: Victim
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
        val hpBefore = victimHp(victim, hero, b1)
        val (hurt, bitten, damage) = victim match {
          case Victim.Hero =>
            // По герою волк бьёт холодом: грани стихии считает `bossHit`.
            val dealt             = bossDamageTaken(hero, battle, (raw * mult).max(1L))
            val (newHp, newArmor) = bossHit(battle, hero, dealt)
            (hero.copy(fightStats = hero.fightStats.copy(hp = newHp, armor = newArmor)), b1, dealt)
          case ally => hurtVictim(ally, hero, b1, (raw * mult).max(1L))
        }
        val bleeds = bleedPct > 0 && victimHp(victim, hurt, bitten) < hpBefore
        val b2     = if (!bleeds) bitten else bleedVictim(victim, bitten, bleedPct)
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
        val missing = victimMissingHp(victim, hero, battle, nowMs)
        val raw     = missing * wolf.JawsMissingHpPct / 100L + (atk * wolf.JawsDamageFactor).toLong
        bite(next, raw.max(1L), 0, "battle.wolf.jaws")
    }
  }

  /** Круг каменного: всплеск → валун → восстановление → вязкая земля → пропуск. */
  private def stoneTurnCast(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      victim: Victim
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
          val raw        = (atk * MiniBoss.StoneElemental.SplashDamageFactor).toLong.max(1L)
          val (h, b1, l) = hurtVictim(victim, hero, spend(cost, next), raw)
          ZIO.succeed((b1, h,
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
          else chargeBurst(hero, spend(cost, next).copy(bossCharges = 0), atk, nowMs, MiniBoss.StoneElemental, victim)
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
    // (см. Bleed): рана затянута. Яд оно ослабляет, а пламя тратит собой:
    // горение до 50% сгорает целиком, сверх того остаётся гореть излишек
    // (см. HealedWounds). Из-под миазм тьмы лечение не выводит.
    val fx     = battle0.effects
    val healed = HealedWounds.of(raw, fx.heroPoison, fx.heroBurn)
    val newHp  = (hero.fightStats.hp + healed.amount).min(hero.effectiveMaxHp(nowMs))
    val battle = battle0.copy(effects = fx.copy(heroBleed = None, heroPoison = healed.poison,
      heroBurn = healed.burn))
    val burnLine = healed.burnCutPct.map(cut => healed.burn match {
      case None    => content.format("battle.burnEatsHealOut", "pct" -> cut.toString)
      case Some(b) => content.format("battle.burnEatsHeal", "pct" -> cut.toString, "left" -> b.pct.toString)
    })
    val poisonLine = fx.heroPoison.map(_ => healed.poison match {
      case None    => content.text("battle.healCuresPoison")
      case Some(p) => content.format("battle.healEasesPoison", "left" -> p.pct.toString)
    })
    val lines = List(burnLine, poisonLine).flatten
    (newHp, newHp - hero.fightStats.hp, battle, Option.when(lines.nonEmpty)(lines.mkString("\n")))
  }

  /** Строки о том, что лечение героя сделало с ранами союзника: пламя съело
    * часть лечения, яд ослаб или вышел. Кровь уходит молча — как и у героя. */
  private def allyWoundLines(a: BattleAlly, w: HealedWounds): Vector[String] =
    Vector(
      w.burnCutPct.map(cut => w.burn match {
        case None    => content.format("battle.group.allyBurnEatsHealOut", "name" -> a.name, "pct" -> cut.toString)
        case Some(b) => content.format("battle.group.allyBurnEatsHeal",
                          "name" -> a.name, "pct" -> cut.toString, "left" -> b.pct.toString)
      }),
      a.poison.map(_ => w.poison match {
        case None    => content.format("battle.group.allyHealCuresPoison", "name" -> a.name)
        case Some(p) => content.format("battle.group.allyHealEasesPoison", "name" -> a.name, "left" -> p.pct.toString)
      })
    ).flatten

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

  /** Стихия, которой бьёт моб. У минибосса она своя (Белый волк — холодом), у
    * прочих — расовая: каджит всегда с ветром, демон с огнём, гном с холодом,
    * человек и гоблин с молнией (см. [[Race.weaponElement]]). */
  private def powderElement(battle: SoloPveBattle): Option[Element] =
    battle.boss.flatMap(_.attackElement)
      .orElse(Race.withNameOption(battle.monsterRace).flatMap(_.weaponElement))

  /** Доли урона по броне и HP от стихии удара моба. У каменного элементаля
    * свой раскол, он важнее — там это природа удара, а не стихия. */
  private def powderSplit(battle: SoloPveBattle): Option[(Double, Double)] =
    powderElement(battle).map(e => (e.armorMult, e.hpMult))

  /** «Каменный страж» (порог 4): стихийный урон по герою слабее на 20%.
    * Стихийным считается ВЕСЬ урон элементаля — он не бьёт «обычной атакой», он
    * и есть огонь: и удар, и всплеск, и смерч, и шипы, и горение. У прочих мобов
    * стихийного урона нет, поэтому им порог ничего не режет.
    *
    * Огонь по герою-нежити («Некромант», порог 10) наоборот весомее — весь урон
    * огненного элементаля и есть огонь. */
  private def bossDamageTaken(hero: Hero, battle: SoloPveBattle, damage: Long): Long =
    if (battle.boss.isEmpty || damage <= 0L) damage
    else {
      val fire = if (battle.boss.contains(MiniBoss.FireElemental)) hero.sets.fireTakenMult else 1.0
      (damage * hero.sets.elementalDamageTakenMult * fire).toLong.max(1L)
    }

  /** Урон по цели приёма. Герою — привычным путём ([[hurtHero]]) с поправкой на
    * его наборы; союзнику — по его защите и броне, как при обычном ударе моба.
    * Возвращает героя, бой и сколько урона прошло (для строки лога). */
  private def hurtVictim(v: Victim, hero: Hero, battle: SoloPveBattle, damage: Long): (Hero, SoloPveBattle, Long) =
    v match {
      case Victim.Hero =>
        // Приём босса — тоже умение: поднятый щит режет и его.
        val (hurt, dealt) = hurtHero(hero, bossDamageTaken(hero, battle, BattleState.shieldCut(battle.heroBlockPct, damage)))
        (hurt, battle, dealt)
      case Victim.Ally(pos) =>
        battle.group.allyAt(pos).filter(_.alive) match {
          case None => (hero, battle, 0L)
          case Some(a) =>
            val reduced  = (damage * (1.0 - allyReduction(a, battle.monsterStats.atk))).toLong.max(1L)
            val toArmor  = reduced.min(a.armor)
            val toHp     = reduced - toArmor
            val hurt     = a.copy(armor = a.armor - toArmor, hp = (a.hp - toHp).max(0L))
            (hero, battle.copy(group = battle.group.updateAlly(pos)(_ => hurt)), reduced)
        }
    }

  /** Повесить рану на цель приёма: герою — в эффекты боя, союзнику — на него
    * самого. Яд копится, как и у героя: повторный приём складывает проценты. */
  private def poisonVictim(v: Victim, battle: SoloPveBattle, pct: Int): SoloPveBattle =
    v match {
      case Victim.Hero =>
        val grown = battle.effects.heroPoison.map(p => Poison(p.pct + pct)).getOrElse(Poison(pct))
        battle.copy(effects = battle.effects.copy(heroPoison = Some(grown)))
      case Victim.Ally(pos) =>
        battle.copy(group = battle.group.updateAlly(pos)(a =>
          a.copy(poison = Some(a.poison.map(p => Poison(p.pct + pct)).getOrElse(Poison(pct))))))
    }

  private def bleedVictim(v: Victim, battle: SoloPveBattle, pct: Int): SoloPveBattle =
    v match {
      case Victim.Hero =>
        val grown = battle.effects.heroBleed.map(b => Bleed(b.pct + pct)).getOrElse(Bleed(pct))
        battle.copy(effects = battle.effects.copy(heroBleed = Some(grown)))
      case Victim.Ally(pos) =>
        battle.copy(group = battle.group.updateAlly(pos)(a =>
          a.copy(bleed = Some(a.bleed.map(b => Bleed(b.pct + pct)).getOrElse(Bleed(pct))))))
    }

  /** Поджечь цель: уже горящее пламя разгорается заново, непогашенное — зажигается.
    * Герой под холодом не загорается: лёд тает (второе в ответе — так и вышло). */
  private def reigniteVictim(v: Victim, battle: SoloPveBattle, pct: Int): (SoloPveBattle, Boolean) =
    v match {
      case Victim.Hero if battle.effects.coldOnHero =>
        (battle.copy(effects = battle.effects.copy(heroColdDefenceCut = 0)), true)
      case Victim.Hero =>
        val fire = battle.effects.heroBurn.map(_.reignited).getOrElse(Burn(pct))
        (battle.copy(effects = battle.effects.copy(heroBurn = Some(fire))), false)
      // Огонь по замёрзшему союзнику растапливает лёд и не занимается — как и на герое.
      case Victim.Ally(pos) if battle.group.allyAt(pos).exists(_.cold) =>
        (battle.copy(group = battle.group.updateAlly(pos)(_.copy(coldCut = 0))), true)
      case Victim.Ally(pos) =>
        (battle.copy(group = battle.group.updateAlly(pos)(a =>
          a.copy(burn = Some(a.burn.map(_.reignited).getOrElse(Burn(pct)))))), false)
    }

  /** Строка о том, что пламя растопило лёд на цели приёма. */
  private def thawLine(v: Victim, battle: SoloPveBattle): String = v match {
    case Victim.Hero      => content.text("battle.elements.thawHero")
    case Victim.Ally(pos) => battle.group.allyAt(pos).fold("")(a => content.format("battle.ally.thaw", "name" -> a.name))
  }

  /** Насколько защита союзника режет удар моба. Холод на нём её обгрызает — как
    * у героя (см. [[heroDamageReduction]]). */
  private def allyReduction(a: BattleAlly, monsterAtk: Long): Double =
    (BattleState.damageReduction(a.stats.defence, 0L, monsterAtk, 0L) - a.coldCut / 100.0).max(0.0)

  /** Куда уходит удар моба по союзнику — как по герою ([[bossHit]]): у
    * минибосса с расколом (каменный) доли в броню и мимо неё в HP, у прочих —
    * сперва броня, остаток в HP, с гранями стихии удара. Урон по броне и по HP. */
  private def allyHitSplit(tmp: SoloPveBattle, a: BattleAlly, damage: Long): (Long, Long) =
    tmp.boss.flatMap(_.heroHitSplit) match {
      case Some((armorPart, hpPart)) =>
        val absorbed = (damage * armorPart).toLong.min(a.armor)
        (absorbed, (damage - absorbed).max((damage * hpPart).toLong))
      case None =>
        val (armorMult, hpMult) = powderSplit(tmp).getOrElse((1.0, 1.0))
        val rawArmor = math.min(a.armor, damage)
        ((rawArmor * armorMult).toLong.min(a.armor).max(0L), ((damage - rawArmor) * hpMult).toLong.max(0L))
    }

  /** Тик ран союзников в конце раунда — те же три, что у героя и у мобов: яд
    * слабеет, кровь держится, пламя растёт, и бьют они мимо брони. Поднятого с
    * алтаря ни яд, ни кровь не берут, зато горит он жарче живого. Павшего от
    * ран уносит свиток, а поднятый рассыпается там же, где стоял. */
  private def tickAllyEffects(res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Continue || !res.endsRound) res
    else {
      val (ticked, lines) = res.battle.group.allies.foldLeft((List.empty[BattleAlly], Vector.empty[String])) {
        case ((acc, log), a) if !a.alive || (a.poison.isEmpty && a.bleed.isEmpty && a.burn.isEmpty) =>
          (acc :+ a, log)
        case ((acc, log), a0) =>
          var ally  = a0
          var out   = log
          val maxHp = a0.stats.hp
          if (a0.isUndead && (a0.poison.isDefined || a0.bleed.isDefined)) {
            ally = ally.copy(poison = None, bleed = None)
            out  = out :+ content.format("battle.ally.undeadImmune", "name" -> a0.name)
          }
          ally.poison.foreach { p =>
            val dmg = p.damageOn(maxHp)
            ally = ally.copy(hp = (ally.hp - dmg).max(0L), poison = p.decayed)
            out   = out :+ allyDotTick(Poison.Icon, Poison.Label, dmg, a0.name)
          }
          ally.bleed.foreach { b =>
            val dmg = b.damageOn(maxHp)
            ally = ally.copy(hp = (ally.hp - dmg).max(0L))
            out   = out :+ allyDotTick(Bleed.Icon, Bleed.Label, dmg, a0.name)
          }
          ally.burn.foreach { bn =>
            val fire  = if (a0.isUndead) ItemSet.Necromancer.UndeadFireTakenPct else 0L
            val dmg   = bn.damageOn(maxHp) * (100L + fire) / 100L
            val steps = if (a0.isUndead) ItemSet.Necromancer.UndeadBurnGrowthMult else 1L
            ally = ally.copy(hp = (ally.hp - dmg).max(0L),
                             burn = Some((1L to steps).foldLeft(bn)((b, _) => b.grown)))
            out   = out :+ allyDotTick(Burn.Icon, Burn.Label, dmg, a0.name)
          }
          (acc :+ ally, out)
      }
      val fallen  = ticked.filter(a => !a.alive && res.battle.group.allyAt(a.position).exists(_.alive))
      val updated = res.battle.copy(group = res.battle.group.copy(allies = ticked))
      val swept   = fallen.foldLeft(updated)((b, a) => b.copy(group = b.group.withoutAlly(a.position)))
      val gone    = fallen.map(a => content.format(
        if (a.undead.isDefined) "battle.ally.crumbled" else "battle.ally.scroll", "name" -> a.name))
      res.copy(battle = swept, log = res.log ++ lines ++ gone)
    }

  /** Строка тика раны на союзнике — та же, что для мобов, только имя его. */
  private def allyDotTick(icon: String, label: String, damage: Long, name: String): String =
    content.format("battle.ally.dotTick", "icon" -> icon, "dot" -> label,
      "damage" -> damage.toString, "name" -> name)

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
    // Огонь и холод гасят друг друга. Сработали оба в одном ударе — ни пламя, ни
    // мороз на цель не ложатся, а что на ней было, то и остаётся. Поодиночке:
    // холод по горящему гасит огонь и сам не ложится, огонь по замёрзшему
    // растапливает лёд и сам не занимается.
    val steam      = fired(Element.Fire) && fired(Element.Cold)

    var b   = battle
    var log = Vector.empty[String]
    if (steam) log = log :+ content.text("battle.elements.steam")

    // Огонь по цели: под холодом — лёд тает и пламя не занимается.
    def thaw(): Unit = {
      b = b.copy(effects = b.effects.copy(monsterColdDefenceCut = 0, chilledTurns = 0))
      log = log :+ content.format("battle.elements.thaw", "monster" -> b.monsterName)
    }

    // Комбо Огонь+Воздух: мгновенный урон 30% макс.HP + текущий % горения, горение → 2%.
    if (fireAir) {
      val burnPct = b.effects.monsterBurn.map(_.pct).getOrElse(0)
      val dmg     = (maxHp.toDouble * (30 + burnPct) / 100.0).toLong.max(0L)
      b = b.copy(monsterCurrentHp = (b.monsterCurrentHp - dmg).max(0L))
      log = log :+ content.format("battle.comboFireAir", "damage" -> dmg.toString)
      // Горение после взрыва — по тем же правилам огня и холода.
      if (!steam) {
        if (b.effects.coldOnMonster) thaw()
        else b = b.copy(effects = b.effects.copy(monsterBurn = Some(Burn(Burn.Initial))))
      }
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
    // Холод по горящему: пламя гаснет, мороз не ложится.
    if (fired(Element.Cold) && !steam && b.effects.monsterBurn.isDefined) {
      b = b.copy(effects = b.effects.copy(monsterBurn = None))
      log = log :+ content.format("battle.elements.quench", "monster" -> b.monsterName)
    }
    else if (fired(Element.Cold) && !steam) {
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
    if (fired(Element.Fire) && !fireAir && !steam && b.effects.coldOnMonster) thaw()
    else if (fired(Element.Fire) && !fireAir && !steam) {
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

  /** Строка тика DoT в конце раунда. Одна на все три — яд, кровотечение и
    * пламя: меняется только значок с именем эффекта. Больше их нигде в логе не
    * поминают, чтобы одно и то же число не попадалось игроку трижды за раунд;
    * сколько снимется за ход, видно в шапке рядом с HP. */
  private def dotTick(icon: String, label: String, damage: Long, monster: String): String =
    content.format("battle.dotTick", "icon" -> icon, "dot" -> label,
      "damage" -> damage.toString, "monster" -> monster)

  /** То же в сторону ГЕРОЯ: имя моба тут ни при чём. */
  private def heroDotTick(icon: String, label: String, damage: Long): String =
    content.format("battle.heroDotTick", "icon" -> icon, "dot" -> label, "damage" -> damage.toString)

  /** Вампиризм за раунд — одной строкой, как реген, и не важно, откуда он пришёл:
    * череп в оружии, «Упырь» (порог 4), вампирская фляга или чужая кровь (порог
    * 10). Пусто, если пить было нечего. */
  private def vampLine(healed: Long): Vector[String] =
    if (healed > 0L) Vector(content.format("battle.vampirism", "healed" -> healed.toString))
    else Vector.empty

  // ── Ход монстра ─────────────────────────────────────────────────────────────

  /** Ход моба после атаки игрока. `log` — строки, накопленные в фазе игрока;
    * между сегментами игрока и монстра вставляется пустая строка-разделитель
    * (в склейке `mkString("\n")` даёт двойной перенос).
    */
  /** Что случилось с героем от обычной атаки моба: новые HP и броня, сам урон,
    * побочные строки (Непробиваемый, Крепкость, шипы, поджог, яд, проки
    * порошка) и бой после удара (шипы, порошок). Строку «наносит N урона»
    * собирает вызывающий — у активного моба и у бьющего сбоку она разная. */
  private def mobStrike(hero: Hero, battle: SoloPveBattle, buffedEff: FightStats, nowMs: Long): Task[MobStrike] =
    // Зеркальный настой: пока копии целы, удар достаётся им, а не герою.
    if (battle.effects.heroMirrors > 0) {
      val left = battle.effects.heroMirrors - 1
      ZIO.succeed(MobStrike(
        newHp      = hero.fightStats.hp,
        newArmor   = hero.fightStats.armor,
        damage     = 0L,
        extraLines = List(content.format("battle.mirrorTook",
                       "monster" -> battle.monsterName, "left" -> left.toString)),
        battle     = battle.copy(effects = battle.effects.copy(heroMirrors = left)),
        mirrored   = true))
    } else
      // Отвести обычный удар целиком: механика готова, но ставку пока никто не
      // даёт (см. HeroSets.blockAttackChancePct) — броска при нуле нет.
      chanceRoll(hero.sets.blockAttackChancePct > 0L, hero.sets.blockAttackChancePct).flatMap {
        case true =>
          ZIO.succeed(MobStrike(
            newHp      = hero.fightStats.hp,
            newArmor   = hero.fightStats.armor,
            damage     = 0L,
            extraLines = List(content.format("battle.hunterBlock", "monster" -> battle.monsterName)),
            battle     = battle,
            mirrored   = true))
        case false => plainMobStrike(hero, battle, buffedEff, nowMs)
      }

  private def plainMobStrike(hero: Hero, battle: SoloPveBattle, buffedEff: FightStats, nowMs: Long): Task[MobStrike] = {
    val monster = battle.toMonster
    for {
            spread <- Random.nextLongBetween(80L, 121L)
            rawDamage = (monsterAttack(battle) * spread / 100L * Achievement.damagePct(hero) / 100L).max(1L)
            reduction = heroDamageReduction(hero, battle, buffedEff, monster.fightStats.atk, nowMs)
            // Поднятый щит принимает три четверти того, что прошло сквозь защиту.
            baseReduced = BattleState.shieldCut(battle.heroBlockPct, (rawDamage * (1.0 - reduction)).toLong.max(1L))
            // «Непробиваемый»: 20% шанс срезать полученный урон обычной атаки вдвое.
            impenTriggered <- chanceRoll(hero.passives.hasImpenetrable, PassiveKind.Impenetrable.TriggerPct)
            impenDamage =
              if (impenTriggered) (baseReduced * (100L - hero.passives.impenetrableReductionPct) / 100L).max(1L)
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
              if (toughTriggered) (hero.effectiveMaxArmor(nowMs) * hero.passives.toughnessRestorePct / 100L).max(1L) else 0L
            // «Шипастый»: вернуть 5% полученного урона врагу (по HP моба, мимо брони).
            thorns = if (hero.passives.hasSpiky) (reducedDamage * hero.passives.thornsPct / 100L).max(1L) else 0L
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
            // Отравленное оружие: удар, дошедший до HP, травит. Мурлок и эльф
            // ходят так всегда (Race.weaponPoison), Крысиному королю это
            // поставлено флагом при встрече.
            poisonedBlade = battle.effects.monsterPoisonsOnHit ||
                              Race.withNameOption(battle.monsterRace).exists(_.weaponPoison)
            poisons = poisonedBlade && newHp < hero.fightStats.hp
            // Огонь и холод гасят друг друга и на герое: огонь по замёрзшему
            // растапливает лёд и не занимается, холод по горящему гасит пламя и
            // не ложится.
            thawed   = ignites && battle.effects.coldOnHero
            quenched = mobProc && procElement.contains(Element.Cold) &&
                         (battle.effects.heroBurn.isDefined || (ignites && !thawed))
            effectsAfterHit = {
              val burned =
                if (!ignites) battle.effects
                else if (thawed) battle.effects.copy(heroColdDefenceCut = 0)
                else battle.effects.copy(heroBurn = Some(
                  battle.effects.heroBurn.map(_.reignited).getOrElse(Burn(burnPct))))
              val poisonedE =
                if (!poisons) burned
                else burned.copy(heroPoison = Some(
                  burned.heroPoison.map(p => Poison(p.pct + Poison.OnHit)).getOrElse(Poison.onHit)))
              // Холод обгрызает защиту героя (накопительно, не затухает), воздух
              // поднимает мобу точность и уклонение на несколько ходов.
              (if (mobProc) procElement else None) match {
                case Some(Element.Cold) if quenched => poisonedE.copy(heroBurn = None)
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
              // Из минибоссов поджигает только огненный элементаль; остальные —
              // мобы с огнём на оружии (демоны), у них строка без элементаля.
              Option.when(ignites && !thawed)(content.text(
                if (battle.boss.isDefined) "battle.elemental.ignites" else "battle.mobIgnites")),
              Option.when(thawed)(content.text("battle.elements.thawHero")),
              Option.when(poisons)(content.text("battle.mobPoisons")),
              Option.when(mobProc && procElement.contains(Element.Cold) && !quenched)(content.text("battle.mobColdBite")),
              Option.when(quenched)(content.text("battle.elements.quenchHero")),
              Option.when(mobProc && procElement.contains(Element.Air))(content.text("battle.mobAirBoost"))
            ).flatten
          } yield MobStrike(newHp, newArmor + restoredArmor, reducedDamage, lines, battleAfterAtk)
  }

  /** Умение моба поверх обычной атаки: самое дорогое по карману, оплачивается
    * энергией. «Охотник» может погасить первую вредную способность. Возвращает
    * бой моба, героя и строку лога (пустую, если кастовать нечего). */
  private def mobSkillCast(hero: Hero, battle: SoloPveBattle, nowMs: Long, victim: Victim): Task[(SoloPveBattle, Hero, String)] =
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
            onAlly = (victim, ms.againstOther) match {
                       case (Victim.Ally(pos), Some(hit)) => Some((pos, hit))
                       case _                             => None
                     }
            hurts = cast.heroHp < hero.fightStats.hp || cast.heroArmor < hero.fightStats.armor
            // «Охотник» (порог 10): приём, который должен был навредить, с шансом
            // срывается целиком — и так каждый раз, а не единожды за бой.
            blocked <- chanceRoll(hurts && hero.sets.blockSkillChancePct > 0L, hero.sets.blockSkillChancePct)
          } yield onAlly match {
            // Перед мобом стоит не герой: бьющее умение достаётся союзнику, а
            // лечение и починка доспеха (у них нет доли урона) идут себе, как
            // шли.
            case Some((pos, hit)) => strikeAllyWithSkill(hero, paid, pos, hit, ms)
            case None =>
              if (blocked) (cast.battle, hero, content.text("battle.hunterBlockSkill"))
              else (cast.battle, hero.copy(fightStats = hero.fightStats.copy(hp = cast.heroHp, armor = cast.heroArmor)), cast.line)
          }
        else ZIO.succeed((battle, hero, ""))
    } yield out

  /** Умение моба по союзнику: та же доля его атаки, его же броня и защита (у
    * дробящего — мимо брони), и яд, если умение его оставляет. */
  private def strikeAllyWithSkill(
      hero: Hero, battle: SoloPveBattle, pos: Int, hit: MonsterSkill.OtherHit, skill: MonsterSkill
  ): (SoloPveBattle, Hero, String) =
    battle.group.allyAt(pos).filter(_.alive) match {
      case None => (battle, hero, "")
      case Some(a) =>
        val raw = (battle.monsterStats.atk * hit.factor).toLong.max(1L)
        val (struck, dealt) =
          if (hit.ignoresArmor)
            (battle.copy(group = battle.group.updateAlly(pos)(x => x.copy(hp = (x.hp - raw).max(0L)))), raw)
          else {
            val (_, b, d) = hurtVictim(Victim.Ally(pos), hero, battle, raw)
            (b, d)
          }
        val poisoned = if (hit.poisonPct > 0) poisonVictim(Victim.Ally(pos), struck, hit.poisonPct) else struck
        val key      = if (hit.poisonPct > 0) "battle.ally.mobSkillPoison" else "battle.ally.mobSkill"
        (poisoned, hero, content.format(key, "monster" -> battle.monsterName, "name" -> a.name,
          "skill" -> skill.label, "damage" -> dealt.toString))
    }

  private def monsterPhase(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long,
      log: Vector[String],
      skip: Set[Long],
      vampHeal: Long
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
            val line  = content.format("battle.mobHit", "damage" -> st.damage.toString, "monster" -> ticked.monsterName)
            val lines = if (st.mirrored) st.extraLines else line :: st.extraLines
            (st.newHp, st.newArmor, lines.mkString("\n"), st.battle)
          }
        else {
          val missLine = content.format("battle.mobMiss", "monster" -> ticked.monsterName, "chance" -> mobHitPct.toString)
          // Контрудар: герой увернулся — и тут же бьёт в ответ, наверняка. Раз за раунд.
          if (ticked.heroCounterReady)
            playerStrike(hero, ticked.copy(heroCounterReady = false), nowMs, Vector.empty, skip,
              retaliate = false, sureHit = true).map { res =>
              (res.hero.fightStats.hp, res.hero.fightStats.armor,
                (Vector(missLine, content.text("battle.counter")) ++ res.log).mkString("\n"), res.battle)
            }
          else ZIO.succeed((hero.fightStats.hp, hero.fightStats.armor, missLine, ticked))
        }
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
          bossTurnCast(heroAfterAtk, battleAfterAtk, nowMs, Victim.Hero)
        else mobSkillCast(heroAfterAtk, battleAfterAtk, nowMs, Victim.Hero)
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
      // «Упырь» (порог 10): кровь ЧУЖОЙ стороны идёт в лечение и делится ровно
      // между вампирами СВОЕЙ. Сегодня вампир один — сам герой, поэтому ему
      // достаётся всё (см. ItemSet.Ghoul.bleedHealShare).
      vampiresOnHeroSide = if (tickedHero.sets.healsFromBleed) 1 else 0
      bleedShare         = ItemSet.Ghoul.bleedHealShare(bleedDealt, vampiresOnHeroSide)
      heroFedByBleed =
        if (bleedShare > 0L)
          tickedHero.copy(fightStats = tickedHero.fightStats.copy(
            hp = (tickedHero.fightStats.hp + bleedShare).min(tickedHero.effectiveMaxHp(nowMs))))
        else tickedHero
      // Выпито за раунд: вампиризм с удара плюс чужая кровь. Считаем по
      // РЕАЛЬНОМУ приросту HP — у полного героя лечить уже нечего, и в логе тогда
      // не появится числа, которого он не получил.
      bleedGained = heroFedByBleed.fightStats.hp - tickedHero.fightStats.hp
      drunk       = vampHeal + bleedGained

      // Реген энергии в конце хода: +(Интеллект + 0.5·Ловкость), не меньше 1 и не
      // выше максимума. «Сосредоточенность» множит реген на 1.1. Пока герой жив.
      finalHeroWithEnergy = if (heroAlive) endOfRoundHero(heroFedByBleed, nowMs) else heroFedByBleed

      // Сегмент монстра: пустой разделитель, строка атаки, затем (в исходном
      // порядке) каст моба, тики DoT на мобе, тики и реген героя, и последним —
      // вампиризм. Только непустые.
      monsterLog = {
        val base       = (log :+ "") :+ mobLine
        val withCast   = if (castLine.nonEmpty) base :+ castLine else base
        val withPoison = if (monsterEffectLine.nonEmpty) withCast :+ monsterEffectLine else withCast
        val withHero   = if (heroEffectLine.nonEmpty) withPoison :+ heroEffectLine else withPoison
        withHero ++ vampLine(drunk)
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
  /** Конец раунда для героя: энергия ([[regainEnergy]]) и реген пассивок
    * «Целебный»/«Самовосстанавливающийся» с черепами в снаряжении — то самое
    * `(+N)` с экрана боя ([[Hero.withCombatRegen]]).
    *
    * Одна точка на все ходы. Раньше реген HP висел на базовой атаке героя, и
    * раунд без удара — «Ждать», шаг к союзнику, умение, после которого бить
    * некого, — проходил без регена вовсе. */
  private def endOfRoundHero(hero: Hero, nowMs: Long): Hero =
    regainEnergy(hero, nowMs).withCombatRegen(nowMs)

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
    TurnResult(endOfRoundHero(hero, nowMs), battle.tickBuffs(skip), log, Outcome.Victory)

  /** Тик эффектов ГЕРОЯ в конце раунда: реген лечит на `pct`% макс.HP и слабеет.
    * Возвращает обновлённых героя и бой (с ослабленным регеном) плюс строку
    * эффекта (пустая, если регена нет). */
  private def tickHeroEffects(
      hero: Hero,
      battle: SoloPveBattle,
      nowMs: Long
  ): (Hero, SoloPveBattle, String) = {
    // «Некромант» (порог 10): герой сам мёртв — травить и обескровливать в нём
    // нечего. Яд и кровь гаснут на нём сразу, одной строкой на оба.
    val undead  = hero.sets.heroIsUndead
    val hadRot  = undead && (battle.effects.heroPoison.isDefined || battle.effects.heroBleed.isDefined)
    val battle0 = if (!hadRot) battle
                  else battle.copy(effects = battle.effects.copy(heroPoison = None, heroBleed = None))
    val rotLine = if (hadRot) content.text("battle.undeadImmune") else ""
    // Горение на герое (огненный элементаль) тикает до регена: оно усиливается
    // каждый раунд, как и горение на мобе. Мёртвая плоть занимается охотнее:
    // и урон огня по ней выше, и само пламя растёт быстрее.
    val (burnedHero, burnedBattle, burnLine) = battle0.effects.heroBurn match {
      case Some(burn) =>
        val maxHp = hero.effectiveMaxHp(nowMs)
        val dmg   = (bossDamageTaken(hero, battle0, burn.damageOn(maxHp)) * hero.sets.fireTakenMult).toLong
        val grown = (1L to hero.sets.heroBurnGrowthMult.max(1L)).foldLeft(burn)((b, _) => b.grown)
        (hero.copy(fightStats = hero.fightStats.copy(hp = (hero.fightStats.hp - dmg).max(0L))),
         battle0.copy(effects = battle0.effects.copy(heroBurn = Some(grown))),
         heroDotTick(Burn.Icon, Burn.Label, dmg))
      case None => (hero, battle0, "")
    }
    // Яд на герое: снимает % макс.HP мимо брони и слабеет, как яд на мобе.
    // Гниль Джо тикает той же строкой, что любой другой яд: своё у минибосса —
    // само отравление («Ядовитые зловония…»), а не его последствия.
    // «Крыса» (порог 10): зараза герою не новость — яд и кровь снимают с него
    // вдвое меньше.
    val dotMult = hero.sets.heroDotTakenMult
    val (poisonedHero, poisonedBattle, poisonLine) = burnedBattle.effects.heroPoison match {
      case Some(poison) =>
        val dmg = (poison.damageOn(burnedHero.effectiveMaxHp(nowMs)) * dotMult).toLong
        (burnedHero.copy(fightStats = burnedHero.fightStats.copy(
           hp = (burnedHero.fightStats.hp - dmg).max(0L))),
         burnedBattle.copy(effects = burnedBattle.effects.copy(heroPoison = poison.decayed)),
         heroDotTick(Poison.Icon, Poison.Label, dmg))
      case None => (burnedHero, burnedBattle, "")
    }
    // Кровотечение на герое (пасть и когти волка): % макс.HP мимо брони. Само
    // собой оно не затухает — снимает его только лечение (см. activeHeal); с
    // «Крысой» (порог 10) рана затягивается, как слабеет яд.
    val (bledHero, bledBattle, bleedLine) = poisonedBattle.effects.heroBleed match {
      case Some(bleed) =>
        val dmg   = (bleed.damageOn(poisonedHero.effectiveMaxHp(nowMs)) * dotMult).toLong
        val decay = hero.sets.heroBleedDecayPerRound
        val left  = if (decay <= 0) Some(bleed) else Bleed.of(bleed.pct - decay)
        (poisonedHero.copy(fightStats = poisonedHero.fightStats.copy(
           hp = (poisonedHero.fightStats.hp - dmg).max(0L))),
         poisonedBattle.copy(effects = poisonedBattle.effects.copy(heroBleed = left)),
         heroDotTick(Bleed.Icon, Bleed.Label, dmg))
      case None => (poisonedHero, poisonedBattle, "")
    }
    val (finalHero, finalBattle, regenLine) = bledBattle.effects.heroRegen match {
      case Some(regen) =>
        val maxHp  = bledHero.effectiveMaxHp(nowMs)
        // Регенерация горением НЕ режется: это не действие игрока, а эффект,
        // который тикает сам по себе (см. activeHeal).
        val heal   = regen.healOn(maxHp)
        val newHp  = (bledHero.fightStats.hp + heal).min(maxHp)
        val healed = newHp - bledHero.fightStats.hp
        (
          bledHero.copy(fightStats = bledHero.fightStats.copy(hp = newHp)),
          bledBattle.copy(effects = bledBattle.effects.copy(heroRegen = regen.decayed)),
          content.format("battle.regenTick", "healed" -> healed.toString)
        )
      case None => (bledHero, bledBattle, "")
    }
    // Строки бывают непустыми одновременно (горим, травимся и регенерируем) — склеиваем.
    (finalHero, finalBattle,
      List(rotLine, burnLine, poisonLine, bleedLine, regenLine).filter(_.nonEmpty).mkString("\n"))
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

    // Поднятый мёртв: травить и обескровливать в нём нечего, и зараза с него
    // сходит сама — ровно как с героя-нежити. Зато сухая плоть занимается
    // охотнее: огонь по ней весомее и растёт быстрее.
    val undead = battle.monsterUndead
    if (undead && (eff.monsterPoison.isDefined || eff.monsterBleed.isDefined)) {
      eff   = eff.copy(monsterPoison = None, monsterBleed = None)
      lines = lines :+ content.format("battle.monsterUndeadImmune", "monster" -> monsterName)
    }

    eff.monsterPoison.foreach { p =>
      val dmg = monsterDotDamage(battle, p.damageOn(maxHp))
      hp = (hp - dmg).max(0L)
      eff = eff.copy(monsterPoison = p.decayed)
      lines = lines :+ dotTick(Poison.Icon, Poison.Label, dmg, monsterName)
    }
    eff.monsterBleed.foreach { b =>
      val dmg = monsterDotDamage(battle, b.damageOn(maxHp))
      hp = (hp - dmg).max(0L)
      bled += dmg
      // Кровотечение не затухает — сила остаётся прежней.
      lines = lines :+ dotTick(Bleed.Icon, Bleed.Label, dmg, monsterName)
    }
    eff.monsterBurn.foreach { bn =>
      val fire = if (undead) ItemSet.Necromancer.UndeadFireTakenPct else 0L
      val dmg  = bn.damageOn(maxHp) * (100L + fire) / 100L
      hp = (hp - dmg).max(0L)
      // «Дикое пламя» (порог 6) ускоряет рост: шаг применяется несколько раз,
      // сохраняя саму кривую роста (+2 п.п. до порога, дальше +1). На поднятом
      // пламя растёт вдвое охотнее — тем же порядком.
      val steps = burnGrowthMult.max(1L) * (if (undead) ItemSet.Necromancer.UndeadBurnGrowthMult else 1L)
      eff = eff.copy(monsterBurn = Some((1L to steps).foldLeft(bn)((b, _) => b.grown)))
      lines = lines :+ dotTick(Burn.Icon, Burn.Label, dmg, monsterName)
    }
    (battle.copy(monsterCurrentHp = hp, effects = eff), lines.mkString("\n"), bled)
  }

  // ── Ход игрока: активный навык ──────────────────────────────────────────────

  /** Кнопка умения. В группе умение с уроном достаёт и соседа героя (моб под
    * номером 2), поэтому сначала спрашиваем, в кого бить; лечение и броня — себе
    * или союзнику рядом, а рядом никого — себе, без вопросов. Неготовому умению
    * экран цели не нужен — обычный ход сам скажет, почему нельзя. */
  private def skillRoute(user: User, itemId: Long, target: Option[Int], renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      state  <-
        if (target.isEmpty && needsTarget(hero, battle, itemId))
          renderer.show(user, targetScreen(hero, battle, itemId)).as(StateType.Battle)
        else resolveLoaded(user, renderer, hero, battle, now,
          // Соперник увидит название умения, а не его рассказ от первого лица.
          battle.slotByItem(itemId).map(_.skill.label).getOrElse(""))(
          skillTurn(itemId, target
            .orElse(battle.slotByItem(itemId).filterNot(s => BattleState.supports(s.skill.effect))
              .flatMap(s => skillTargets(hero, battle, s.skill).headOption))
            .getOrElse(battle.group.heroPos)))
    } yield state

  /** Спрашивать ли цель: умение готово, и выбрать есть из кого — бьёт, а рядом
    * больше одного врага, или лечит, а рядом стоит союзник. */
  private def needsTarget(hero: Hero, battle: SoloPveBattle, itemId: Long): Boolean =
    battle.slotByItem(itemId).exists { slot =>
      val ready = slot.cooldown <= 0 && hero.fightStats.energy >= slot.skill.energyCost(hero)
      ready && (
        (BattleState.dealsDamage(slot.skill.effect) && skillTargets(hero, battle, slot.skill).size > 1) ||
        (BattleState.supports(slot.skill.effect) && supportAllies(battle).nonEmpty))
    }

  /** Экран выбора цели для базовой атаки: те же кнопки, что у умения с уроном. */
  private def attackTargetScreen(hero: Hero, battle: SoloPveBattle): Screen =
    targetChoices(battle, "Attack", content.text("battle.group.attackLabel"), support = false,
      heroTargets(hero, battle))

  /** Экран выбора цели: моб напротив и соседи по местам, с их полосками.
    * Подпись кнопки ограничена по длине, поэтому имя — короткое (у отмеченного
    * тьмой — только раса), а что всё равно не влезло, срезается. */
  private def targetScreen(hero: Hero, battle: SoloPveBattle, itemId: Long): Screen = {
    val slot       = battle.slotByItem(itemId)
    val skillLabel = slot.map(_.skill.label).getOrElse("")
    val support    = slot.exists(s => BattleState.supports(s.skill.effect))
    targetChoices(battle, s"Skill_$itemId", skillLabel, support,
      slot.fold(battle.group.attackTargets)(s => skillTargets(hero, battle, s.skill)))
  }

  /** По кому герой может ударить оружием: мечом — соседей, из лука — дальше
    * (см. [[BattleState.heroReach]]). */
  private def heroTargets(hero: Hero, battle: SoloPveBattle): List[Int] =
    battle.group.targetsWithin(BattleState.heroReach(hero))

  /** По кому достаёт умение (см. [[BattleState.skillReach]]). */
  private def skillTargets(hero: Hero, battle: SoloPveBattle, skill: Skill): List[Int] =
    battle.group.targetsWithin(BattleState.skillReach(hero, skill))

  /** До каких союзников достают лечение и броня: живые соседи героя по ряду
    * (см. [[BattleState.SupportReach]]) — так же, как удар по врагам. */
  private def supportAllies(battle: SoloPveBattle): List[BattleAlly] =
    battle.group.alliesWithin(BattleState.SupportReach)

  /** Кнопки целей под действие `choiceId`: лечение и броня — себе или
    * союзнику рядом (см. [[supportAllies]]); урон — `enemies`, моб в паре и все,
    * до кого достаёт оружие (см. [[heroTargets]]). */
  private def targetChoices(battle: SoloPveBattle, choiceId: String, label: String, support: Boolean,
                            enemies: List[Int]): Screen = {
    val targets =
      if (support) {
        val me = pangea.engine.Choice(choiceId, content.text("battle.group.targetSelf"),
          data = Map("target" -> battle.group.heroPos.toString), row = Some(0))
        me :: supportAllies(battle).zipWithIndex.map { case (a, i) =>
          pangea.engine.Choice(choiceId,
            pangea.engine.Choice.fit(content.format("battle.group.targetAlly",
              "n" -> a.position.toString, "name" -> a.name, "hp" -> a.hpPct.toString, "armor" -> a.armorPct.toString)),
            data = Map("target" -> a.position.toString), row = Some(1 + i / BattleState.AlliesPerRow))
        }
      } else
        enemies.zipWithIndex.flatMap { case (pos, i) =>
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
      case Some(slot) if BattleState.needsEnemy(slot.skill.effect) && skillTargets(hero, battle, slot.skill).isEmpty =>
        cont(hero, battle, content.text("battle.group.noTarget"))
      // Лечение и броня — себе или союзнику рядом; до дальнего не дотянуться.
      case Some(slot) if BattleState.supports(slot.skill.effect) && target != battle.group.heroPos &&
                         !supportAllies(battle).exists(_.position == target) =>
        cont(hero, battle, content.text("battle.group.allyOutOfReach"))
      // Блок и Контрудар тратят бонусное действие раунда — то же, что пояс.
      case Some(slot) if BattleState.bonusAction(slot.skill.effect) && battle.consumableUsedThisRound =>
        cont(hero, battle, content.text("battle.consumableAlreadyUsed"))
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
      battle0: SoloPveBattle,
      slot: SkillSlotState,
      nowMs: Long,
      target: Int
  ): Task[TurnResult] = {
    // «Отскок»: сперва прыжок, потом выстрел — уже с нового места. Пригвождённый
    // не прыгает — стреляет с места.
    val leapTo =
      if (slot.skill.effect != Skill.Effect.Leap || battle0.effects.heroRooted) None
      else battle0.group.leapSpot(target, BattleState.displacement(hero), BattleState.skillReach(hero, slot.skill))
    val battle      = leapTo.fold(battle0)(p => battle0.copy(group = battle0.group.copy(heroPos = p)))
    val home        = battle.group.activePos
    val paired      = battle.group.paired
    val damageSkill = BattleState.dealsDamage(slot.skill.effect)
    // Цель не в полях (сосед в досягаемости, а из лука — и дальше) — разворачиваем
    // её туда на время.
    val aimSide = damageSkill && target != home &&
                    battle.group.inReachOf(target, BattleState.skillReach(hero, slot.skill))
    val aimed   = if (aimSide) battle.engage(target) else battle
    val skip    = Set(slot.itemId)
    // Таран по мобу не из пары помечает место, куда герой шагнёт в конце раунда.
    def ramMark(b: SoloPveBattle): SoloPveBattle =
      if (slot.skill == Skill.Ram && target != b.group.heroPos) b.copy(group = b.group.copy(pendingMove = Some(target))) else b
    // Что делать после урона. В паре: победа или базовая атака по паре (по
    // соседу — сперва вернуть пару в поля). Пары нет: базовая атака идёт по
    // той же цели, а у лечения — по единственному соседу, если он есть; ответа
    // пары нет — ход кончается тихо. `at` — где цель стоит сейчас:
    // «Отбрасывающий выстрел» уносит её с того места, куда целились.
    def nextAt(at: Int): (Hero, SoloPveBattle, Vector[String]) => Task[TurnResult] = (h, b0, lines) => {
      val b    = if (damageSkill) b0.rememberTarget(at) else b0
      val dead = damageSkill && b.monsterCurrentHp <= 0
      // Отброшенного базовая атака достаёт в этом же раунде, как бы далеко он ни
      // улетел: выстрел и удар — одно движение. Дальше — по обычной дальности.
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
            val fallen = back.sideFallen(back.group.idxOf(at))
            val slain  = lines :+ content.format("battle.group.sideSlain", "monster" -> b.monsterName)
            if (paired) playerStrike(h, fallen, nowMs, slain, skip) else followUp(h, fallen, nowMs, slain, skip)
          }
        } else {
          val rammed = ramMark(back)
          if (paired && !homeGone) playerStrike(h, rammed, nowMs, lines, skip)
          else strikeSide(h, rammed, at, nowMs, lines, skip).flatMap(closeTurn(_, nowMs, skip))
        }
      }
    }
    val next = nextAt(target)
    // Лечение и броня, наведённые на живого союзника, достаются ему, а не герою.
    object OnAlly {
      def unapply(effect: Skill.Effect): Option[BattleAlly] = effect match {
        case Skill.Effect.Heal | Skill.Effect.RepairArmor | Skill.Effect.GuardRepair(_, _) if target != aimed.group.heroPos =>
          aimed.group.allyAt(target).filter(_.alive)
        case _ => None
      }
    }
    for {
      spread <- Random.nextLongBetween(80L, 121L)
      // Понимание руны множит число умения — и с вещи, и с клейма (см. Hero.runeMult).
      // Умение оружия из лука вблизи — на пятую часть слабее, как и удар; удары
      // телом и щитом лук не трогает.
      closeMod = if (Skill.armorSkills.contains(slot.skill)) 1.0 else BattleState.rangedCloseMod(hero, aimed)
      raw = (slot.skill.baseValue(hero, nowMs) * spread / 100.0 * hero.weaponDust.damageMult * hero.runeMult(slot.skill) *
               closeMod).toLong.max(1L)
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

        case Skill.Effect.Pin(turns) =>
          // Урон, срезанный защитой, — и цель не сходит с места.
          dealSkillDamage(hero, rooted(bumped, turns), reduced, dealt => tmpl.replace("{}", dealt.toString), next)

        case Skill.Effect.Volley(turns) =>
          // По цели — как удар, срезанный защитой; каждому другому в пределах
          // дальности оружия — тот же бросок, срезанный уже его защитой. Все
          // задетые раунд не шагают.
          val reach = BattleState.skillReach(hero, slot.skill)
          dealSkillDamage(hero, rooted(bumped, turns), reduced, dealt => tmpl.replace("{}", dealt.toString), next,
            splash = (b, _) => strikeOthers(b, (b.group.heroPos - reach) to (b.group.heroPos + reach)) { tmp =>
              val amount = (raw * (1.0 - monsterDefenceCut(hero, tmp, effWithAir(hero, tmp, nowMs), nowMs))).toLong.max(1L)
              (rooted(plainHit(tmp, amount), turns),
               content.format("battle.group.barrageHit", "monster" -> tmp.monsterName, "damage" -> amount.toString))
            })

        case Skill.Effect.Leap =>
          // Прыжок уже сделан (см. начало): выстрел — как удар, срезанный защитой.
          val jump = leapTo.fold(content.text(
              if (battle0.effects.heroRooted) "battle.group.leapPinned" else "battle.group.leapNoRoom"))(p =>
              content.format("battle.group.leap", "n" -> p.toString)) + "\n"
          dealSkillDamage(hero, bumped, reduced, dealt => jump + tmpl.replace("{}", dealt.toString), next)

        case Skill.Effect.Push =>
          // Урон, срезанный защитой; выживший отлетает от героя, и пара рвётся.
          // Минибосса и сооружение не сдвинуть.
          val pushed: (Hero, SoloPveBattle, Vector[String]) => Task[TurnResult] = (h, b, lines) =>
            if (b.monsterCurrentHp <= 0) next(h, b, lines)
            else b.pushActive(BattleState.displacement(hero)) match {
              case Some((moved, to)) =>
                nextAt(to)(h, moved, lines :+ content.format("battle.group.pushed", "monster" -> b.monsterName, "n" -> to.toString))
              case None if b.bossKind.isDefined || Race.immovable(Race.withName(b.monsterRace)) =>
                next(h, b, lines :+ content.format("battle.group.unpushable", "monster" -> b.monsterName))
              case None => next(h, b, lines)
            }
          dealSkillDamage(hero, bumped, reduced, dealt => tmpl.replace("{}", dealt.toString), pushed)

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
            // «Охотник» (порог 12) прибавляется к своему шансу умения, а не
            // множит урон второй раз: четверного удара больше нет.
            chance = (2L * hero.effectiveBaseStats(nowMs).int - aimed.monsterLvl + hero.sets.skillCritBonusPct)
                       .max(0L).min(95L)
            doubled = roll <= chance
            value   = if (doubled) raw * 2L else raw
            line: (Long => String) = (dealt: Long) =>
              tmpl.replace("{}", dealt.toString) +
                (if (doubled) "\n" + content.text("battle.weakSpotDouble") else "")
            r <- dealSkillDamage(hero, bumped, value, line, next, critRolled = true)
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
        // союзнику даёт только броню — бафа защиты у него нет. С ранами
        // лечение обходится так же, как с ранами героя (см. activeHeal): кровь
        // снимает, яд ослабляет, пламя тратит собой.
        case OnAlly(ally) =>
          val heals = slot.skill.effect == Skill.Effect.Heal
          val full  = if (heals) (raw * hero.passives.healMult).toLong.max(1L) else raw
          val (updated, gained, wounds) =
            if (heals) {
              val w  = HealedWounds.of(full, ally.poison, ally.burn)
              val nh = (ally.hp + w.amount).min(ally.stats.hp)
              (ally.copy(hp = nh, poison = w.poison, bleed = None, burn = w.burn),
               nh - ally.hp, allyWoundLines(ally, w))
            } else {
              val na = (ally.armor + full).min(ally.stats.armor)
              (ally.copy(armor = na), na - ally.armor, Vector.empty[String])
            }
          val line = content.format("battle.group.supportAlly", "name" -> ally.name,
            "skill" -> slot.skill.label, "amount" -> gained.toString, "what" -> (if (heals) "HP" else "брони"))
          next(hero, bumped.copy(group = bumped.group.updateAlly(ally.position)(_ => updated)), line +: wounds)

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

        // Щит поднят (сколько снимет — по редкости лучшего в руках) или кинжал
        // наизготовку — до конца раунда. Это бонусное действие, как глоток с
        // пояса: ход не кончается, можно ударить или взять умение, а «Быстрые
        // руки» могут его и не потратить. Перезарядка в конце этого раунда не
        // тикает (bonusSlots).
        case Skill.Effect.Block | Skill.Effect.Counter =>
          quickHandsRoll(hero).map { case (consumed, qhLog) =>
            val pct    = hero.equipment.blockPct
            val raised =
              if (slot.skill.effect == Skill.Effect.Block) bumped.copy(heroBlockPct = pct)
              else bumped.copy(heroCounterReady = true)
            TurnResult(hero,
              raised.copy(consumableUsedThisRound = consumed, bonusSlots = raised.bonusSlots :+ slot.itemId),
              Vector(tmpl.replace("{}", pct.toString)) ++ qhLog, Outcome.Continue, endsRound = false)
          }
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
      splash: (SoloPveBattle, Long) => (SoloPveBattle, Vector[String]) = (b, _) => (b, Vector.empty),
      // Крит по этому умению уже разыгран снаружи («Удар в слабое место» катает
      // свой шанс сам) — второй раз не катаем.
      critRolled: Boolean = false
  ): Task[TurnResult] =
    // «Охотник» (порог 12): любое атакующее умение с шансом бьёт вдвое.
    chanceRoll(!critRolled && hero.sets.skillCritBonusPct > 0L, hero.sets.skillCritBonusPct).flatMap { crit =>
      dealSkillDamageCritted(hero, battle, value, line, next, splash, crit)
    }

  private def dealSkillDamageCritted(
      hero: Hero,
      battle: SoloPveBattle,
      value: Long,
      line: Long => String,
      next: (Hero, SoloPveBattle, Vector[String]) => Task[TurnResult],
      splash: (SoloPveBattle, Long) => (SoloPveBattle, Vector[String]),
      crit: Boolean
  ): Task[TurnResult] = {
    // На арене соперник мог поднять щит — умение по нему режется так же, как удар.
    val raw = BattleState.shieldCut(battle.foeBlockPct, if (crit) value.max(1L) * 2L else value.max(1L))
    // Урон навыка тоже несёт стихию оружия — раздельный урон по броне/HP с тем же
    // усилением. Проки стихий роллятся отдельным броском (как и на обычной атаке).
    val resisted          = (raw * bossResistance(hero, battle)).toLong.max(1L)
    // Предел босса за раунд делится между ударом и умением — счёт общий.
    val capped            = capBossDamage(battle, resisted)
    val (armorDmg, hpDmg) = splitElementalDamage(battle.monsterCurrentArmor, capped, hero)
    val critLine          = if (crit) "\n" + content.text("battle.skillCrit") else ""
    val skillLine         = line(armorDmg + hpDmg) + critLine + capLine(resisted, capped).map("\n" + _).getOrElse("")
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
    val effects  = bledEffects
    val hit      = noteBossDamage(battle, armorDmg + hpDmg)
      .copy(monsterCurrentHp = newHp, monsterCurrentArmor = newArmor)
      .withEffects(effects)
    if (newHp <= 0) {
      val (swept, splashLog) = splash(hit, armorDmg + hpDmg)
      next(hero, swept, Vector(skillLine) ++ splashLog)
    } else
      resolveElementProcs(hero, hit).flatMap { case (afterProcs, elemLog) =>
        val (swept, splashLog) = splash(afterProcs, armorDmg + hpDmg)
        next(hero, swept, Vector(skillLine) ++ elemLog ++ splashLog)
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

  /** Моб в полях не сходит с места `turns` раундов. Счётчик ставится с запасом
    * в один тик: он тикает в начале хода моба, а шаги — в конце раунда (см.
    * `BattleEffects.monsterRootedTurns`). Новое не укорачивает уже наложенное. */
  private def rooted(b: SoloPveBattle, turns: Int): SoloPveBattle =
    b.withEffects(b.effects.copy(monsterRootedTurns = b.effects.monsterRootedTurns.max(turns + 1)))

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
    chanceRoll(hero.passives.hasQuickHands, hero.passives.quickHandsChancePct).map { repeated =>
      if (repeated) (false, Vector(content.text("battle.quickHands")))
      else (true, Vector.empty)
    }

  /** Эффект выпитой фляги. Герой приходит уже с потраченным зарядом, а бой —
    * с отметкой о расходнике: откуда фляга, из пояса или из доживающего
    * слота, здесь уже неважно. */
  private def flaskEffect(
      hero: Hero,
      battle: SoloPveBattle,
      effect: FlaskEffect,
      flaskName: String,
      nowMs: Long,
      qhLog: Vector[String]
  ): TurnResult =
    effect match {
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
          TurnResult(hero.copy(fightStats = newStats),
            healedBattle, lines, Outcome.Continue, endsRound = false)
        case FlaskEffect.AddBuff(buff, rounds) =>
          val timedBuff = buff.copy(turnsLeft = Some(rounds))
          val newBattle = battle.copy(heroBattleState = battle.heroBattleState.add(timedBuff))
          TurnResult(hero, newBattle, Vector(content.text("battle.flaskBuff")) ++ qhLog,
            Outcome.Continue, endsRound = false)

        // Фляга кузнеца: чинит броню до потолка.
        case FlaskEffect.ArmorPercent(pct) =>
          val maxArmor = hero.effectiveMaxArmor(nowMs)
          val newArmor = (hero.fightStats.armor + (maxArmor * pct / 100L).max(1L)).min(maxArmor).max(hero.fightStats.armor)
          val msg = content.format("battle.flaskArmor",
            "armor" -> (newArmor - hero.fightStats.armor).toString, "cur" -> newArmor.toString, "max" -> maxArmor.toString)
          TurnResult(hero.copy(fightStats = hero.fightStats.copy(armor = newArmor)),
            battle, Vector(msg) ++ qhLog, Outcome.Continue, endsRound = false)

        // Фляга бодрости: энергия до потолка.
        case FlaskEffect.EnergyPercent(pct) =>
          val maxEn = hero.maxEnergy(nowMs)
          val newEn = (hero.fightStats.energy + (maxEn * pct / 100L).max(1L)).min(maxEn).max(hero.fightStats.energy)
          val msg = content.format("battle.flaskEnergy",
            "energy" -> (newEn - hero.fightStats.energy).toString, "cur" -> newEn.toString, "max" -> maxEn.toString)
          TurnResult(hero.copy(fightStats = hero.fightStats.copy(energy = newEn)),
            battle, Vector(msg) ++ qhLog, Outcome.Continue, endsRound = false)

        // Стихийная фляга: урона нет, прок стихии — гарантирован. Иммунитеты
        // цели в силе: огненного элементаля пламя не берёт, элементалю и
        // нежити прок ложится как обычный (см. withEffects).
        case FlaskEffect.Splash(element) =>
          val (procced, procLog) = applyProcs(battle, Set(element))
          val guarded = procced.withEffects(procced.effects)
          val head = content.format("battle.flaskSplash", "flask" -> flaskName)
          val lines =
            if (element == Element.Fire && guarded.boss.exists(_.immuneToBurn))
              Vector(head, content.text("battle.flaskSplashImmune"))
            else Vector(head) ++ procLog
          TurnResult(hero, guarded, lines ++ qhLog, Outcome.Continue, endsRound = false)

        // Фляга очищения: всё вредное с героя долой — раны, дебафы, срез защиты.
        case FlaskEffect.Cleanse =>
          val cleaned = battle.effects.copy(
            heroBurn = None, heroPoison = None, heroBleed = None,
            heroStunnedTurns = 0, heroGroundedTurns = 0, heroColdDefenceCut = 0)
          TurnResult(hero, battle.copy(effects = cleaned),
            Vector(content.text("battle.flaskCleanse")) ++ qhLog, Outcome.Continue, endsRound = false)

        // Дымная фляга: мобы вне пары на несколько раундов теряют из виду и
        // героя, и друг друга. Запас в один тик — см. BattleEffects.heroSmokeTurns.
        case FlaskEffect.Smoke(rounds) =>
          TurnResult(hero,
            battle.copy(effects = battle.effects.copy(heroSmokeTurns = rounds + 1)),
            Vector(content.format("battle.flaskSmoke", "rounds" -> rounds.toString)) ++ qhLog,
            Outcome.Continue, endsRound = false)

        // Вампирская фляга: следующие удары по HP лечат (см. playerStrike).
        case FlaskEffect.Vampiric(hits, pct) =>
          TurnResult(hero,
            battle.copy(effects = battle.effects.copy(heroVampiricHits = hits)),
            Vector(content.format("battle.flaskVampiric", "hits" -> hits.toString, "pct" -> pct.toString)) ++ qhLog,
            Outcome.Continue, endsRound = false)

        // Фляги яда и крови: оружие смазано на несколько раундов (см. playerStrike).
        case FlaskEffect.PoisonCoat(rounds) =>
          TurnResult(hero,
            battle.copy(effects = battle.effects.copy(heroPoisonCoatTurns = rounds)),
            Vector(content.format("battle.flaskPoisonCoat", "rounds" -> rounds.toString)) ++ qhLog,
            Outcome.Continue, endsRound = false)
        case FlaskEffect.BleedCoat(rounds) =>
          TurnResult(hero,
            battle.copy(effects = battle.effects.copy(heroBleedCoatTurns = rounds)),
            Vector(content.format("battle.flaskBleedCoat", "rounds" -> rounds.toString)) ++ qhLog,
            Outcome.Continue, endsRound = false)
    }

  /** Фляга из доживающего слота: кнопки в бою у неё больше нет, но ход
    * остаётся рабочим — старое сообщение в диалоге не должно ронять бой. */
  private def flaskTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] = {
    val flask = hero.equipment.flask
    flask.details match {
      case f: ItemDetails.Flask if f.charges <= 0 =>
        cont(hero, battle, content.text("battle.flaskEmpty"))
      case _: ItemDetails.Flask if battle.consumableUsedThisRound =>
        cont(hero, battle, content.text("battle.consumableAlreadyUsed"))
      case f: ItemDetails.Flask =>
        quickHandsRoll(hero).map { case (consumed, qhLog) =>
          val drunk = hero.copy(equipment = hero.equipment.copy(flask = flask.copy(details = f.spent)))
          flaskEffect(drunk, battle.copy(consumableUsedThisRound = consumed), f.effect, flask.name, nowMs, qhLog)
        }
      case _ =>
        cont(hero, battle, content.text("battle.noFlask"))
    }
  }

  /** Пояс в бою: сумка с расходниками. Кнопка «Пояс» ход не тратит — она
    * присылает отдельное сообщение со списком ячеек, и кнопки живут внутри
    * него (`inline`), поэтому боевая клавиатура и экран боя остаются как есть.
    *
    * Опустевшие ячейки стоят в списке с пометкой «пусто», но кнопки у них нет:
    * брать из них нечего. */
  private def showBelt(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      belt   = hero.equipment.belt
      // Номер у строки и у кнопки — настоящий номер ячейки: иначе высыпанная
      // горсть в первой ячейке сдвинула бы все остальные.
      slots  = BattleState.beltCells(hero)
      cells  = BattleState.beltUsable(hero)
      cap    = ItemDetails.Belt.capacityFor(belt.rarity)
      screen =
        if (belt.itemType != ItemType.Belt) Screen(content.text("battle.belt.none"), Nil, inline = true)
        else if (slots.isEmpty) Screen(content.text("battle.belt.empty"), Nil, inline = true)
        else {
          val lines = slots.map { case (slot, idx) =>
            if (slot.uses > 0)
              content.format("battle.belt.line", "n" -> (idx + 1).toString,
                "name" -> slot.name, "uses" -> slot.uses.toString)
            else content.format("battle.belt.spentLine", "n" -> (idx + 1).toString, "name" -> slot.name)
          }
          val buttons = cells.zipWithIndex.map { case ((slot, idx), i) =>
            pangea.engine.Choice("BeltUse", pangea.engine.Choice.fit(
              content.format("battle.belt.useBtn", "name" -> slot.name)),
              data = Map("slot" -> idx.toString), row = Some(i))
          }
          val head = if (cells.isEmpty) "battle.belt.headSpent" else "battle.belt.head"
          Screen(content.format(head, "used" -> slots.size.toString,
            "cap" -> cap.toString) + "\n" + lines.mkString("\n"), buttons, inline = true)
        }
      _ <- renderer.show(user, screen)
    } yield StateType.Battle

  /** Применить расходник из ячейки пояса. Фляга пьётся, пыль ложится на
    * оружие, отвар уходит внутрь, роза раскрывается; ход при этом не кончается
    * — как и раньше у расходников.
    *
    * Отвару, которому нужно знать, на что его тратят (какую травму снять, кого
    * приговорить), сперва задаём вопрос — ход на вопрос не уходит. */
  private def beltUse(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    BattleState.parseSlot(ua) match {
      case None      => showBelt(user, renderer)
      case Some(idx) =>
        (getHero(user) <*> ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))).flatMap { case (hero, now) =>
          BattleState.beltAt(hero, idx) match {
            case Some(br: BeltSlot.Brew) if BattleState.needsPick(br.kind) =>
              brewRefusal(hero, br.kind, now) match {
                case Some(why) => renderer.show(user, Screen(why, Nil, inline = true)).as(StateType.Battle)
                case None      => offerBeltPick(user, hero, idx, br.kind, now, renderer)
              }
            case _ =>
              resolve(user, renderer, content.text("arena.turn.belt"))(beltTurn(idx)) <* showBelt(user, renderer)
          }
        }
    }

  /** Выбор назван: тратим ячейку на него. Кнопка несёт и номер ячейки, и сам
    * выбор, поэтому между вопросом и ответом ничего не хранится. */
  private def beltPick(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    (BattleState.parseSlot(ua), BattleState.parsePick(ua)) match {
      case (Some(idx), Some(pick)) =>
        resolve(user, renderer, content.text("arena.turn.belt"))(beltTurn(idx, Some(pick))) <* showBelt(user, renderer)
      case _ => showBelt(user, renderer)
    }

  /** Вопрос к отвару: пока такой один — костоправный спрашивает, какую травму
    * снять. Кнопки живут внутри отдельного сообщения, как и сам пояс, — боевая
    * сцена не меняется. */
  private def offerBeltPick(user: User, hero: Hero, idx: Int, kind: BrewKind, nowMs: Long,
                            renderer: Renderer): Task[StateType] = {
    val (head, options) = kind.effect match {
      case BrewEffect.CureTrauma =>
        (content.text("battle.belt.whichTrauma"),
          hero.activeTraumas(nowMs).filter(_.severity != Trauma.Heavy).distinct.map(t => (t.name, t.name)))
      case _ => (content.text("battle.belt.gone"), Nil)
    }
    val buttons = options.zipWithIndex.map { case ((label, pick), i) =>
      pangea.engine.Choice("BeltPick", pangea.engine.Choice.fit(label),
        data = Map("slot" -> idx.toString, "pick" -> pick),
        row = Some(i / BattleState.PicksPerRow))
    }
    renderer.show(user, Screen(head, buttons, inline = true)).as(StateType.Battle)
  }

  /** Ход расходником из ячейки `idx`. `pick` — ответ на вопрос отвара, если он
    * его задавал (см. [[offerBeltPick]]). */
  private def beltTurn(idx: Int, pick: Option[String] = None)(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    hero.equipment.belt.details match {
      case b: ItemDetails.Belt if battle.consumableUsedThisRound =>
        val _ = b
        cont(hero, battle, content.text("battle.consumableAlreadyUsed"))
      case b: ItemDetails.Belt =>
        b.at(idx) match {
          case None => cont(hero, battle, content.text("battle.belt.gone"))
          // Кнопка из старого сообщения: у пустой ячейки кнопки нет.
          case Some(f: BeltSlot.Flask) if f.empty =>
            cont(hero, battle, content.text("battle.belt.gone"))
          case Some(f: BeltSlot.Flask) =>
            quickHandsRoll(hero).map { case (consumed, qhLog) =>
              // Выпитая до дна фляга остаётся в ячейке пустой: её дольют
              // Густаво, ручей или отвар, а вампирская напьётся крови сама.
              val drunk = withBelt(hero, b.updated(idx, f.spent))
              flaskEffect(drunk, battle.copy(consumableUsedThisRound = consumed), f.kind.effect,
                f.name, nowMs, qhLog)
            }
          case Some(d: BeltSlot.Dust) if d.spent =>
            cont(hero, battle, content.text("battle.belt.gone"))
          case Some(d: BeltSlot.Dust) =>
            quickHandsRoll(hero).map { case (consumed, qhLog) =>
              val sprinkled = sprinkleInBattle(hero, d.kind)
              val (dusted, line) = sprinkled
              // Ячейка остаётся пустой, но помнит пыль: после боя доберёт такую же.
              val next  = b.updated(idx, d.poured)
              TurnResult(withBelt(dusted, next), battle.copy(consumableUsedThisRound = consumed),
                Vector(line) ++ qhLog, Outcome.Continue, endsRound = false)
            }
          case Some(br: BeltSlot.Brew) if br.spent =>
            cont(hero, battle, content.text("battle.belt.gone"))
          // Кнопка из старого сообщения: ответ на вопрос отвара устарел (травму
          // уже сняли, рода нет). Склянку на это не тратим — пусть спросит снова.
          case Some(br: BeltSlot.Brew) if !pickValid(hero, br.kind, pick, nowMs) =>
            cont(hero, battle, content.text("battle.belt.askAgain"))
          case Some(br: BeltSlot.Brew) =>
            brewRefusal(hero, br.kind, nowMs) match {
              // Лечить нечего, мазать нечем — склянка цела, и раунд тоже.
              case Some(why) => cont(hero, battle, why)
              case None =>
                quickHandsRoll(hero).map { case (consumed, qhLog) =>
                  // Выпитая ячейка остаётся пустой, но помнит отвар: после боя
                  // доберёт такой же из сумки.
                  val drunk = withBelt(hero, b.updated(idx, br.drunk))
                  brewEffect(drunk, battle.copy(consumableUsedThisRound = consumed), br.kind, nowMs, qhLog, pick)
                }
            }
          case Some(r: BeltSlot.Rose) if r.withered =>
            cont(hero, battle, content.text("battle.belt.gone"))
          case Some(r: BeltSlot.Rose) =>
            quickHandsRoll(hero).map { case (consumed, qhLog) =>
              // Раскрытие бьёт по всему полю, как божественное оружие, — только
              // силу цветок берёт от уровня хозяина, своего у него нет. На поясе
              // он расходник, как и всё прочее: один за раунд.
              val kind             = r.kind
              val blow             = DivineRates.DamagePerLvl * hero.lvl
              val (swept, _, foes) = sweep(battle, blow, kind.element, kind.poisons)
              val spent            = r.spent
              val line = content.format(s"battle.rose.${kind.entryName}",
                "damage" -> blow.toString, "foes" -> foes.toString)
              val log = Vector(line) ++
                (if (spent.withered) Vector(content.format("battle.rose.withered", "name" -> kind.itemName))
                 else Vector.empty) ++ qhLog
              val outcome = if (swept.monsterCurrentHp <= 0L) Outcome.Victory else Outcome.Continue
              val hero1   = withBelt(hero, b.updated(idx, spent))
              val after   = if (outcome == Outcome.Victory && swept.promoteNext.isEmpty) endOfRoundHero(hero1, nowMs) else hero1
              TurnResult(after, swept.copy(consumableUsedThisRound = consumed), log, outcome, endsRound = false)
            }
          case Some(h: BeltSlot.HolyWater) if h.spent =>
            cont(hero, battle, content.text("battle.belt.gone"))
          // Вода уже на поле — вторая склянка ничего не прибавит, её не тратим.
          case Some(_: BeltSlot.HolyWater) if battle.holyWater =>
            cont(hero, battle, content.text("battle.holyWater.already"))
          case Some(h: BeltSlot.HolyWater) =>
            quickHandsRoll(hero).map { case (consumed, qhLog) =>
              // Святая вода держится до конца боя. Ячейка пустеет, но помнит
              // склянку: после боя доберёт такую же из сумки.
              TurnResult(withBelt(hero, b.updated(idx, h.poured)),
                battle.copy(holyWater = true, consumableUsedThisRound = consumed),
                Vector(content.text("battle.holyWater.poured")) ++ qhLog, Outcome.Continue, endsRound = false)
            }
        }
      case _ => cont(hero, battle, content.text("battle.belt.none"))
    }

  /** Годится ли ответ на вопрос отвара: травма ещё на герое и не тяжёлая.
    * Отвару, который ничего не спрашивает, годится любой. */
  private def pickValid(hero: Hero, kind: BrewKind, pick: Option[String], nowMs: Long): Boolean =
    kind.effect match {
      case BrewEffect.CureTrauma =>
        pick.flatMap(Trauma.byName)
          .exists(t => t.severity != Trauma.Heavy && hero.activeTraumas(nowMs).contains(t))
      case _ => true
    }

  /** Почему этим отваром сейчас не воспользоваться: лечить нечего, мазать
    * нечем, заправлять некуда. Ячейка при этом остаётся полной, а раунд —
    * нетронутым. */
  private def brewRefusal(hero: Hero, kind: BrewKind, nowMs: Long): Option[String] =
    kind.effect match {
      case BrewEffect.CureTrauma =>
        val active = hero.activeTraumas(nowMs)
        if (active.isEmpty) Some(content.text("battle.belt.noTrauma"))
        else if (active.forall(_.severity == Trauma.Heavy)) Some(content.text("battle.belt.onlyHeavy"))
        else None
      case BrewEffect.Coat(_, _) if hero.equipment.unarmed =>
        Some(content.text("battle.belt.noWeapon"))
      case BrewEffect.RefillFlask(sources) if refillableFlask(hero, sources).isEmpty =>
        Some(content.text("battle.belt.noFlask"))
      case _ => None
    }

  /** Какую флягу на поясе заправит отвар: своего толка и не полную. */
  private def refillableFlask(hero: Hero, sources: Set[RefillSource]): Option[(Int, BeltSlot.Flask)] =
    hero.equipment.belt.details match {
      case b: ItemDetails.Belt =>
        b.slots.zipWithIndex.collectFirst {
          case (f: BeltSlot.Flask, i)
            if sources.contains(f.kind.effect.refillSource) && f.charges < f.maxCharges => (i, f)
        }
      case _ => None
    }

  /** Отвар из ячейки пояса. Герой приходит уже с пустой ячейкой, а бой — с
    * отметкой о расходнике. Делает то же, что сделал бы глоток с карточки, —
    * только посреди боя. Чистый расчёт, как и всякий боевой ход: бафы, травмы и
    * быстрые отдыхи едут в результате хода, а пишет их commit (см.
    * [[persistExtras]]). */
  private def brewEffect(
      hero: Hero,
      battle: SoloPveBattle,
      kind: BrewKind,
      nowMs: Long,
      qhLog: Vector[String],
      pick: Option[String]
  ): TurnResult = {
    def done(h: Hero, b: SoloPveBattle, lines: Vector[String]): TurnResult = {
      val outcome = if (b.monsterCurrentHp <= 0L) Outcome.Victory else Outcome.Continue
      val after   = if (outcome == Outcome.Victory && b.promoteNext.isEmpty) endOfRoundHero(h, nowMs) else h
      TurnResult(after, b, lines ++ qhLog, outcome, endsRound = false)
    }
    def boosted(name: String, buff: ParamsBuff, until: Long, line: String): TurnResult =
      done(hero.copy(statBoosts = hero.statBoosts.add(StatBoost(name, buff, until), nowMs)), battle, Vector(line))
    kind.effect match {
      // Боевой отвар: ровно то, что делала склянка того же толка.
      case BrewEffect.Potion(potion) =>
        val (stats, next, msg) = applyPotion(hero, battle, potion, kind.label, nowMs)
        done(hero.copy(fightStats = stats), next, Vector(msg))

      // Грибная смесь: лопается под ногами, достаётся всем, кто стоит в поле.
      case BrewEffect.Bomb =>
        val blow             = BrewRates.MushroomDamagePerLvl * hero.lvl
        val (swept, _, foes) = sweep(battle, blow, None, poisons = true)
        done(hero, swept, Vector(content.format("battle.mixThrown",
          "damage" -> blow.toString, "foes" -> foes.toString)))

      // Смазка на клинок: держится до конца боя, как и наложенная до него.
      case BrewEffect.Coat(coat, _) =>
        val key = coat match {
          case WeaponCoat.Poison => "battle.belt.coatPoison"
          case WeaponCoat.Bleed  => "battle.belt.coatBleed"
        }
        done(hero.copy(weaponDust = hero.weaponDust.copy(coat = Some(coat))), battle,
          Vector(content.format(key, "weapon" -> hero.equipment.mainWeapon.name)))

      // Костоправный отвар: названную травму снимает прямо в сече.
      case BrewEffect.CureTrauma =>
        pick.flatMap(Trauma.byName)
          .filter(t => t.severity != Trauma.Heavy && hero.activeTraumas(nowMs).contains(t)) match {
          case None    => done(hero, battle, Vector(content.text("battle.belt.noTrauma")))
          case Some(t) =>
            val names = BattleState.withoutFirst(hero.traumaNames, t.name)
            val until = if (names.isEmpty) None else hero.traumaUntil
            done(hero.copy(traumaUntil = until, traumaNames = names), battle,
              Vector(content.format("battle.belt.cured", "trauma" -> t.name)))
        }

      // Заправка: отвар уходит во флягу своего толка, что лежит рядом в ячейке.
      case BrewEffect.RefillFlask(sources) =>
        (refillableFlask(hero, sources), hero.equipment.belt.details) match {
          case (Some((i, f)), b: ItemDetails.Belt) =>
            done(withBelt(hero, b.updated(i, f.refilled)), battle,
              Vector(content.format("battle.belt.flaskRefilled", "flask" -> f.name)))
          case _ => done(hero, battle, Vector(content.text("battle.belt.noFlask")))
        }

      // Бодрящий сбор: быстрые отдыхи ложатся в запас, как и выпитый у костра.
      case BrewEffect.InstantRest =>
        done(hero, battle, Vector(content.format("battle.belt.rests",
          "count" -> BrewRates.InstantRests.toString))).copy(instantRests = BrewRates.InstantRests)

      // Часовой баф: складывается с зельями Густаво, как и выпитый до боя.
      case BrewEffect.Boost(name, buff, label) =>
        boosted(name, buff, nowMs + BrewRates.BoostDurationMs,
          content.format("battle.belt.boosted", "brew" -> kind.label,
            "pct" -> BrewRates.BoostPct.toString, "stat" -> label))

      // Зеркальный настой: копии встают тут же, в этом бою.
      case BrewEffect.Mirror(copies) =>
        done(hero, battle.copy(effects = battle.effects.copy(heroMirrors = copies)),
          Vector(content.format("battle.belt.mirrors", "count" -> copies.toString)))

      // Волчий зов: запах держится час и сгорит на ближайшей встрече.
      case BrewEffect.WolfCall =>
        boosted(BrewRates.WolfCallBoost, ParamsBuff.zero, nowMs + BrewRates.WolfCallMs,
          content.text("battle.belt.wolfCall"))

      // Гнилой зов — узелок: смрад ведёт к Гнилому Джо, ближайшая встреча — он.
      case BrewEffect.RotCall =>
        boosted(BrewRates.RotCallBoost, ParamsBuff.zero, nowMs + BrewRates.RotCallMs,
          content.text("battle.belt.rotCall"))

      // Пузырь: срока у плёнки нет — она ждёт смерти, сколько бы её ни ждать.
      case BrewEffect.Bubble =>
        boosted(BrewRates.BubbleBoost, ParamsBuff.zero, Long.MaxValue, content.text("battle.belt.bubble"))

      // В ячейку эти не ложатся вовсе (см. BrewKind.beltable): шнапс с самогоном
      // носят Трактирщику, а приговор пьют до боя — с кем встретиться, решают
      // не посреди сечи. На Inert же не сидит ни один отвар.
      case BrewEffect.Sellable(_) | BrewEffect.Sentence | BrewEffect.Inert =>
        done(hero, battle, Vector(content.text("battle.belt.notInBattle")))
    }
  }

  /** Эффект боевого отвара — того, что пришёл на место склянки. Мгновенные
    * (лечение, броня, энергия) меняют статы героя; временные (атака, защита,
    * уворот) кладут баф на [[BrewRates.PotionBuffRounds]] раундов; яд и реген
    * ставят свой статус-эффект. Возвращает новые статы, обновлённый бой и
    * строку эффекта — записывает всё commit. */
  private def applyPotion(
      hero: Hero,
      battle: SoloPveBattle,
      potion: PotionKind,
      brew: String,
      nowMs: Long
  ): (FightStats, SoloPveBattle, String) = {
    val buffed = battle.heroBattleState.applyTo(hero.effectiveFightStats(nowMs))
    val rounds = BrewRates.PotionBuffRounds
    potion match {
      case PotionKind.Healing =>
        val maxHp = hero.effectiveMaxHp(nowMs)
        // «Целитель» множит лечение отвара на 1.1, дальше — общий расчёт.
        val full  = (maxHp * BrewRates.PotionHealPct / 100L * hero.passives.healMult).toLong.max(1L)
        val (newHp, healed, healedBattle, cutLine) = activeHeal(hero, battle, full, nowMs)
        val line  = content.format("battle.beltHeal", "brew" -> brew,
                      "healed" -> healed.toString, "hp" -> newHp.toString, "max" -> maxHp.toString)
        (hero.fightStats.copy(hp = newHp), healedBattle, (line +: cutLine.toVector).mkString("\n"))

      case PotionKind.Metal =>
        val maxArmor = hero.effectiveMaxArmor(nowMs)
        val restore  = (maxArmor * BrewRates.PotionArmorPct / 100L).max(1L)
        val newArmor = (hero.fightStats.armor + restore).min(maxArmor).max(hero.fightStats.armor)
        (hero.fightStats.copy(armor = newArmor), battle,
          content.format("battle.beltMetal", "brew" -> brew,
            "armor" -> (newArmor - hero.fightStats.armor).toString,
            "cur" -> newArmor.toString, "max" -> maxArmor.toString))

      case PotionKind.Poison =>
        (hero.fightStats, battle.copy(effects = battle.effects.copy(heroPoisonousAttacks = true)),
          content.format("battle.beltPoison", "brew" -> brew))

      case PotionKind.Regeneration =>
        (hero.fightStats, battle.copy(effects = battle.effects.copy(heroRegen = Some(Regen.onDrink))),
          content.format("battle.beltRegen", "brew" -> brew, "pct" -> BrewRates.PotionRegenPct.toString))

      case PotionKind.Evasion =>
        (hero.fightStats,
          battle.copy(heroBattleState = battle.heroBattleState.add(
            Buff(0L, 0L, 0L, dodgePct = BrewRates.PotionDodgePct.toLong, defencePct = 0L, turnsLeft = Some(rounds)))),
          content.format("battle.beltEvasion", "brew" -> brew,
            "pct" -> BrewRates.PotionDodgePct.toString, "rounds" -> rounds.toString))

      case PotionKind.Energy =>
        val maxEn     = hero.maxEnergy(nowMs)
        val restore   = (maxEn * BrewRates.PotionEnergyPct / 100L).max(1L)
        val newEnergy = (hero.fightStats.energy + restore).min(maxEn).max(hero.fightStats.energy)
        (hero.fightStats.copy(energy = newEnergy), battle,
          content.format("battle.beltEnergy", "brew" -> brew,
            "energy" -> (newEnergy - hero.fightStats.energy).toString,
            "cur" -> newEnergy.toString, "max" -> maxEn.toString))

      case PotionKind.Attack =>
        val bonus = (buffed.atk * BrewRates.PotionAtkPct / 100L).max(1L)
        (hero.fightStats,
          battle.copy(heroBattleState = battle.heroBattleState.add(
            Buff(atk = bonus, 0L, 0L, dodgePct = 0L, defencePct = 0L, turnsLeft = Some(rounds)))),
          content.format("battle.beltAttack", "brew" -> brew,
            "atk" -> bonus.toString, "rounds" -> rounds.toString))

      case PotionKind.Defence =>
        val bonus = (buffed.defence * BrewRates.PotionDefencePct / 100L).max(1L)
        (hero.fightStats,
          battle.copy(heroBattleState = battle.heroBattleState.add(
            Buff(0L, 0L, defence = bonus, dodgePct = 0L, defencePct = 0L, turnsLeft = Some(rounds)))),
          content.format("battle.beltDefence", "brew" -> brew,
            "defence" -> bonus.toString, "rounds" -> rounds.toString))
    }
  }

  /** Горсть пыли на оружие прямо в бою: правила те же, что у наковальни —
    * слой ложится, противоположная стихия даёт всполох, четвёртая горсть
    * сбивает покрытие (см. [[WeaponDust.sprinkle]]). */
  private def sprinkleInBattle(hero: Hero, dust: pangea.model.item.MaterialKind): (Hero, String) = {
    val outcome = WeaponDust.sprinkle(dust, hero.weaponDust, hero.equipment.handGems)
    val line = outcome match {
      case _: WeaponDust.Outcome.Applied  => content.format("battle.belt.dustApplied", "dust" -> dust.displayName)
      case _: WeaponDust.Outcome.Clash    => content.format("battle.belt.dustClash", "dust" -> dust.displayName)
      case _: WeaponDust.Outcome.Overload => content.format("battle.belt.dustOverload", "dust" -> dust.displayName)
    }
    (hero.copy(weaponDust = outcome.next), line)
  }

  /** Опустевшие ячейки пояса добирают из сумки то же, чем были полны: горсть
    * пыли, склянку отвара, такой же цветок. Чего не нашлось, остаётся пустым —
    * игрок доложит сам. */
  private def refillBelt(user: User, hero: Hero): Task[List[String]] =
    hero.equipment.belt.details match {
      case b: ItemDetails.Belt if b.slots.exists(BattleState.refillable) =>
        for {
          inv <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
          // Идём по ячейкам, забирая из сумки по одной вещи: две пустые ячейки
          // одного вида не должны разобрать одну и ту же щепоть дважды.
          taken = b.slots.foldLeft((List.empty[BeltSlot], List.empty[Item], inv.items.data)) {
                    case ((slots, used, bag), d: BeltSlot.Dust) if d.spent =>
                      bag.find(_.material.contains(d.kind)) match {
                        case Some(item) => (slots :+ d.refilled, used :+ item, bag.filterNot(_.id == item.id))
                        case None       => (slots :+ d, used, bag)
                      }
                    case ((slots, used, bag), br: BeltSlot.Brew) if br.spent =>
                      bag.find(_.brew.contains(br.kind)) match {
                        case Some(item) => (slots :+ br.refilled, used :+ item, bag.filterNot(_.id == item.id))
                        case None       => (slots :+ br, used, bag)
                      }
                    case ((slots, used, bag), r: BeltSlot.Rose) if r.withered =>
                      bag.find(_.rose.exists(_.kind == r.kind)) match {
                        case Some(item) => (slots :+ r.refilled, used :+ item, bag.filterNot(_.id == item.id))
                        case None       => (slots :+ r, used, bag)
                      }
                    case ((slots, used, bag), h: BeltSlot.HolyWater) if h.spent =>
                      bag.find(_.material.contains(pangea.model.item.MaterialKind.HolyWater)) match {
                        case Some(item) => (slots :+ h.refilled, used :+ item, bag.filterNot(_.id == item.id))
                        case None       => (slots :+ h, used, bag)
                      }
                    case ((slots, used, bag), other) => (slots :+ other, used, bag)
                  }
          (slots, used, _) = taken
          _ <- ZIO.when(used.nonEmpty)(
                 inventoryRepo.removeItems(used.map(_.id).toSet, hero.id)
                   .mapError(e => new Throwable(e.toString)) *>
                   heroDao.updateEquipment(user.userId, hero.equipment.copy(
                     belt = hero.equipment.belt.copy(details = b.copy(slots = slots)))))
        } yield used.map(_.name)
      case _ => ZIO.succeed(Nil)
    }

  /** Пояс героя с новым содержимым. */
  private def withBelt(hero: Hero, belt: ItemDetails.Belt): Hero =
    hero.copy(equipment = hero.equipment.copy(
      belt = hero.equipment.belt.copy(details = belt)))

  /** Экран подтверждения бегства (чистая навигация, состояние не меняется). */
  /** Кнопка миазм: отпустить тьму или свернуть её. Выбор держится до
    * следующего нажатия, а не до конца боя (см. BattlePrefs). Ход он не тратит —
    * это не действие, а переключатель. */
  private def toggleMiasma(user: User, renderer: Renderer): Task[StateType] =
    for {
      prefs <- BattleState.prefs(heroDao, user.userId)
      next   = prefs.withMiasma(!prefs.miasma)
      _     <- heroDao.writeBattlePrefs(user.userId, next.asJson)
      _     <- renderer.show(user, Screen(
                 content.text(if (next.miasma) "battle.miasmaOn" else "battle.miasmaOff"), Nil))
      _     <- showScreen(user, renderer)
    } yield StateType.Battle

  private def flee(user: User, renderer: Renderer): Task[StateType] =
    getBattle(user).flatMap { battle =>
      // Западня: место и время выбирал не герой, и уйти отсюда некуда.
      if (battle.noFlee)
        renderer.show(user, Screen(content.format("battle.noFlee", "monster" -> battle.monsterName), Nil))
          .as(StateType.Battle)
      else renderer.show(user, content.screen("battle.fleeConfirm")).as(StateType.Battle)
    }

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
        // Напротив никого — бить в спину некому. На арене тоже: соперник —
        // живой игрок, и вдогонку он не бьёт, его ход ещё не наступил.
        else if (battle.unpaired || battle.arena.isDefined)
          ZIO.succeed(TurnResult(hero, battle, Vector.empty, Outcome.Fled))
        else if (hitRoll > playerDodgeChance(hero, battle, nowMs, fleeing = true)) {
          for {
            spread <- Random.nextLongBetween(80L, 121L)
            rawDamage = (monster.fightStats.atk * spread / 100L).max(1L)
            reduction = heroDamageReduction(hero, battle, buffedEff, monster.fightStats.atk, nowMs)
            baseReduced = (rawDamage * (1.0 - reduction)).toLong.max(1L)
            // «Непробиваемый»: 20% шанс срезать урон обычной атаки вдвое.
            impenTriggered <- chanceRoll(hero.passives.hasImpenetrable, PassiveKind.Impenetrable.TriggerPct)
            reducedDamage =
              if (impenTriggered) (baseReduced * (100L - hero.passives.impenetrableReductionPct) / 100L).max(1L)
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
        // Замену к герою не подставляем — просто говорим, что место опустело.
        val line = content.format("battle.group.fell", "monster" -> res.battle.monsterName)
        res.copy(battle = next, outcome = Outcome.Continue, log = res.log :+ line)
    }

  /** Ход ауры в конце раунда (см. [[Aura]]). Аура держит ВСЁ поле боя и
    * достаётся каждому, кто на нём стоит, — своим и чужим одинаково, по природе
    * существа ([[Nature]]). Герой держит её силой: доля энергии уходит за раунд
    * и она же идёт в силу ауры, так что к концу схватки аура слабеет сама.
    * Энергии меньше порога — держать нечем, аура оседает.
    *
    * В логе аура говорит о себе ОДНОЙ строкой на всё поле: перечислять, кому
    * сколько досталось, бессмысленно — в групповом бою это двадцать строк. Своё
    * герой видит отдельно: его-то лечение или урон ему и важны. Павший от ауры
    * союзник тоже сказан — это событие, а не сдача.
    *
    * Герой-некромант на пороге 10 сам числится нежитью, поэтому своей же аурой
    * лечится.
    *
    * Святая вода и миазмы гасят друг друга: пока на поле держится тьма — чья
    * угодно, героя или Некроманта, — не работает ни она, ни вода. Свернул герой
    * миазмы, выдохся или пал Некромант — вода начинает жечь мёртвых. */
  private def auraTick(res: TurnResult, on: Boolean): TurnResult =
    if (res.battle.holyWater && miasmaHeld(res, on)) holyClash(res)
    else {
      val byHero = res.hero.sets.auras.foldLeft(res)((acc, aura) => heroAura(acc, aura, on))
      // Чужая аура: её держит моб, и платит он своей энергией. Правила те же — и
      // она так же не разбирает, кто на поле свой.
      val byMob = byHero.battle.auraHolder.fold(byHero) { case (aura, _) => mobAura(byHero, aura) }
      if (byMob.battle.holyWater) holyTick(byMob) else byMob
    }

  /** Держатся ли на поле миазмы в этом раунде: герой их не свернул и ему есть
    * чем платить — или их держит моб, у которого хватает энергии. Осевшие
    * миазмы святую воду не гасят. */
  private def miasmaHeld(res: TurnResult, on: Boolean): Boolean = {
    val miasma = Aura.Miasma
    val byHero = on && res.hero.sets.auras.contains(miasma) && res.hero.fightStats.energy >= miasma.minEnergy
    val byMob  = res.battle.auraHolder.exists { case (aura, energy) => aura == miasma && energy >= aura.minEnergy }
    byHero || byMob
  }

  /** Святая вода и миазмы встретились: обе молчат, и строка об этом одна. */
  private def holyClash(res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Continue || !res.endsRound) res
    else res.copy(log = res.log :+ content.text("battle.holyWater.clash"))

  /** Святая вода на поле: каждый мёртвый — свой, чужой, сам герой-нежить —
    * теряет долю максимума HP. Живых и камня она не касается, а нет на поле
    * мёртвых — молчит, чтобы не сорить строкой каждый раунд. */
  private def holyTick(res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Continue || !res.endsRound || !undeadOnField(res)) res
    else applyAura(res, Aura.HolyWater, 0L,
      Vector(content.format("battle.aura.holyWater.holds", "pct" -> AuraRates.HolyWaterPct.toString)))

  /** Есть ли на поле хоть один мёртвый: герой, союзник или моб. */
  private def undeadOnField(res: TurnResult): Boolean = {
    val b = res.battle
    res.hero.sets.heroIsUndead ||
      b.group.allies.exists(a => a.alive && Nature.of(a.kind.race) == Nature.Undead) ||
      (b.monsterCurrentHp > 0L && Nature.of(b.monsterRace, b.monsterUndead) == Nature.Undead) ||
      b.group.others.exists(s => s.alive && Nature.of(s.race, s.undead) == Nature.Undead)
  }

  /** Аура, которую держит моб (Некромант): энергия его, правила общие. Энергии
    * не хватает — аура оседает, и строка об этом одна на всё поле. */
  private def mobAura(res: TurnResult, aura: Aura): TurnResult =
    if (res.outcome != Outcome.Continue || !res.endsRound) res
    else res.battle.auraHolder match {
      case None => res
      case Some((_, energy)) if energy < aura.minEnergy =>
        res.copy(log = res.log :+ content.text(s"battle.aura.${aura.key}.mobFades"))
      case Some((_, energy)) =>
        val spent  = aura.cost(energy)
        val paid   = res.copy(battle = res.battle.spendAuraEnergy(spent))
        val lines  = Vector(content.format(s"battle.aura.${aura.key}.mobHolds", "energy" -> spent.toString))
        applyAura(paid, aura, spent, lines)
    }

  private def heroAura(res: TurnResult, aura: Aura, on: Boolean): TurnResult =
    if (res.outcome != Outcome.Continue || !res.endsRound || !on) res
    else if (res.hero.fightStats.energy < aura.minEnergy)
      res.copy(log = res.log :+ content.text(s"battle.aura.${aura.key}.fades"))
    else {
      val spent = aura.cost(res.hero.fightStats.energy)
      val paid  = res.copy(hero = res.hero.copy(fightStats = res.hero.fightStats.copy(
        energy = (res.hero.fightStats.energy - spent).max(0L))))
      applyAura(paid, aura, spent,
        Vector(content.format(s"battle.aura.${aura.key}.holds", "energy" -> spent.toString)))
    }

  /** Сама аура: каждому на поле по его природе — герою, союзникам и чужой
    * стороне. Энергию держатель к этому моменту уже заплатил, `head` — строка о
    * самой ауре. Своё герой видит отдельной строкой, остальные — молча: в
    * групповом бою это были бы двадцать строк. */
  private def applyAura(res: TurnResult, aura: Aura, spent: Long, head: Vector[String]): TurnResult = {
    def power(maxHp: Long): Long = aura.power(maxHp, spent)

    // ── Герой ─────────────────────────────────────────────────────────────────
    val heroNature = if (res.hero.sets.heroIsUndead) Nature.Undead else Nature.of(res.hero.race)
    // Лежачего аура не трогает — ни лечит, ни жжёт: встанет он, когда отряд
    // отобьётся, а до тех пор его здоровье — ноль.
    val heroSign   = if (res.battle.group.heroDown) 0 else aura.sign(heroNature)
    val maxHp      = res.hero.effectiveMaxHp(0L)
    val heroHp     = heroSign match {
      // Лечение не опускает: текущее HP выше потолка аура не срезает.
      case s if s > 0 => (res.hero.fightStats.hp + power(maxHp)).min(maxHp).max(res.hero.fightStats.hp)
      case s if s < 0 => (res.hero.fightStats.hp - power(maxHp)).max(0L)
      case _          => res.hero.fightStats.hp
    }
    val heroDelta = heroHp - res.hero.fightStats.hp
    val hero1     = res.hero.copy(fightStats = res.hero.fightStats.copy(hp = heroHp))

    // ── Союзники: та же мера, по их природе ───────────────────────────────────
    val allies = res.battle.group.allies.map { a =>
      val sign = if (!a.alive) 0 else aura.sign(Nature.of(a.kind.race))
      sign match {
        case s if s > 0 => a.copy(hp = (a.hp + power(a.stats.hp)).min(a.stats.hp).max(a.hp))
        case s if s < 0 => a.copy(hp = (a.hp - power(a.stats.hp)).max(0L))
        case _          => a
      }
    }
    val fellAllies  = allies.filter(a => !a.alive && res.battle.group.allyAt(a.position).exists(_.alive))
    val withAllies  = res.battle.copy(group = res.battle.group.copy(allies = allies))
    val sweptAllies = fellAllies.foldLeft(withAllies)((b, a) => b.copy(group = b.group.withoutAlly(a.position)))

    // ── Чужая сторона ─────────────────────────────────────────────────────────
    val battle2 = auraOnMonsters(sweptAllies, aura, power)
    val lines = head ++
      Option.when(heroDelta > 0L)(
        content.format(s"battle.aura.${aura.key}.feeds", "hp" -> heroDelta.toString)).toVector ++
      Option.when(heroDelta < 0L)(
        content.format(s"battle.aura.${aura.key}.burns", "hp" -> (-heroDelta).toString)).toVector ++
      fellAllies.map(a => content.format("battle.ally.auraFell", "name" -> a.name))
    res.copy(hero = hero1, battle = battle2, log = res.log ++ lines)
  }

  /** Аура по чужой стороне: каждому по его природе, павшие уходят в добычу.
    * Строк не даёт — о себе аура сказала одной на всё поле. */
  private def auraOnMonsters(battle: SoloPveBattle, aura: Aura, power: Long => Long): SoloPveBattle = {
    def touched(sign: Int, cur: Long, maxHp: Long): Long = sign match {
      case s if s > 0 => (cur + power(maxHp)).min(maxHp).max(cur)
      case s if s < 0 => (cur - power(maxHp)).max(0L)
      case _          => cur
    }
    // Активный: его HP живут в полях боя.
    val afterActive = {
      val sign = if (battle.monsterCurrentHp <= 0L) 0 else aura.sign(Nature.of(battle.monsterRace, battle.monsterUndead))
      battle.copy(monsterCurrentHp = touched(sign, battle.monsterCurrentHp, battle.monsterStats.hp))
    }
    // Слоты строя: павших убираем с конца, чтобы индексы не плыли.
    val slots = afterActive.group.others.zipWithIndex.map { case (slot, idx) =>
      val sign = if (!slot.alive) 0 else aura.sign(Nature.of(slot.race, slot.undead))
      val hp   = touched(sign, slot.currentHp, slot.stats.hp)
      (slot.copy(currentHp = hp), idx, hp <= 0L && slot.alive)
    }
    val withSlots = afterActive.copy(group = afterActive.group.copy(others = slots.map(_._1)))
    slots.filter(_._3).map(_._2).sorted.reverse.foldLeft(withSlots)((b, idx) => b.sideFallen(idx))
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

  private def allyTurn(a: BattleAlly, battle: SoloPveBattle): Task[(SoloPveBattle, Vector[String])] =
    for {
      // 1) обычная атака по цели
      struck <- battle.allyTarget(a) match {
        case None      => ZIO.succeed(AllyBlow(battle, None, 0L, slew = false))
        case Some(pos) => allyHits(a, battle, pos, a.stats.atk, crushing = false)
      }
      b1 = struck.battle
      // 2) случайное умение из тех, что по карману и к месту
      a1 = b1.group.allies.find(_.kind == a.kind).getOrElse(a)
      target2 = b1.allyTarget(a1)
      // Крыса умений не знает — только кусает (см. AllyKind.usesSkills).
      // Удар попадает в выбор только вместе с целью — без неё его и нет.
      options = if (!a1.kind.usesSkills) Nil else AllySkill.values.toList.flatMap {
        case AllySkill.QuickStrike     => target2.map(BattleState.AllyCast.Strike(_, crushing = false))
        case AllySkill.CrushingStrike  => target2.map(BattleState.AllyCast.Strike(_, crushing = true))
        case AllySkill.HealingFlask    => Option.when(a1.hp < a1.stats.hp)(BattleState.AllyCast.Flask)
        case AllySkill.EmergencyRepair => Option.when(a1.armor < a1.stats.armor)(BattleState.AllyCast.Repair)
      }.filter(_.skill.cost(a1.lvl) <= a1.energy)
      casted <-
        if (options.isEmpty) ZIO.succeed((AllyBlow(b1, None, 0L, slew = false), Vector.empty[String]))
        else Random.nextIntBetween(0, options.size).flatMap { i =>
          val cast = options(i)
          val paid = b1.copy(group = b1.group.updateAlly(a1.position)(x => x.copy(energy = x.energy - cast.skill.cost(a1.lvl))))
          cast match {
            case BattleState.AllyCast.Strike(target, crushing) =>
              val factor = if (crushing) AllySkill.CrushingStrikeFactor else AllySkill.QuickStrikeFactor
              allyHits(a1, paid, target, (a1.stats.atk * factor).toLong.max(1L),
                crushing = crushing, sure = true).map(_ -> Vector.empty[String])
            case BattleState.AllyCast.Flask =>
              val heal = (a1.stats.hp * AllySkill.HealPct / 100L).max(1L)
              val newHp = (a1.hp + heal).min(a1.stats.hp)
              // Кровь и пламя фляга снимает целиком, яд ослабляет — как у мобов.
              ZIO.succeed((AllyBlow(paid.copy(group = paid.group.updateAlly(a1.position)(
                  x => x.copy(hp = newHp, poison = x.poison.flatMap(_.weakenedByHeal), bleed = None, burn = None))),
                  None, 0L, slew = false),
                Vector(content.format("battle.ally.healingFlask", "name" -> a1.name, "hp" -> (newHp - a1.hp).toString))))
            case BattleState.AllyCast.Repair =>
              val fix = (a1.stats.armor * AllySkill.RepairPct / 100L).max(1L)
              val newArmor = (a1.armor + fix).min(a1.stats.armor)
              ZIO.succeed((AllyBlow(paid.copy(group = paid.group.updateAlly(a1.position)(_.copy(armor = newArmor))), None, 0L, slew = false),
                Vector(content.format("battle.ally.emergencyRepair", "name" -> a1.name, "armor" -> (newArmor - a1.armor).toString))))
          }
        }
      (blow2, aidLog) = casted
      b2 = blow2.battle
      // 3) энергия копится
      b3 = b2.copy(group = b2.group.updateAlly(a.position)(x =>
             x.copy(energy = (x.energy + x.kind.energyRegen(x.lvl)).min(x.stats.energy))))
      // Лог — одной строкой на цель, с суммарным уроном удара и умения; проки и
      // раны считаются, но экран не засоряют. Промах — только если не попало ничем.
      blows   = List(struck, blow2).flatMap(b => b.target.map(name => (name, b.damage, b.slew)))
      byName  = blows.groupMapReduce(_._1)(_._2)(_ + _)
      order   = blows.map(_._1).distinct
      hitLog  = order.map { m =>
        val dmg = byName(m)
        if (dmg > 0L) content.format("battle.ally.hit", "name" -> a.name, "monster" -> m, "damage" -> dmg.toString)
        else content.format("battle.ally.miss", "name" -> a.name, "monster" -> m)
      }.toVector
      slewLog = blows.collect { case (m, _, true) => content.format("battle.ally.slew", "name" -> a.name, "monster" -> m) }.toVector
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
          // Крыса бьёт зубами, без стихии: грани урона у неё обычные, а боссовое
          // сопротивление — как у голой стали (см. AllyKind.element).
          resist  = tmp.boss.map(b => elem.fold(b.plainDamageTakenMult)(b.damageTakenMult)).getOrElse(1.0)
          dmg     = (base * resist).toLong.max(1L)
          // Предел босса за раунд (Крысиный король) общий для всех, кто его бьёт:
          // и для героя, и для отряда (см. capBossDamage).
          (armorDmg, hpDmg) = if (crushing) (0L, capBossDamage(tmp, dmg)) else {
            val cut       = capBossDamage(tmp,
                              (dmg * (1.0 - BattleState.defenceReduction(effectiveMonsterDefence(tmp), 0L, a.stats.atk))).toLong.max(1L))
            val rawArmor  = math.min(tmp.monsterCurrentArmor, cut)
            val rawHp     = cut - rawArmor
            val armorMult = elem.map(_.armorMult).getOrElse(1.0)
            val hpMult    = elem.map(_.hpMult).getOrElse(1.0)
            val aDmg      = math.min(tmp.monsterCurrentArmor, (rawArmor * armorMult).toLong).max(0L)
            val lightning = if (elem.contains(Element.Lightning)) (aDmg * Element.Lightning.ArmorToHpFrac).toLong else 0L
            (aDmg, ((rawHp * hpMult).toLong + lightning).max(0L))
          }
          newHp    = (tmp.monsterCurrentHp - hpDmg).max(0L)
          hit      = noteBossDamage(tmp, armorDmg + hpDmg)
                       .copy(monsterCurrentHp = newHp, monsterCurrentArmor = tmp.monsterCurrentArmor - armorDmg)
          dealt    = armorDmg + hpDmg
          // Прок стихии считается молча — строки его в лог союзника не идут.
          procRoll <- if (newHp > 0L) Random.nextIntBetween(1, 101) else ZIO.succeed(100)
          procced  = if (newHp > 0L && procRoll <= Element.ProcChancePct) applyProcs(hit, elem.toSet)._1 else hit
          guarded  = procced.withEffects(procced.effects)
          // обратно: активный — как есть, слот — в строй; добитый слот — в павшие
          result =
            if (active) AllyBlow(guarded, Some(tmp.monsterName), dealt, slew = false)
            else {
              // Счёт предела живёт на бою: босс в бою один, где бы он ни стоял.
              val restored = battle.copy(bossTaken = guarded.bossTaken,
                               group = battle.group.copy(others = battle.group.others.updated(idx, guarded.activeSlot)))
              if (guarded.monsterCurrentHp <= 0L) AllyBlow(restored.sideFallen(idx), Some(tmp.monsterName), dealt, slew = true)
              else AllyBlow(restored, Some(tmp.monsterName), dealt, slew = false)
            }
        } yield result
    } yield out
  }

  /** Моб бьёт союзника: бросок против уклонения союзника, урон с разбросом,
    * защита союзника (холод её обгрызает), броня — потом HP, у каменного —
    * раскол. Сверху — всё то же, что достаётся от удара герою: отравленное
    * оружие травит (мурлок, эльф, Крысиный король), огонь поджигает, холод
    * сковывает, воздух подхватывает самого моба; огонь и холод гасят друг
    * друга. Обнулённый союзник уходит по свитку. */
  private def mobStrikesAlly(a: BattleAlly, tmp: SoloPveBattle): Task[(SoloPveBattle, Vector[String])] =
    for {
      hitRoll <- Random.nextIntBetween(1, 101)
      dodge    = BattleState.dodgeChance(0L, a.stats.evasion, a.stats.defence, tmp.monsterStats.accuracy)
      out <-
        if (hitRoll <= dodge) ZIO.succeed((tmp, Vector(content.format("battle.ally.mobMiss", "monster" -> tmp.monsterName, "name" -> a.name))))
        else for {
          spread <- Random.nextLongBetween(80L, 121L)
          raw      = (monsterAttack(tmp) * spread / 100L).max(1L)
          reduced  = (raw * (1.0 - allyReduction(a, tmp.monsterStats.atk))).toLong.max(1L)
          (armorDmg, hpDmg) = allyHitSplit(tmp, a, reduced)
          // Те же броски, что при ударе по герою (см. plainMobStrike).
          igniteChance = tmp.boss.map(_.heroIgniteChancePct)
                           .getOrElse(if (powderElement(tmp).contains(Element.Fire)) Element.ProcChancePct else 0L)
          ignites <- chanceRoll(igniteChance > 0L && !tmp.effects.chilled, igniteChance)
          burnPct      = tmp.boss.map(_.heroIgniteBurnPct).getOrElse(Burn.Initial)
          procElement  = powderElement(tmp).filter(e => e == Element.Cold || e == Element.Air)
          mobProc <- chanceRoll(procElement.isDefined, Element.ProcChancePct)
          poisons  = hpDmg > 0L && (tmp.effects.monsterPoisonsOnHit ||
                       Race.withNameOption(tmp.monsterRace).exists(_.weaponPoison))
          thawed   = ignites && a.cold
          bites    = mobProc && procElement.contains(Element.Cold)
          quenched = bites && (a.burn.isDefined || (ignites && !thawed))
          airy     = mobProc && procElement.contains(Element.Air)
          struck   = a.copy(armor = a.armor - armorDmg, hp = (a.hp - hpDmg).max(0L))
          burned   =
            if (!ignites) struck
            else if (thawed) struck.copy(coldCut = 0)
            else struck.copy(burn = Some(struck.burn.map(_.reignited).getOrElse(Burn(burnPct))))
          poisoned = if (poisons) burned.copy(poison = Some(burned.poison.map(_.stacked).getOrElse(Poison.onHit))) else burned
          hurt     =
            if (quenched) poisoned.copy(burn = None)
            else if (bites) poisoned.copy(coldCut = poisoned.coldCut + Element.Cold.DefenceReductionCut)
            else poisoned
          after    = if (airy) tmp.copy(effects = tmp.effects.copy(mobAirBoostTurns = Element.Air.ProcTurns)) else tmp
          line     = content.format("battle.ally.mobHit", "monster" -> tmp.monsterName, "name" -> a.name, "damage" -> (armorDmg + hpDmg).toString)
          extras   = List(
            Option.when(ignites && !thawed)(content.format("battle.ally.ignites", "name" -> a.name)),
            Option.when(thawed)(content.format("battle.ally.thaw", "name" -> a.name)),
            Option.when(poisons)(content.format("battle.ally.poisons", "name" -> a.name)),
            Option.when(bites && !quenched)(content.format("battle.ally.coldBite", "name" -> a.name)),
            Option.when(quenched)(content.format("battle.ally.quench", "name" -> a.name)),
            Option.when(airy)(content.text("battle.mobAirBoost"))
          ).flatten
          result   =
            if (hurt.alive) (after.copy(group = after.group.updateAlly(a.position)(_ => hurt)), line +: extras.toVector)
            else (after.copy(group = after.group.withoutAlly(a.position)),
                  // Наёмника уносит свиток, а поднятого с алтаря выносить
                  // нечем: он рассыпается там же, где стоял.
                  Vector(line, content.format(
                    if (a.undead.isDefined) "battle.ally.crumbled" else "battle.ally.scroll",
                    "name" -> a.name)))
        } yield result
    } yield out

  /** Отряд после боя: состояние союзников из боя, выбывшие — кто в отлучку,
    * кто насовсем. Наёмника уносит свиток на сутки, а поднятый с алтаря
    * рассыпается прахом — возвращаться ему неоткуда. Крыса, выскочившая в бою
    * из-под ног (её признак — `home = 0`, в отряде её ещё нет), остаётся при
    * герое на [[pangea.model.squad.AllyRates.RatMs]]. */
  private def squadAfterBattle(hero: Hero, battle: SoloPveBattle, nowMs: Long): pangea.model.squad.Squad = {
    val (joined, fromSquad) = battle.group.allies.partition(_.home <= 0)
    // Позиции могли поменяться за бой (Таран, «Переместиться») — вместе с героем.
    val synced0 = fromSquad.foldLeft(hero.squad.copy(heroPos = battle.group.heroPos)) { (s, a) =>
      s.updateAt(a.home)(old => a.toAlly.copy(hiredUntil = old.hiredUntil))
    }
    // Выжившие, пришедшие не из отряда, — в отряд: крыса на сутки, восставший
    // на двое. Крысе мест не нашлось — разбежится, восставший будет ждать
    // ответа, кем за него пожертвовать (см. Squad.admitRisen).
    val synced = joined.filter(_.alive).foldLeft(synced0) { (s, a) =>
      a.undead.fold(s) { form =>
        if (a.kind == pangea.model.squad.AllyKind.Rat) s.summonRat(form, a.lvl, nowMs)
        else s.admitRisen(form, a.lvl, nowMs)
      }
    }
    // Павших убираем разом: по одному нельзя — строй смыкается, и вторая
    // позиция указала бы уже не на того.
    val (raised, hired) = battle.group.alliesGone.partition(_.undead.isDefined)
    val withoutRaised   = synced.dismissAll(raised.map(_.home).toSet)
    hired.foldLeft(withoutRaised)((s, a) => s.sentAway(a.kind, nowMs))
  }

  /** Ход мобов вне пары. Сосед героя (номер 2) достаёт его сбоку: обычная
    * атака, а поверх — умение, как у моба в паре; лечащее умение он отдаёт
    * раненому соседу, если сам цел. Мобы дальше по строю героя не достают —
    * копят энергию и лечат соседей. Раны на них (яд, кровь, огонь) тикают как у
    * всех; кого добило — уходит в павшие. */
  private def sideMobsPhase(res: TurnResult, nowMs: Long): Task[TurnResult] =
    // На арене чужие бойцы ходят в зеркале соперника, своим ходом, — не здесь.
    if (res.outcome != Outcome.Continue || !(res.battle.isGroup || res.battle.unpaired || res.battle.group.heroDown) ||
        !res.endsRound || res.battle.arena.isDefined) ZIO.succeed(res)
    else {
      val smokeLine = if (res.battle.effects.heroInSmoke) Vector(content.text("battle.flaskSmokeHolds")) else Vector.empty[String]
      // Ходят те, кто стоял в строю к началу фазы, — ищем их по местам: павший
      // смыкает ряды, призванный минибоссом встаёт в строй, и индексы плывут.
      // Пришедшие в этом раунде не бьют — они только что встали.
      ZIO.foldLeft(res.battle.group.places)((res.hero, res.battle, Vector.empty[String])) {
        case ((h, b, log), place) =>
          val idx = b.group.idxOf(place)
          // Герой пал прямо сейчас — мобы замирают; лежит с прошлого раунда — бьют отряд.
          if ((h.fightStats.hp <= 0 && !b.group.heroDown) || idx < 0) ZIO.succeed((h, b, log))
          else freeMobTurn(h, b, idx, nowMs).map { case (h2, b2, lines) => (h2, b2, log ++ lines) }
      }.flatMap { case (h, b, log) =>
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
    * место — соседнее. Минибосс ходит иначе — см. [[bossSlotTurn]]. */
  private def freeMobTurn(hero: Hero, battle: SoloPveBattle, idx: Int, nowMs: Long): Task[(Hero, SoloPveBattle, Vector[String])] = {
    val slot = battle.group.others(idx)
    val pos  = battle.group.posOf(idx)
    if (slot.boss.isDefined) bossSlotTurn(hero, battle, pos, nowMs)
    else {
      // Временный бой: этот моб — «активный», геройская половина эффектов та же;
      // его временные эффекты тикают здесь (общий tickBuffs крутит только пару).
      val tmp0 = battle.withActive(slot)
      val tmp  = tmp0.copy(effects = tickSlotEffects(tmp0.effects))
      freeTurnOf(hero, battle, tmp, pos, nowMs, realActive = false).map { case (h2, b2, ticked, log3) =>
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
  }

  /** Минибосс, стоящий в строю, а не в полях боя. Его круг и счётчики живут на
    * бою, а приёмы меняют сам строй — зов крыс, воскрешение, шаг за спины
    * своих. Поэтому на свой ход он встаёт в поля (как под удар героя, см.
    * [[strikeSide]]), ходит как тот, кто там стоит, и возвращается в строй.
    * Раньше его ход шёл во временном бою, и всё это пропадало: король звал крыс
    * впустую, а Некромант не колдовал вовсе. Истёк ранами — в павшие. */
  private def bossSlotTurn(hero: Hero, battle: SoloPveBattle, pos: Int, nowMs: Long): Task[(Hero, SoloPveBattle, Vector[String])] = {
    val home    = battle.group.activePos
    val engaged = battle.engage(pos)
    // Его временные эффекты тикают здесь, как у всякого моба строя.
    val tmp     = engaged.copy(effects = tickSlotEffects(engaged.effects))
    freeTurnOf(hero, tmp, tmp, pos, nowMs, realActive = false).map { case (h2, b2, ticked, log3) =>
      val merged = mergeFieldsTurn(b2, ticked)
      val boss   = merged.activeSlot
      val at     = merged.group.activePos
      val back   = merged.engage(home)
      if (boss.alive) (h2, back, log3)
      else (h2, back.sideFallen(back.group.idxOf(at)), log3 :+ content.format("battle.group.sideFell", "monster" -> boss.name))
    }
  }

  /** Активный моб, стоящий не напротив героя: ходит как моб вне пары, прямо в
    * полях (его временные эффекты уже оттикали в `tickBuffs`). Истёк ранами —
    * остаётся в полях с нулём HP, промоут дальше по цепочке. */
  private def unpairedActiveTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[(Hero, SoloPveBattle, Vector[String])] =
    freeTurnOf(hero, battle, battle, battle.group.activePos, nowMs, realActive = true).map { case (h2, b2, ticked, log3) =>
      (h2, mergeFieldsTurn(b2, ticked), log3)
    }

  /** Свести ход того, кто стоит в полях боя (см. [[freeTurnOf]]). Лечение соседа
    * правит `battle`-копию, и вылеченного надо забрать оттуда. Но ход минибосса
    * мог изменить и сам строй — зов крыс, поглощение своего; тогда главный он:
    * иначе места остались бы от нового строя, а слоты — от старого, и бой падал
    * бы на первом же обращении к месту. */
  private def mergeFieldsTurn(b2: SoloPveBattle, ticked: SoloPveBattle): SoloPveBattle = {
    val group =
      if (b2.group.others.sizeCompare(ticked.group.others) == 0)
        ticked.group.copy(others = b2.group.others)
      else ticked.group
    ticked.copy(group = group)
  }

  /** Ход моба вне пары, развёрнутого в поля `tmp` на месте `pos`: удар, умение,
    * энергия, тик ран. Бьёт он союзника напротив; нет его — героя, если
    * достаёт; не достаёт — ближайшего союзника в досягаемости: союзника — по тем
    * же правилам, что и героя, сбоку, а эльф — за три места. `realActive` —
    * ходит ли тот, кто стоит в полях по-настоящему, а не встал туда на свой ход.
    * Возвращает героя, `battle` с возможным лечением соседа и `tmp` после хода;
    * собрать это обратно — дело вызывающего. */
  private def freeTurnOf(
      hero: Hero, battle: SoloPveBattle, tmp: SoloPveBattle, pos: Int, nowMs: Long, realActive: Boolean
  ): Task[(Hero, SoloPveBattle, SoloPveBattle, Vector[String])] = {
    // В дыму (дымная фляга) мобы вне пары не видят ни героя, ни друг друга:
    // ни удара сбоку, ни лечения соседу — только раны тикают и энергия копится.
    val smoke       = battle.effects.heroInSmoke
    // Стрелок на башне достаёт любое место и бьёт того, кто к нему ближе.
    // В дыму он не стреляет вовсе: цели не видно (см. дымную флягу).
    val tower       = BattleState.isTower(tmp)
    val reach       = RaceTactics.reach(Race.withName(tmp.monsterRace))
    // Напротив стоит союзник — моб занят им и до героя не тянется; у башни
    // «напротив» нет, поэтому ей ищем ближайшую цель по всему строю.
    val opposite    =
      if (smoke) None
      else if (tower) BattleState.towerTarget(battle, pos)
      else battle.group.allyAt(pos).filter(_.alive)
    // Лежащего героя мобы не добивают — его смерть решится исходом боя. Эльф —
    // стрелок: достаёт героя на три места (см. RaceTactics), прочие — соседа.
    val reachesHero = !smoke && opposite.isEmpty && !battle.group.heroDown &&
                        (tower || battle.group.inReachOf(pos, reach))
    // Ни напротив никого, ни героя не достать — бьёт союзника, которого достаёт.
    val facing      =
      if (smoke || tower || opposite.isDefined || reachesHero) opposite
      else battle.allyWithin(pos, reach)
    for {
      // 1) удар: по союзнику, которого бьёт, либо сбоку по герою, если достаёт
      struck <-
        facing match {
          case Some(ally) => mobStrikesAlly(ally, tmp).map { case (t2, lines) => (hero, t2, lines) }
          case None =>
            if (!reachesHero) ZIO.succeed((hero, tmp, Vector.empty[String]))
            else for {
              hitRoll <- Random.nextIntBetween(1, 101)
              dodge    = playerDodgeChance(hero, tmp, nowMs)
              out <-
                if (hitRoll > dodge)
                  mobStrike(hero, tmp, effWithAir(hero, tmp, nowMs), nowMs).map { st =>
                    val line = content.format("battle.group.sideHit", "monster" -> tmp.monsterName, "damage" -> st.damage.toString)
                    val head = if (st.mirrored) Vector.empty[String] else Vector(line)
                    (hero.copy(fightStats = hero.fightStats.copy(hp = st.newHp, armor = st.newArmor)), st.battle,
                      head ++ st.extraLines)
                  }
                else {
                  val miss = content.format("battle.group.sideMiss", "monster" -> tmp.monsterName)
                  // Контрудар и по соседу: увернулся от удара сбоку — бьёт в ответ, наверняка.
                  if (tmp.heroCounterReady)
                    playerStrike(hero, tmp.copy(heroCounterReady = false), nowMs, Vector.empty, Set.empty,
                      retaliate = false, sureHit = true)
                      .map(res => (res.hero, res.battle, Vector(miss, content.text("battle.counter")) ++ res.log))
                  else ZIO.succeed((hero, tmp, Vector(miss)))
                }
            } yield out
        }
      (h0, t1, log0) = struck
      // Контрудар один на раунд: отработал по этому мобу — снят и с самого боя.
      countered = tmp.heroCounterReady && !t1.heroCounterReady
      sideTrauma <- hitTrauma(hero, h0, nowMs)
      (h1, sideTraumaLine) = sideTrauma
      log1 = log0 ++ sideTraumaLine.toVector
      // 2) умение: раненому соседу — лечение, себе — что по карману, герою — урон
      //    (минибосс вне пары идёт по своему кругу). Герой пал прямо сейчас —
      //    мобы замирают; лежит с прошлого раунда — колдуют по отряду, как и бьют
      //    его (см. sideMobsPhase). Раньше у лежачего ноль HP глушил умения всем.
      casted <-
        if (smoke || (h1.fightStats.hp <= 0 && !battle.group.heroDown) || t1.monsterCurrentHp <= 0 ||
            t1.effects.monsterSkillBlockedTurns > 0)
          ZIO.succeed((h1, t1, battle, Vector.empty[String]))
        else allyAid(h1, t1, battle, pos, nowMs).flatMap {
          case Some((t2, b2, line)) => ZIO.succeed((h1, t2, b2, Vector(line)))
          case None =>
            // Кто держит позицию (Некромант), колдует в ход героя (`quietPhase`),
            // если стоит в полях боя: здесь ему ход не достаётся, иначе за раунд
            // вышло бы два. Герой лежит — его хода нет; стоит Некромант в строю —
            // тот ход не его. Тогда он колдует здесь.
            val holdsOwn = t1.boss.exists(_.holdsPosition) && realActive && !battle.group.heroDown
            // Минибосс, который дерётся в строю, идёт по своему кругу и не
            // доставая героя: зов крыс и объединение — про строй, а не про
            // героя, и ждать свободного места напротив ему незачем. Прикрыли его
            // союзником — круг всё равно идёт.
            val inFormation = t1.boss.exists(_.fightsInGroup)
            // Бьёт он того же, кого бил с руки: союзника, а нет его — героя.
            val prey = facing.fold[Victim](Victim.Hero)(a => Victim.Ally(a.position))
            if (!holdsOwn && (reachesHero || inFormation || facing.isDefined))
              (if (t1.boss.isDefined) bossTurnCast(h1, t1, nowMs, prey) else mobSkillCast(h1, t1, nowMs, prey)).map {
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
    } yield (h2, if (countered) b2.copy(heroCounterReady = false) else b2, ticked, log3)
  }

  /** Арена: раны на бойцах соперника — и на нём самом, и на его союзниках —
    * тикают в конце хода стороны, которая их наложила: ход за ход, как у мобов
    * в лабиринте в их ход. Сроки эффектов на том, кто в полях, уже оттикали в
    * `tickBuffs`; на строе — тикают здесь. Добитого ранами — в павшие. */
  private def arenaWounds(res: TurnResult): TurnResult =
    if (res.outcome != Outcome.Continue || !res.endsRound || res.battle.arena.isEmpty) res
    else {
      val mult = res.hero.sets.burnGrowthMult
      val (active, activeLine, _) = tickMonsterEffects(res.battle, res.battle.monsterName, mult)
      val n = active.group.others.size
      val (b, lines, _) = (0 until n).foldLeft((active, Vector(activeLine).filter(_.nonEmpty), 0)) {
        case ((acc, log, fallen), i) =>
          val idx  = i - fallen
          val tmp0 = acc.withActive(acc.group.others(idx))
          val tmp1 = tmp0.copy(effects = tmp0.effects.withMonsterPart(tickSlotEffects(tmp0.effects.monsterPart)))
          val (tmp, line, _) = tickMonsterEffects(tmp1, tmp1.monsterName, mult)
          val placed = acc.copy(group = acc.group.copy(others = acc.group.others.updated(idx, tmp.activeSlot)))
          val log2   = if (line.isEmpty) log else log :+ line
          if (tmp.monsterCurrentHp > 0L) (placed, log2, fallen)
          else (placed.sideFallen(idx), log2 :+ content.format("battle.group.sideSlain", "monster" -> tmp.monsterName),
                fallen + 1)
      }
      val outcome = if (b.monsterCurrentHp <= 0L) Outcome.Victory else Outcome.Continue
      res.copy(battle = b, outcome = outcome, sideLog = res.sideLog ++ lines)
    }

  /** Временные эффекты на мобе вне пары тикают сами: общий `tickBuffs` крутит
    * только активную пару. */
  private def tickSlotEffects(e: BattleEffects): BattleEffects = e.copy(
    mobAirBoostTurns         = (e.mobAirBoostTurns - 1).max(0),
    monsterSkillBlockedTurns = (e.monsterSkillBlockedTurns - 1).max(0),
    monsterWeakenedTurns     = (e.monsterWeakenedTurns - 1).max(0),
    monsterRootedTurns       = (e.monsterRootedTurns - 1).max(0),
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
    if (res.outcome != Outcome.Continue || !res.endsRound || res.battle == before ||
        res.battle.boss.exists(!_.fightsInGroup)) ZIO.succeed(res)
    else {
      val b0 = res.battle.copy(group = res.battle.group.copy(round = res.battle.group.round + 1))
      for {
        // подкрепление: сородич первого моба, редкость как обычно (сюжетный бой — нет)
        comes <- chanceRoll(b0.group.aliveCount < GroupState.MaxMonsters && b0.story.isEmpty && !b0.noKin,
                   GroupState.ReinforcementChancePct)
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
          if (b1a.group.round != 1 || b1a.story.isDefined || b1a.noKin) ZIO.succeed((b1a, Vector.empty[String]))
          else ZIO.foldLeft(b1a.monstersInOrder.filter(m => BattleState.summons(Rarity.withName(m.rarity))))((b1a, Vector.empty[String])) {
            case ((b, log), caller) => summonKin(b, caller, res.hero.dungeonLevel).map { case (b2, line) => (b2, log :+ line) }
          }
        (b1b, log1b) = summoned
        // очередь: освободилось место — из-за спин выходят следующие
        (b1q, entered) = b1b.admitQueued
        // дальние подтягиваются к герою на одно свободное место — чтобы в
        // следующем раунде достать его сбоку; в сводку это не идёт, перестановка
        // и так видна по строю
        (b1, _) = if (b1q.arena.isDefined) (b1q, Nil) else b1q.closeIn
        log1 = log1a ++ log1b ++ entered.map(m => content.format("battle.group.fromQueue", "monster" -> m.name)).toVector
        // перемешивание: каждый четвёртый раунд, если есть кого мешать. Жребий
        // тянут только подвижные — сооружения стоят где стояли. Группу берём у
        // УЖЕ перемешанного боя — иначе новый активный встанет поверх старого
        // строя, один моб пропадёт, а другой задвоится. Строй не изменился —
        // молчим: сообщать о перемешивании, которого не было, незачем.
        shuffled <-
          if (!b1.isGroup || b1.arena.isDefined || b1.group.round % GroupState.ShufflePeriod != 0) ZIO.succeed((b1, Vector.empty[String]))
          else Random.shuffle(b1.movableInOrder.indices.toList).map { order =>
            val mixed = b1.reshuffle(order)
            val b     = mixed.copy(group = mixed.group.copy(pendingMove = None))
            val line  =
              if (b.placesInOrder == b1.placesInOrder) Vector.empty[String]
              else if (b.group.paired) Vector(content.format("battle.group.shuffle", "monster" -> b.monsterName))
              else Vector(content.text("battle.group.shuffleNoPair"))
            (b, line)
          }
        (b2, log2) = shuffled
        // Таран: в пару встаёт тот, кого таранили
        // Таран: герой шагает на место, куда таранил; мобы остаются где стояли
        rammed = b2.group.pendingMove match {
          case Some(pos) if b2.group.hasMonster(pos) && !b2.effects.heroRooted =>
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
  /** Значки ран союзника для строки строя: пусто, если на нём ничего нет. */
  private def allyDots(a: BattleAlly): String = {
    val marks = List(a.poison.map(_ => Poison.Icon), a.bleed.map(_ => Bleed.Icon), a.burn.map(_ => Burn.Icon),
                     Option.when(a.cold)(Element.Cold.emoji)).flatten
    if (marks.isEmpty) "" else " " + marks.mkString
  }

  private def groupLines(battle: SoloPveBattle): Vector[String] = {
    val queued = if (battle.group.queue.isEmpty) Vector.empty
                 else Vector(content.format("battle.group.queueLine", "n" -> battle.group.queue.size.toString))
    formationLines(battle) ++ queued
  }

  /** Имя моба в строю — с булавкой, пока он пригвождён. */
  private def mobLabel(m: MonsterSlot): String =
    if (m.effects.monsterRooted) m.name + content.text("battle.group.pinnedMark") else m.name

  private def formationLines(battle: SoloPveBattle): Vector[String] =
    if (battle.group.allies.isEmpty && battle.group.paired)
      battle.placesInOrder.map {
        case (pos, Some(m)) =>
          val key = if (pos == battle.group.heroPos) "battle.group.lineHero" else "battle.group.lineFree"
          content.format(key, "n" -> pos.toString, "monster" -> mobLabel(m), "hp" -> m.hpPct.toString, "armor" -> m.armorPct.toString)
        case (pos, None) =>
          content.format("battle.group.lineEmpty", "n" -> pos.toString)
      }.toVector
    else
      (1 to battle.group.rows).toVector.map { pos =>
        val left =
          if (pos == battle.group.heroPos) content.text("battle.group.sideHero")
          else battle.group.allyAt(pos).map(a => content.format("battle.group.sideAlly",
                 "name" -> a.name, "hp" -> a.hpPct.toString, "armor" -> a.armorPct.toString,
                 "dots" -> allyDots(a)))
            .getOrElse(content.text("battle.group.sideNone"))
        val right = battle.monsterAt(pos).map(m => content.format("battle.group.sideMonster",
            "monster" -> mobLabel(m), "hp" -> m.hpPct.toString, "armor" -> m.armorPct.toString))
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
      // Башни в счёт не идут: с них нечего взять и не за что дать опыт.
      fallen0 = battle.group.slain :+ battle.slainActive
      fallen  = {
        val spoils = fallen0.filterNot(m => Race.withNameOption(m.race).exists(Race.immovable))
        // Бой из одних башен всё же должен чем-то кончиться — тогда берём как есть.
        if (spoils.nonEmpty) spoils else fallen0
      }
      // Сюжетный бой: ни опыта, ни добычи по таблицам — только то, что положил
      // сюжет. Налёт на деревню мурлоков — исключение: награда как за обычный
      // бой, только этаж для неё — уровень героя (мурлоки его уровня). Задание
      // с доски называет свой уровень само (см. SoloPveBattle.lootLevel):
      // разбойники третьего уровня и падают как третьего.
      raid       = battle.story.contains(MurlocQuest.RaidStory)
      storyFight = battle.story.isDefined && !raid
      killLevel  = battle.lootLevel.getOrElse(if (raid) hero.lvl else hero.dungeonLevel.toLong)
      // Минибосс боя — тот, кто пал минибоссом, когда бы он ни пал: Некроманта
      // со свитой бьют первым, Крысиный король падает раньше своих крыс. Смотреть
      // только на последнего добитого нельзя — тогда бой считался бы рядовым.
      bossIdx    = fallen.indexWhere(_.boss.flatMap(MiniBoss.byName).isDefined)
      bossAt     = fallen.lift(bossIdx)
      fallenBoss = bossAt.flatMap(_.boss).flatMap(MiniBoss.byName)
      // У минибосса своя награда за уровень босса, а не по этажу и редкости.
      // Группа — опыт одной суммой за всех.
      baseExp = if (storyFight) 0L
                else fallenBoss.zip(bossAt)
                  .map { case (b, rec) => b.expReward(rec.lvl) }
                  .getOrElse(fallen.map(m => (killLevel * Rarity.withName(m.rarity).factor).toLong.max(1L)).sum)
                  .max(1L)
      // Опыт остановлен зельем Фета — за бой ничего (expFrom).
      expGained = hero.expFrom(
                    if (storyFight) 0L else if (blessed) (baseExp * (100L + BattleState.BlessingBonusPct) / 100L).max(1L) else baseExp,
                    now)
      leveled = hero.gainExp(expGained)
      // лут катаем чистым ядром; начисление (инвентарь/серебро) — в LootState
      seed <- Random.nextLong
      // У минибосса дроп свой и всегда есть; обычная таблица лута не катается.
      // Группа — своя добыча с каждого павшего, по порядку гибели.
      (perMonster, rngAfter) = fallenBoss.zip(bossAt) match {
        case _ if storyFight => (List(battle.monsterName -> List.empty[LootGenerator.LootDrop]), Rng(seed))
        case Some((e, rec)) =>
          // Этаж встречи нужен клыку волка: трофей считается по нему.
          val (d, r) = LootGenerator.rollMiniBoss(e, rec.lvl, hero.lvl, Rng(seed),
            floorLvl = hero.dungeonLevel.toLong)
          // Прочие павшие — те, кого босс позвал, и его свита — роняют своё.
          // Съеденных королём в павших нет — их не убивали (см. GroupState.devour).
          // Порядок тот же, что у `fallen`: босс стоит среди павших на своём месте.
          fallen.zipWithIndex.foldLeft((List.empty[(String, List[LootGenerator.LootDrop])], r)) {
            case ((acc, rr), (m, i)) if i == bossIdx => (acc :+ (m.name -> d), rr)
            case ((acc, rr), (m, _)) =>
              val (dd, r3) = LootGenerator.roll(
                Rarity.withName(m.rarity), Race.withName(m.race), killLevel, rr,
                gearChanceBonusPct = hero.gems.gearDropBonusPct,
                rarityBumpPct = if (blessed) BattleState.BlessingBonusPct else 0L)
              (acc :+ (m.name -> dd), r3)
          }
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
            hero.passives.taxidermistChancePct,
            hero.passives.jewelerChancePct,
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
      lootReturn = if (fallenBoss.exists(_.race == Race.Elemental))
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
      flaskRefill <- vampiricRefill(user, fallen.size)
      // Счёт убитых за всю жизнь: пятидесятый оставляет письмо Марисе (сотый
      // зовёт старейшину мурлоков — см. выше).
      _ <- heroDao.updateKills(user.userId, kills)
      // Кого именно положили — в журнал: по нему раса потом придёт за расплатой.
      _ <- KillLogData.add(heroDao, user.userId, fallen.map(_.race))
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
      monsterName       = bossAt.filter(_ => fallenBoss.isDefined).map(_.name)
                            .getOrElse(fallen.map(_.name).mkString(", ")),
      expGained         = expGained,
      newLvl            = Option.when(leveled.lvl > hero.lvl)(leveled.lvl),
      unlocksDarkness   = unlocksDarkness,
      cubeDropped       = cubeDropped,
      elementalDefeated = fallenBoss.exists(_ != MiniBoss.WhiteWolf),
      wolfDefeated      = fallenBoss.contains(MiniBoss.WhiteWolf),
      slainCount        = fallen.size,
      letterFound       = letterFound,
      flaskRefill       = flaskRefill
    )

  /** Вампирская фляга: за каждого павшего — бросок на +1 заряд (FlaskRates.VampiricRefillPct),
    * но не выше полной. Бросок не тратится, если фляги нет или она полна. Пьёт
    * фляга из старого слота, а нет её — первая неполная вампирская на поясе,
    * хоть бы и пустая.
    *
    * Героя читаем заново: до нас пояс уже добрал из сумки опустевшие ячейки
    * ([[refillBelt]]), и запись по старому снимку стёрла бы этот добор.
    * Возвращает (сколько долила, зарядов стало, максимум), если долила. */
  private def vampiricRefill(user: User, slain: Int): Task[Option[(Int, Int, Int)]] =
    getHero(user).flatMap { hero =>
      val equip = hero.equipment
      val inSlot = equip.flask.details match {
        case f @ ItemDetails.Flask(FlaskEffect.Vampiric(_, _), charges, max) if charges < max =>
          Some((charges, max, (n: Int) => equip.copy(flask = equip.flask.copy(details = f.withCharges(n)))))
        case _ => None
      }
      val onBelt = equip.belt.details match {
        case b: ItemDetails.Belt =>
          b.slots.zipWithIndex.collectFirst {
            case (f: BeltSlot.Flask, i) if f.kind.effect.refillSource == RefillSource.Blood && f.charges < f.maxCharges =>
              (f.charges, f.maxCharges,
                (n: Int) => equip.copy(belt = equip.belt.copy(details = b.updated(i, f.copy(charges = n)))))
          }
        case _ => None
      }
      inSlot.orElse(onBelt) match {
        case None => ZIO.succeed(None)
        case Some((charges, max, withCharges)) =>
          ZIO.foldLeft((1 to slain).toList)(charges) { (cur, _) =>
            chanceRoll(cur < max, FlaskRates.VampiricRefillPct).map(hit => if (hit) cur + 1 else cur)
          }.flatMap { after =>
            if (after == charges) ZIO.succeed(None)
            else heroDao.updateEquipment(user.userId, withCharges(after))
                   .as(Some((after - charges, after, max)))
          }
      }
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

  // ── Божественное оружие ─────────────────────────────────────────────────────

  /** Удар божественного оружия: бьёт всех врагов на поле разом — сперва броня,
    * остатком по HP, — стихия ложится на каждого без броска, и сверху идёт грань
    * своего вида: вытянутая жизнь, верный глаз, яд или ветер в спину. Раунд не завершает
    * (герой бьёт дальше), но за раунд удар один, и «Быстрые руки» его не
    * ускоряют. Последний удар рассыпает божественное оружие прямо в руках. */
  private def divineTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    hero.equipment.additionalWeapon.divine match {
      case None                                 => cont(hero, battle, content.text("battle.divine.none"))
      case Some(_) if battle.divineUsedThisRound => cont(hero, battle, content.text("battle.divine.alreadyUsed"))
      case Some(divine)                          =>
        val item  = hero.equipment.additionalWeapon
        val kind  = divine.kind
        val blow  = DivineRates.DamagePerLvl * item.lvl
        // `dealt` — сколько сняли со всех разом (по нему пьёт череп), `blow` —
        // сколько прилетело каждому.
        val (swept, dealt, foes) = divineSweep(battle, blow, kind)

        // Грань вида: череп пьёт половину отнятого, аметист даёт верный глаз,
        // бриллиант — ветер в спину за каждого задетого. Яд змеиного клинка
        // уже лёг на врагов в самом ударе.
        val maxHp   = hero.effectiveMaxHp(nowMs)
        val drained = if (kind == DivineKind.DarkLordSword) (dealt * DivineRates.DrainPct / 100L).max(0L) else 0L
        val healed  = ((hero.fightStats.hp + drained).min(maxHp) - hero.fightStats.hp).max(0L)
        val heroFed = if (healed > 0) hero.copy(fightStats = hero.fightStats.copy(hp = hero.fightStats.hp + healed)) else hero

        val eyed =
          if (kind == DivineKind.AmethystGoddess)
            swept.withEffects(swept.effects.copy(heroTrueStrikeTurns = DivineRates.BuffRounds))
          else swept
        val hasted =
          if (kind == DivineKind.WindLordStaff && foes > 0)
            eyed.copy(heroBattleState = eyed.heroBattleState.add(
              Buff(0L, 0L, 0L, dodgePct = DivineRates.HastePctPerFoe * foes, defencePct = 0L,
                   turnsLeft = Some(DivineRates.BuffRounds))))
          else eyed

        // Заряд потрачен; последний рассыпает божественное оружие — слот пустеет.
        val spent      = divine.spent
        val crumbles   = spent.charges <= 0
        val slot       = if (crumbles) Item.NoItem else item.copy(details = spent)
        val heroAfter0 = heroFed.copy(equipment = heroFed.equipment.copy(additionalWeapon = slot))

        val line = content.format(s"battle.divine.${kind.entryName}",
          "damage" -> blow.toString, "heal" -> healed.toString, "foes" -> foes.toString)
        val log  = Vector(line) ++
          (if (crumbles) Vector(content.format("battle.divine.crumbled", "name" -> kind.itemName)) else Vector.empty)

        val outcome = if (hasted.monsterCurrentHp <= 0L) Outcome.Victory else Outcome.Continue
        // Божественное оружие раунд не завершает — энергию за раунд герой возьмёт своим
        // ударом; но если поле ею и закончилось, раунд кончился вместе с боем.
        val heroAfter = if (outcome == Outcome.Victory && hasted.promoteNext.isEmpty) endOfRoundHero(heroAfter0, nowMs) else heroAfter0
        ZIO.succeed(TurnResult(heroAfter, hasted.copy(divineUsedThisRound = true), log, outcome, endsRound = false))
    }

  /** Бросок грибной смеси прямо из сумки: склянка лопается под ногами врагов,
    * достаётся всем в поле, и споры травят выживших. Расходник раунда — как
    * глоток фляги, только вместо себя выручает по площади; смесь можно и
    * положить в ячейку пояса, тогда её достают оттуда. */
  private def mixTurn(hero: Hero, battle: SoloPveBattle, nowMs: Long): Task[TurnResult] =
    if (battle.consumableUsedThisRound) cont(hero, battle, content.text("battle.consumableAlreadyUsed"))
    else
      inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString)).flatMap { inv =>
        inv.items.data.find((i: Item) => i.brew.contains(BrewKind.MushroomMix)) match {
          case None => cont(hero, battle, content.text("battle.mixNone"))
          case Some(mix) =>
            quickHandsRoll(hero).flatMap { case (consumed, qhLog) =>
              val blow             = BrewRates.MushroomDamagePerLvl * hero.lvl
              val (swept, _, foes) = sweep(battle, blow, None, poisons = true)
              val outcome = if (swept.monsterCurrentHp <= 0L) Outcome.Victory else Outcome.Continue
              val after   = if (outcome == Outcome.Victory && swept.promoteNext.isEmpty) endOfRoundHero(hero, nowMs) else hero
              inventoryRepo.removeItem(mix.id, hero.id).mapError(e => new Throwable(e.toString)) *>
                ZIO.succeed(TurnResult(after, swept.copy(consumableUsedThisRound = consumed),
                  Vector(content.format("battle.mixThrown", "damage" -> blow.toString,
                    "foes" -> foes.toString)) ++ qhLog,
                  outcome, endsRound = false))
            }
        }
      }

  /** Проход божественного оружия по всему полю: каждому врагу — удар, который
    * сперва сносит броню и лишь остатком идёт в HP (защита его не режет,
    * сопротивление минибосса в силе), и прок стихии со стопроцентным шансом;
    * змеиный клинок вдобавок травит. Павшие уходят в добычу. Возвращает бой,
    * суммарный нанесённый урон и число задетых врагов. */
  private def divineSweep(battle: SoloPveBattle, damage: Long, kind: DivineKind): (SoloPveBattle, Long, Int) =
    sweep(battle, damage, kind.element, poisons = kind == DivineKind.SnakeGodBlade)

  /** Сам проход: врагов бьёт одинаково, кто бы ни замахнулся — клинок или
    * цветок. Отличаются только стихия удара и то, травит ли он. */
  private def sweep(
    battle:  SoloPveBattle,
    damage:  Long,
    element: Option[Element],
    poisons: Boolean
  ): (SoloPveBattle, Long, Int) = {

    def strike(b: SoloPveBattle): (SoloPveBattle, Long) = {
      val resist = element.map(e => b.boss.map(_.damageTakenMult(e)).getOrElse(1.0))
        .getOrElse(b.boss.map(_.plainDamageTakenMult).getOrElse(1.0))
      val full     = (damage * resist).toLong.max(1L)
      // Сперва броня, остаток — в HP: удар тяжёлый, но не сквозь доспех.
      val armorDmg = full.min(b.monsterCurrentArmor)
      val hpDmg    = (full - armorDmg).min(b.monsterCurrentHp)
      val dealt    = armorDmg + hpDmg
      val hit      = b.copy(monsterCurrentHp = (b.monsterCurrentHp - hpDmg).max(0L),
                            monsterCurrentArmor = (b.monsterCurrentArmor - armorDmg).max(0L))
      // Стихия и яд ложатся молча: строки проков только засорили бы экран.
      val alive   = hit.monsterCurrentHp > 0L
      val procced = if (alive) element.map(e => applyProcs(hit, Set(e))._1).getOrElse(hit) else hit
      val venomed =
        if (poisons && alive)
          procced.withEffects(procced.effects.copy(
            monsterPoison = Some(procced.effects.monsterPoison.map(_.stacked).getOrElse(Poison.onHit))))
        else procced
      (venomed, dealt)
    }

    // Мобы вне пары — с конца строя: павший уходит из `others`, и удаление не
    // сдвигает ещё не битых.
    val (sided, sideDealt, sideFoes) =
      battle.group.others.indices.reverse.foldLeft((battle, 0L, 0)) { case ((b, total, foes), idx) =>
        val slot = b.group.others(idx)
        if (!slot.alive) (b, total, foes)
        else {
          val (hit, dealt) = strike(b.withActive(slot))
          val back    = b.copy(group = b.group.copy(others = b.group.others.updated(idx, hit.activeSlot)))
          val settled = if (hit.monsterCurrentHp <= 0L) back.sideFallen(idx) else back
          (settled, total + dealt, foes + 1)
        }
      }
    val (swept, activeDealt) = strike(sided)
    (swept, sideDealt + activeDealt, sideFoes + 1)
  }

  // ── Экран боя ───────────────────────────────────────────────────────────────

  /** Экран боя; в группе перед ним — строй, чтобы с порога было видно, кто
    * стоит против героя (после хода строй идёт в сводке раунда). */
  private def showScreen(user: User, renderer: Renderer): Task[Unit] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero   <- getHero(user)
      battle <- getBattle(user)
      mixes  <- mixCount(hero)
      prefs  <- BattleState.prefs(heroDao, user.userId)
      // На арене строй виден всегда: от расстояния зависит, кто кого достаёт.
      _ <- ZIO.when(battle.group.hasFormation || battle.arena.isDefined)(
             renderer.show(user, Screen(groupLines(battle).mkString("\n"), Nil)))
      _ <- renderer.show(
        user,
        buildBattleScreen(hero, battle, hero.effectiveMaxHp(now), now, mixes, prefs.miasma)
      )
    } yield ()

  private def buildBattleScreen(
      hero: Hero,
      battle0: SoloPveBattle,
      maxHp: Long,
      nowMs: Long,
      mixes: Int,
      miasmaOn: Boolean = true
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
      "monsterRace"     -> battle.toMonster.raceName,
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
    // Блок и Контрудар краснеют и тогда, когда бонусное действие раунда уже
    // потрачено (пояс или второе из них).
    val skillButtons = battle.skillSlots.collect {
      case slot if slot.cooldown <= 0 =>
        val enoughEnergy = hero.fightStats.energy >= slot.skill.energyCost(hero)
        val bonusSpent   = BattleState.bonusAction(slot.skill.effect) && battle.consumableUsedThisRound
        pangea.engine.Choice(
          id = s"Skill_${slot.itemId}",
          label = slot.skill.label,
          color =
            if (enoughEnergy && !bonusSpent) pangea.engine.ChoiceColor.Primary
            else pangea.engine.ChoiceColor.Negative,
          // Умения щита стоят в ряду действий, справа от «Атаки»; прочие — над ним.
          row = Some(if (Skill.handSkills.contains(slot.skill)) 1 else 0)
        )
    }
    val (handButtons, topButtons) = skillButtons.partition(_.row.contains(1))
    // Ряды кнопок: row 0 — активные навыки; row 1 — Атака; row 2 — Пояс;
    // row 3 — Сбежать. Отдельной кнопки фляги больше нет: фляги живут в поясе,
    // и он присылает их списком. Места под заглушки (доп. слот, стиль атаки,
    // сменить цель) пока скрыты.
    val beltButton = Option.when(hero.equipment.belt.itemType == ItemType.Belt) {
      val slots = BattleState.beltSlots(hero)
      pangea.engine.Choice(
        "Belt",
        s"👝 Пояс (${slots.size})",
        color =
          if (slots.isEmpty) pangea.engine.ChoiceColor.Negative
          else pangea.engine.ChoiceColor.Primary,
        row = Some(2)
      )
    }
    // В доп. слоте божественное оружие (а не обычная вещь) — своя кнопка над
    // «Сбежать»: удар по всему полю.
    val divineButton = hero.equipment.additionalWeapon.divine.map { _ =>
      pangea.engine.Choice("UseDivine", pangea.engine.Choice.fit(hero.equipment.additionalWeapon.name),
        color = if (battle0.divineUsedThisRound) pangea.engine.ChoiceColor.Negative
                else pangea.engine.ChoiceColor.Positive,
        row = Some(3))
    }
    // Грибная смесь лежит в сумке, а не в слоте, — кнопка появляется, только
    // когда склянка при герое.
    val mixButton = Option.when(mixes > 0)(
      pangea.engine.Choice("ThrowMix",
        pangea.engine.Choice.fit(content.format("battle.mixLabel", "count" -> mixes.toString)),
        color = if (battle0.consumableUsedThisRound) pangea.engine.ChoiceColor.Negative
                else pangea.engine.ChoiceColor.Positive,
        row = Some(2)))
    // Бить некого (напротив пусто, соседей нет) — вместо атаки «Переместиться»
    // (поменяться местами с союзником), а без союзников — «Ждать».
    // Бить некого — можно шагнуть в сторону (если есть куда) или просто
    // выждать: шаг ход тратит, а выжидание копит энергию, и подменять одно
    // другим неправильно.
    val actionButtons =
      if (heroTargets(hero, battle0).nonEmpty) List(pangea.engine.Choice("Attack", "Атака", row = Some(1)))
      else
        List(
          Option.when(BattleState.stepTargets(battle0).nonEmpty)(
            pangea.engine.Choice("Move", content.text("battle.group.moveLabel"), row = Some(1))),
          Some(pangea.engine.Choice("Wait", content.text("battle.group.waitLabel"), row = Some(1)))
        ).flatten
    // «Некромант» (порог 10): тьму можно отпустить или свернуть. Ход на это не
    // тратится, поэтому кнопка стоит не среди действий, а рядом с ними.
    val miasmaButton = Option.when(hero.sets.spreadsMiasma)(
      pangea.engine.Choice("Miasma",
        content.text(if (miasmaOn) "battle.miasmaLabelOn" else "battle.miasmaLabelOff"),
        color = if (miasmaOn) pangea.engine.ChoiceColor.Positive else pangea.engine.ChoiceColor.Secondary,
        row = Some(3)))
    val mainButtons =
      actionButtons ++
        handButtons ++
        beltButton.toList ++
        mixButton.toList ++
        divineButton.toList ++
        miasmaButton.toList ++
        List(
          pangea.engine.Choice(
            "Flee",
            "Сбежать",
            color = pangea.engine.ChoiceColor.Negative,
            row = Some(4)
          )
        )
    Screen(text, topButtons ++ mainButtons)
  }

  /** Пузырьковый нектар: если герой обнулился, а плёнка на нём ещё цела —
    * смерть не состоится, останется один HP, и нектар на этом кончится.
    * Лопнувшую плёнку запишет commit вместе с прочими бафами (см.
    * [[persistExtras]]). */
  private def bubbleRescue(res: TurnResult, nowMs: Long): TurnResult =
    if (res.outcome != Outcome.Death || !res.hero.statBoosts.hasActive(BrewRates.BubbleBoost, nowMs)) res
    else {
      val saved = res.hero.copy(
        fightStats = res.hero.fightStats.copy(hp = BattleState.BubbleHp),
        statBoosts = res.hero.statBoosts.without(BrewRates.BubbleBoost))
      res.copy(hero = saved, outcome = Outcome.Continue,
        log = res.log :+ content.text("battle.bubbleSaved"))
    }

  /** Сколько склянок грибной смеси в сумке героя. Сумка может быть недоступна
    * (стаб в тестах) — тогда кнопки просто не будет. */
  private def mixCount(hero: Hero): Task[Int] =
    inventoryRepo.get(hero.id)
      .map(_.items.data.count((i: Item) => i.brew.contains(BrewKind.MushroomMix)))
      .orElseSucceed(0)

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
    * точность моба ×3 (снижена «Сливающимся» на 10%, см. [[HeroPassives.enemyAccuracyMult]] —
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
  // в знаменателе только точность игрока ×3. Защита сюда БОЛЬШЕ НЕ входит: она
  // отвечает лишь за процентное снижение урона (см. monsterDefenceCut), иначе
  // одна цифра работала бы дважды — и в уклонении, и в защите.
  private def mobDodgeChance(heroAccuracy: Long, battle: SoloPveBattle): Double = {
    // Тот же прок Воздуха добавляет мобу и уклонения; инстинкт волка — свои 5%.
    val airPct      = if (battle.effects.mobAirBoostTurns > 0) Element.Air.ProcBonusPct else 0L
    val instinctPct = if (battle.effects.mobInstinct) MiniBoss.WhiteWolf.InstinctBoostPct else 0L
    val evasion     = battle.monsterStats.evasion * (100L + airPct + instinctPct) / 100L
    val dodge       = BattleState.dodgeChance(0L, evasion, 0L, heroAccuracy)
    // Пока держится грань аметистовой божественного оружия, герой бьёт почти наверняка.
    if (battle.effects.heroTrueStrikeTurns > 0) dodge.min((100L - DivineRates.TrueStrikePct).toDouble) else dodge
  }
}

object BattleState {

  /** Умение союзника, выбранное на ход. Удар несёт свою цель: без цели его
    * в выборе просто нет, и доставать её потом неоткуда. */
  private[battle] sealed abstract class AllyCast(val skill: AllySkill)
  private[battle] object AllyCast {
    final case class Strike(target: Int, crushing: Boolean)
      extends AllyCast(if (crushing) AllySkill.CrushingStrike else AllySkill.QuickStrike)
    case object Flask  extends AllyCast(AllySkill.HealingFlask)
    case object Repair extends AllyCast(AllySkill.EmergencyRepair)
  }

  /** Сколько союзников помещается в ряд клавиатуры: их бывает до десяти, а
    * рядов у ВК всего десять. */
  val AlliesPerRow: Int = 2

  /** Кем встанет павший, если «Некромант» (порог 12) его поднимет. None — в нём
    * нечего поднимать: элементаль, сооружение, уже нежить или Крысиный король
    * (он не тело, а сборище крыс). Белый волк встаёт как по клыку, прочие — как
    * по своему трофею, и с тем именем, под которым пали. */
  def riseForm(slain: pangea.model.battle.SlainMonster,
               boss: Option[MiniBoss]): Option[pangea.model.squad.UndeadForm] =
    boss match {
      case Some(MiniBoss.WhiteWolf) =>
        Some(pangea.service.state.states.events.cave.DarkAltar.wolfOf(slain.lvl))
      case Some(_)                  => None
      case None =>
        Race.withNameOption(slain.race).filterNot(r =>
          r == Race.Elemental || r == Race.Construct || r == Race.Undead
        ).flatMap { race =>
          pangea.model.monster.Rarity.withNameOption(slain.rarity).map { rarity =>
            pangea.service.state.states.events.cave.DarkAltar
              .fromTrophy(race, rarity, slain.lvl.max(1L)).copy(name = slain.name)
          }
        }
    }

  /** Переключатели боя героя: по умолчанию миазмы включены. */
  def prefs(heroDao: pangea.dao.hero.HeroDao, userId: pangea.model.user.UserId): Task[pangea.model.battle.BattlePrefs] =
    heroDao.readBattlePrefs(userId).map(
      _.flatMap(_.as[pangea.model.battle.BattlePrefs].toOption).getOrElse(pangea.model.battle.BattlePrefs.default))

  /** Сооружение ли это — башня со стрелком и всё, что ей подобно. */
  def isTower(battle: SoloPveBattle): Boolean =
    Race.withNameOption(battle.monsterRace).exists(Race.immovable)

  def isTowerSlot(slot: pangea.model.battle.MonsterSlot): Boolean =
    Race.withNameOption(slot.race).exists(Race.immovable)

  /** Кого достанет стрелок с башни: он бьёт с любого места, поэтому целью
    * берёт того, кто к нему ближе. Герой ближе или на равном расстоянии — бьёт
    * героя (тогда None, и удар идёт обычным путём); ближе союзник — достаётся
    * союзнику. Лежачего героя в расчёт не берём: его не добивают. */
  def towerTarget(battle: SoloPveBattle, pos: Int): Option[pangea.model.battle.BattleAlly] = {
    val heroDist = if (battle.group.heroDown) Int.MaxValue else math.abs(pos - battle.group.heroPos)
    battle.group.allies.filter(a => a.alive && math.abs(pos - a.position) < heroDist)
      .minByOption(a => math.abs(pos - a.position))
  }

  /** Куда герой может шагнуть: ровно одно место влево или вправо, и только в
    * пределах строя — за его край, туда, где никого нет и не будет, ходить
    * незачем. Дальше своей стороны ему тоже нельзя: мест у отряда
    * [[Formation.HeroPlaces]], а хвост чужого строя — не его сторона, даже когда
    * враги стоят глубже. Место может быть занято союзником (тогда поменяются)
    * или пустым. */
  def stepTargets(battle: SoloPveBattle): List[Int] =
    // Пригвождённый с места не сходит.
    if (battle.effects.heroRooted) Nil
    else {
      val here  = battle.group.heroPos
      val edge  = battle.group.rows.min(Formation.HeroPlaces)
      List(here - 1, here + 1).filter(p => p >= 1 && p <= edge)
    }

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
  final case class MobStrike(
    newHp:      Long,
    newArmor:   Long,
    damage:     Long,
    extraLines: List[String],
    battle:     SoloPveBattle,
    /** Удар ушёл в призрачную копию: героя он не тронул, и строки об уроне не будет. */
    mirrored:   Boolean = false)

  /** Удар союзника: бой после него, кого бил, сколько снял, добил ли соседа. */
  final case class AllyBlow(battle: SoloPveBattle, target: Option[String], damage: Long, slew: Boolean)

  /** Кто зовёт сородичей в конце первого раунда. */
  def summons(rarity: Rarity): Boolean = rarity == Rarity.Legendary || rarity == Rarity.Mythical

  /** Раунд боя без героя — каждые полминуты. */
  val SquadTickMs: Long = 30L * 1000L
  val SquadTickAction: String = """{"action":"SquadTick"}"""
  /** С каким HP герой приходит в себя, когда отряд добил всех без него. */
  val DownReviveHp: Long = 1L

  /** С каким HP остаётся тот, кого вытащил пузырьковый нектар. */
  val BubbleHp: Long = 1L

  /** Кого достаёт приём того, кто ходит: героя или союзника напротив него.
    * Приёмы мобов и минибоссов бьют того, кто перед ними, а не того, кто им
    * интереснее. */
  sealed trait Victim
  object Victim {
    case object Hero                     extends Victim
    final case class Ally(position: Int) extends Victim
  }

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
      endsRound: Boolean = true,
      // Быстрые отдыхи, которые ход положил в запас (бодрящий сбор с пояса).
      // Запас лежит у Азата, а не в герое, — его допишет commit.
      instantRests: Int = 0
  )

  /** Текущее число зарядов надетой фляги (0, если фляга не надета). */
  def flaskCharges(hero: Hero): Int = hero.equipment.flask.details match {
    case f: ItemDetails.Flask => f.charges
    case _                    => 0
  }

  /** Чем на поясе героя ещё можно воспользоваться — их число стоит на кнопке
    * «Пояс». Пустые ячейки не считаем: брать из них нечего — ни глотка, ни
    * горсти. */
  def beltSlots(hero: Hero): List[BeltSlot] = beltUsable(hero).map(_._1)

  /** Все ячейки пояса с их настоящими номерами — и полные, и опустевшие. */
  def beltCells(hero: Hero): List[(BeltSlot, Int)] = hero.equipment.belt.details match {
    case b: ItemDetails.Belt => b.slots.zipWithIndex
    case _                   => Nil
  }

  /** Ячейки, из которых есть что взять, с настоящими номерами: по ним кнопка и
    * найдёт содержимое, сколько бы пустых ячеек ни стояло выше. */
  def beltUsable(hero: Hero): List[(BeltSlot, Int)] = beltCells(hero).filter(_._1.uses > 0)

  /** Что лежит в ячейке с этим номером. */
  def beltAt(hero: Hero, idx: Int): Option[BeltSlot] = hero.equipment.belt.details match {
    case b: ItemDetails.Belt => b.at(idx)
    case _                   => None
  }

  /** Спрашивает ли отвар, на что его тратят: костоправный — какую травму
    * снять. Приговор спрашивал бы род, но в ячейку он не ложится. */
  def needsPick(kind: BrewKind): Boolean = kind.effect match {
    case BrewEffect.CureTrauma => true
    case _                     => false
  }

  /** Ячейка, которой после боя найдётся замена в сумке. */
  def refillable(slot: BeltSlot): Boolean = slot match {
    case d: BeltSlot.Dust => d.spent
    case b: BeltSlot.Brew => b.spent
    case r: BeltSlot.Rose => r.withered
    case h: BeltSlot.HolyWater => h.spent
    case _                => false
  }

  /** Список без первого такого имени (травмы с одинаковым именем не дубли —
    * их может быть несколько). */
  def withoutFirst(names: List[String], name: String): List[String] = {
    val i = names.indexOf(name)
    if (i < 0) names else names.patch(i, Nil, 1)
  }

  /** Сколько кнопок выбора ставим в ряд, когда отвар спрашивает. */
  val PicksPerRow: Int = 3

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

  /** Номер ячейки пояса, если кнопка его несла. */
  def parseSlot(ua: pangea.service.state.UserAction): Option[Int] =
    ua.payload.flatMap(p =>
      io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("slot")).flatMap(_.toIntOption))

  /** Что игрок выбрал на вопрос отвара: имя травмы или род. */
  def parsePick(ua: pangea.service.state.UserAction): Option[String] =
    ua.payload.flatMap(p =>
      io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("pick"))).filter(_.nonEmpty)

  /** Номер цели умения в строю (с нуля), если кнопка его несла. */
  def parseTarget(ua: pangea.service.state.UserAction): Option[Int] =
    ua.payload.flatMap(p =>
      io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("target")).flatMap(_.toIntOption))

  def parseAction(ua: pangea.service.state.UserAction): Option[String] =
    ua.payload.flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  /** Дальность лечения и брони на союзника: только соседи героя по ряду, как
    * удар мечом по врагам. Массовое или дальнобойное лечение, если появится, —
    * отдельными умениями со своей дальностью. */
  val SupportReach: Int = 1

  /** Лечит ли умение или чинит броню — такое можно отдать союзнику рядом
    * (см. [[SupportReach]]). */
  def supports(effect: Skill.Effect): Boolean = effect match {
    case Skill.Effect.Heal | Skill.Effect.RepairArmor | Skill.Effect.GuardRepair(_, _) => true
    case _                                                                             => false
  }

  /** Поднятый щит (см. Skill.Block): снимает `blockPct`% урона — уже срезанного
    * защитой, — но единица проходит всегда. Щит опущен (0) — урон как есть. */
  def shieldCut(blockPct: Long, damage: Long): Long =
    if (blockPct > 0L) (damage * (100L - blockPct) / 100L).max(1L) else damage

  /** На сколько мест в обе стороны достаёт оружие героя: лук и арбалет — на
    * три, прочее — только соседей (см. [[GripRates.RangedReach]]). Стрелковое
    * всегда двуручное, поэтому лежит в правой руке. */
  def heroReach(hero: Hero): Int =
    if (hero.equipment.weapon.ranged) GripRates.RangedReach else GroupState.Reach

  /** Досягаемость умения: умения оружия бьют как оружие героя, а умения
    * нагрудника — Таран и «Отбросить», удары телом и щитом, — только вблизи. */
  def skillReach(hero: Hero, skill: Skill): Int =
    if (Skill.armorSkills.contains(skill)) GroupState.Reach else heroReach(hero)

  /** Крыса из-под ног героя (набор «Крыса»): обычная на пороге 6, чумная на
    * пороге 12 ([[HeroSets.summonsPlagueRats]]), уровнем в самого героя.
    * `home = 0` помечает её как пришедшую не из отряда — после боя в лабиринте
    * по этому признаку она в отряд и попадает (см. squadAfterBattle). */
  def ratAlly(hero: Hero, pos: Int): BattleAlly = {
    val rarity = if (hero.sets.summonsPlagueRats) Rarity.Rare else Rarity.Uncommon
    val rat    = MonsterGenerator.generateOfRaceAndRarity(hero.lvl.toInt, Race.Animal, rarity)
    val form   = pangea.model.squad.UndeadForm(rat.name, hero.lvl, rat.fightStats)
    BattleAlly(pangea.model.squad.AllyKind.Rat, pos,
      hp = rat.fightStats.hp, armor = rat.fightStats.armor, energy = rat.fightStats.energy,
      stats = rat.fightStats, lvl = hero.lvl, undead = Some(form), home = 0)
  }

  /** На сколько мест прыгает «Отскок» и отлетает цель «Отбрасывающего
    * выстрела»: на одно меньше дальности оружия, но хоть на одно, — так цель
    * остаётся на выстреле. Дальность станет другой — и это число вместе с ней. */
  def displacement(hero: Hero): Int = (heroReach(hero) - 1).max(1)

  /** Стрелковое вблизи бьёт слабее: по цели напротив героя и по соседям — на
    * пятую часть (см. [[GripRates.RangedClosePenaltyPct]]). Цель — тот, кто
    * стоит в полях боя: удар по соседу или дальней цели идёт через подмену. */
  def rangedCloseMod(hero: Hero, battle: SoloPveBattle): Double =
    if (hero.equipment.weapon.ranged && math.abs(battle.group.activePos - battle.group.heroPos) <= GroupState.Reach)
      (100L - GripRates.RangedClosePenaltyPct) / 100.0
    else 1.0

  /** Умение берётся бонусным действием раунда (тем же, что пояс), а не ходом:
    * Блок и Контрудар. */
  def bonusAction(effect: Skill.Effect): Boolean = effect match {
    case Skill.Effect.Block | Skill.Effect.Counter => true
    case _                                         => false
  }

  /** Бьёт ли умение по врагу — таким в группе нужна цель. */
  def dealsDamage(effect: Skill.Effect): Boolean = effect match {
    case Skill.Effect.Damage(_) | Skill.Effect.Sweep(_) | Skill.Effect.Whirl | Skill.Effect.FanBleed(_) | Skill.Effect.Knockback |
         Skill.Effect.BleedDamage(_) | Skill.Effect.WeakSpotStrike | Skill.Effect.BloodHarvest |
         Skill.Effect.Pin(_) | Skill.Effect.Volley(_) | Skill.Effect.Leap | Skill.Effect.Push => true
    case Skill.Effect.Heal | Skill.Effect.RepairArmor | Skill.Effect.GuardRepair(_, _) | Skill.Effect.WarCry(_, _) |
         Skill.Effect.Block | Skill.Effect.Counter => false
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

  /** Чем арена зовёт игрока к экрану и чем добивает просроченный ход. */
  val ArenaPokeAction: String = """{"action":"ArenaPoke"}"""
  val ArenaTurnAction: String = """{"action":"ArenaTurn"}"""

  /** Что арена двигает сама, не спрашивая, чей сейчас ход. */
  val ArenaOwnActions: Set[String] = Set("ArenaPoke", "ArenaTurn")

  /** Шанс уклонения защищающегося юнита от удара атакующего, в процентах, зажат
    * в [5, 95]: 100 * (agi + evasion) / (agi + evasion + defence * 1 +
    * attackerAccuracy * 3) Единая логика «попадания по юниту» для обеих
    * сторон: положительные параметры (числитель) — ловкость и уклонение
    * защищающегося; отрицательные (знаменатель) — защита защищающегося (×1) и
    * точность атакующего (×3). Атакующий попадает, если бросок 1..100 больше
    * уклонения. Вес точности был 1.5: его подняли вдвое, а точность вещей и
    * мобов вдвое срезали — попадания те же, а урон умений от точности меньше.
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
    val denom    = positive + defence * 1.0 + attackerAccuracy * 3.0
    val raw      = if (denom <= 0.0) 0.0 else 100.0 * positive / denom
    raw.max(5.0).min(95.0)
  }
}
