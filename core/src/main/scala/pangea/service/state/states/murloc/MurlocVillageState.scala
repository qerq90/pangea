package pangea.service.state.states.murloc

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.{BattleEffects, MonsterSlot, SoloPveBattle}
import pangea.model.hero.{Achievement, Hero}
import pangea.model.item.Item
import pangea.model.monster.Monster
import pangea.model.quest.NpcQuest
import pangea.model.skill.MonsterEnergy
import pangea.model.squad.AllyKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.murloc.MurlocVillageState._
import pangea.service.state.{MurlocQuest, NpcQuestLog, QuestSupport, State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Деревня мурлоков — куда ведёт карта старейшины (только из города, см.
  * InventoryState). Сначала развилка: напасть или помочь.
  *
  * Налёт — сюжетный бой (`story = murlocRaid`) с десятью мурлоками в строю и
  * тринадцатью в очереди за ним, уровня героя; в отличие от коллектора,
  * награждается как обычный бой. Отряд идёт с героем, но Плюх в налёте
  * участвовать отказывается и уходит по свитку до боя. Победа возвращает сюда
  * (`Progress(AfterRaid)` через добычу): карта уходит, «Гроза Мурлоков».
  *
  * Помощь — сдать старейшине снаряжение из сумки пачками по цвету; двадцатая
  * вещь — «Клинок старого мурлока», карта уходит, «Любимец Мурлоков». Не влез в
  * сумку — ждёт в деревне (третий шаг задания), кнопка «Забрать клинок». */
case class MurlocVillageState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "Attack"      -> Target.Run { (u, _, r) => confirmRaid(u, r) },
      "Help"        -> Target.Run { (u, _, r) => village(u, r) },
      "Raid"        -> Target.Run { (u, _, r) => startRaid(u, r) },
      "HandIn"      -> Target.Run { (u, _, r) => handIn(u, r) },
      "TakeBlade"   -> Target.Run { (u, _, r) => takeBlade(u, r) },
      "Village"     -> Target.Run { (u, _, r) => village(u, r) },
      "ToInventory" -> Target.Goto(StateType.Inventory),
      "ToCity"      -> Target.Goto(StateType.GlobalMap)
    ) ++ MurlocQuest.GearGroup.values.map(g => g.action -> Target.Run { (u, _, r) => give(u, r, g) }),
    fallback = Target.Run { (u, _, r) => enter(u, r).as(StateType.MurlocVillage) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets ++ Set(StateType.Battle, StateType.MurlocVillage)

  /** Вход: после налёта (добыча положила `Progress(AfterRaid)`) — развязка;
    * иначе — развилка, пока карта на руках. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero     <- getHero(user)
      progress <- heroDao.readSceneData(user.userId).map(_.flatMap(_.as[Progress].toOption))
      quests   <- NpcQuestLog.load(heroDao, user.userId)
      _        <- if (progress.exists(_.step == Step.AfterRaid)) afterRaid(user, hero, renderer)
                  else if (MurlocQuest.mapOnHands(quests)) renderer.show(user, routeScreen)
                  else renderer.show(user, Screen(content.text("murlocVillage.questDone"), List(content.choice("ToCity", "murlocVillage.toCity"))))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Развилка ────────────────────────────────────────────────────────────────

  private def routeScreen: Screen =
    Screen(content.text("murlocVillage.map.route"), List(
      content.choice("Attack", "murlocVillage.map.attackLabel").copy(row = Some(0)),
      content.choice("Help", "murlocVillage.map.helpLabel").copy(row = Some(0)),
      content.choice("ToInventory", "murlocVillage.map.backLabel").copy(row = Some(1))))

  // ── Налёт ───────────────────────────────────────────────────────────────────

  private def confirmRaid(user: User, renderer: Renderer): Task[StateType] =
    renderer.show(user, Screen(content.text("murlocVillage.raid.confirm"), List(
      content.choice("Raid", "murlocVillage.raid.goLabel"),
      content.choice("ToCity", "murlocVillage.raid.leaveLabel")))).as(StateType.MurlocVillage)

  /** Бой: строй из десяти, очередь из тринадцати, у каждого своя стартовая
    * энергия. Плюх уходит до боя — по свитку, на сутки, как после смерти. */
  private def startRaid(user: User, renderer: Renderer): Task[StateType] =
    for {
      now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero0  <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      plukh   = hero0.squad.has(AllyKind.Murloc)
      hero    = if (plukh) hero0.copy(squad = hero0.squad.sentAway(AllyKind.Murloc, now)) else hero0
      _      <- ZIO.when(plukh)(heroDao.updateSquad(user.userId, hero.squad))
      seed   <- Random.nextLong
      (front, queue, _) = MurlocQuest.raidMonsters(hero.lvl.toInt, Rng(seed))
      frontEnergies <- ZIO.foreach(front)(startEnergy)
      queueSlots    <- ZIO.foreach(queue)(m => startEnergy(m).map(e => slot(m, e)))
      battle  = queueSlots.foldLeft(SoloPveBattle.fromGroup(front, hero, frontEnergies))(_ admit _)
                  .copy(story = Some(MurlocQuest.RaidStory))
      routing = LootData(Nil, Nil, returnState = Some(StateType.MurlocVillage), eventData = Some(Progress(Step.AfterRaid).asJson))
      _      <- heroDao.writeActiveBattle(user.userId, battle.asJson)
      _      <- heroDao.writeSceneData(user.userId, routing.asJson)
      traitor = if (MurlocQuest.handed(quests) > 0L) "\n\n" + content.text("murlocVillage.raid.startTraitor") else ""
      _      <- renderer.show(user, Screen(content.text("murlocVillage.raid." + MurlocQuest.key(hero, "start")) + traitor, Nil))
      _      <- ZIO.when(plukh)(renderer.show(user, Screen(content.text("murlocVillage.raid.plukhLeaves"), Nil)))
    } yield StateType.Battle

  private def startEnergy(m: Monster): Task[Long] =
    Random.nextLongBetween(MonsterEnergy.StartPctMin, MonsterEnergy.StartPctMax + 1L)
      .map(pct => MonsterEnergy.startEnergy(m.lvl, m.rarity, pct))

  private def slot(m: Monster, energy: Long): MonsterSlot =
    MonsterSlot(m.lvl, m.race.entryName, m.rarity.entryName, m.fightStats, m.fightStats.hp, m.fightStats.armor,
      m.marked, energy, BattleEffects.empty)

  /** Деревня вырезана: карта уходит, «Гроза Мурлоков», в город. */
  private def afterRaid(user: User, hero: Hero, renderer: Renderer): Task[Unit] =
    for {
      _    <- renderer.show(user, Screen(
                content.text("murlocVillage.raid.victory") +
                  (if (MurlocQuest.isKin(hero)) "\n\n" + content.text("murlocVillage.raid.victoryKin") else ""), Nil))
      line <- MurlocQuest.finish(heroDao, inventoryRepo, content, user.userId, hero)
      _    <- renderer.show(user, Screen(line, Nil))
      _    <- QuestSupport.grant(heroDao, content, user, hero, Achievement.MurlocBane, renderer)
      _    <- heroDao.writeSceneData(user.userId, Json.Null)
      _    <- renderer.show(user, Screen(content.text("murlocVillage.raid.afterVictory"), List(content.choice("ToCity", "murlocVillage.toCity"))))
    } yield ()

  // ── Помощь ──────────────────────────────────────────────────────────────────

  /** Деревня: сдать снаряжение или уйти; клинок ждёт — забрать. */
  private def village(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      screen  =
        if (MurlocQuest.rewardWaits(quests))
          Screen(content.text("murlocVillage.village.rewardWaits"), List(
            content.choice("TakeBlade", "murlocVillage.village.takeBladeLabel"),
            content.choice("ToCity", "murlocVillage.village.leaveLabel")))
        else
          Screen(content.text("murlocVillage.village." + MurlocQuest.key(hero, "enter")), List(
            content.choice("HandIn", "murlocVillage.village.handInLabel"),
            content.choice("ToCity", "murlocVillage.village.leaveLabel")))
      _      <- renderer.show(user, screen)
    } yield StateType.MurlocVillage

  /** Экран сдачи: сколько сдано, кнопки по цветам (пустые не показываются) и «всё». */
  private def handIn(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      inv    <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      gear    = MurlocQuest.gear(inv.items.data)
      header  = content.format("murlocVillage.village.handIn",
                  "given" -> MurlocQuest.handed(quests).toString, "goal" -> NpcQuest.MurlocGearGoal.toString)
      text    = if (gear.isEmpty) header + "\n\n" + content.text("murlocVillage.village.handInEmpty") else header
      groups  = MurlocQuest.GearGroup.values.zipWithIndex.flatMap { case (g, i) =>
                  val n = gear.count(g.matches)
                  Option.when(n > 0)(Choice(g.action, content.format(g.labelKey, "n" -> n.toString),
                    color = ChoiceColor.Positive, row = Some(i)))
                }
      back    = content.choice("Village", "murlocVillage.village.backLabel").copy(row = Some(groups.size))
      _      <- renderer.show(user, Screen(text, groups :+ back))
    } yield StateType.MurlocVillage

  /** Сдать пачку: не больше, чем осталось до двадцати, сначала похуже. Двадцатая
    * вещь — клинок. */
  private def give(user: User, renderer: Renderer, group: MurlocQuest.GearGroup): Task[StateType] =
    for {
      hero      <- getHero(user)
      quests    <- NpcQuestLog.load(heroDao, user.userId)
      inv       <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      remaining  = NpcQuest.MurlocGearGoal - MurlocQuest.handed(quests)
      picked     = MurlocQuest.pick(inv.items.data, group, remaining)
      res       <- if (picked.isEmpty) handIn(user, renderer)
                   else for {
                     _     <- inventoryRepo.removeItems(picked.map(_.id).toSet, hero.id).mapError(e => new Throwable(e.toString))
                     total  = MurlocQuest.handed(quests) + picked.size.toLong
                     _     <- NpcQuestLog.modify(heroDao, user.userId)(_.update(NpcQuest.Murloc)(_.copy(counter = total)))
                     next  <- if (total >= NpcQuest.MurlocGearGoal) reward(user, hero, renderer)
                              else renderer.show(user, Screen(content.format(
                                     "murlocVillage.village." + MurlocQuest.key(hero, "given"),
                                     "n" -> (NpcQuest.MurlocGearGoal - total).toString), Nil)) *> handIn(user, renderer)
                   } yield next
    } yield res

  /** Клинок ждал в деревне — забрать, если теперь есть место. */
  private def takeBlade(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      quests <- NpcQuestLog.load(heroDao, user.userId)
      res    <- if (MurlocQuest.rewardWaits(quests)) reward(user, hero, renderer) else village(user, renderer)
    } yield res

  /** Награда: клинок в сумку, карта прочь, «Любимец Мурлоков» — в город. Места
    * нет — клинок остаётся у старейшины (третий шаг), в город без него. */
  private def reward(user: User, hero: Hero, renderer: Renderer): Task[StateType] =
    for {
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      seed  <- Random.nextLong
      added <- if (!inv.hasRoomFor(1L)) ZIO.succeed(Option.empty[Item])
               else itemRepo.persist(hero.id, MurlocQuest.blade(hero, Rng(seed))).flatMap(blade =>
                 inventoryRepo.addItem(hero.id, blade).as(Option(blade)).catchAll(_ => ZIO.succeed(Option.empty[Item])))
      res   <- added match {
        case Some(blade) =>
          for {
            _    <- renderer.show(user, Screen(
                      content.text("murlocVillage.village." + MurlocQuest.key(hero, "reward")) + "\n\n" +
                        content.format("murlocVillage.village.bladeGiven", "name" -> blade.name), Nil))
            line <- MurlocQuest.finish(heroDao, inventoryRepo, content, user.userId, hero)
            _    <- renderer.show(user, Screen(line, Nil))
            _    <- QuestSupport.grant(heroDao, content, user, hero, Achievement.MurlocFavorite, renderer)
          } yield StateType.GlobalMap
        case None =>
          NpcQuestLog.modify(heroDao, user.userId)(_.update(NpcQuest.Murloc)(_.copy(step = 3))) *>
            renderer.show(user, Screen(content.text("murlocVillage.village.noRoom"), Nil)).as(StateType.GlobalMap)
      }
    } yield res

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object MurlocVillageState {

  /** Где мы: развилка с карты или развязка после налёта. */
  object Step {
    val Route: String     = "route"
    val AfterRaid: String = "afterRaid"
  }

  final case class Progress(step: String)

  object Progress {
    implicit val encoder: Encoder[Progress] = (p: Progress) => Json.obj("murlocStep" -> p.step.asJson)
    // Ключ свой, а не общий «step»: в scene_data лежат и чужие структуры, и
    // случайное совпадение поля не должно читаться как наш прогресс.
    implicit val decoder: Decoder[Progress] = (c: HCursor) => c.get[String]("murlocStep").map(Progress(_))
  }
}
