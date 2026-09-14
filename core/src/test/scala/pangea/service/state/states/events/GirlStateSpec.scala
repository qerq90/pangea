package pangea.service.state.states.events

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.ItemType
import pangea.model.monster.{Race, Rarity}
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.events.GirlState.{GirlScene, Step}
import pangea.test.{TestBarrelRepository, TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer, TestScheduler}
import zio.test.{TestClock, TestRandom}
import zio.test._
import zio.{Duration, Task, ZIO}

/** Событие «Девушка»: встреча, бой с тремя, благодарности в городе, комната в
  * таверне и брат с ножом. */
object GirlStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero: Hero = TestFixtures.hero(userId, dungeonLevel = 7).copy(lvl = 10L, silver = 1000L, guildReputation = 50L)

  private def orcScene(step: String): GirlScene = GirlScene(step, Race.Orc.entryName)

  private def make(h: Hero, scene: Option[GirlScene] = None, barrelSilver: Long = 0L) =
    for {
      dao   <- TestHeroDao.withHero(userId, h)
      _     <- ZIO.foreachDiscard(scene)(s => dao.writeSceneData(userId, s.asJson))
      inv    = TestInventoryRepository.accepting
      barrel = new TestBarrelRepository(Nil, barrelSilver)
      sched <- TestScheduler.make
      r     <- TestRenderer.make
      c     <- ZIO.attempt(SceneContent.load())
    } yield (GirlState(dao, inv, TestItemRepository.make, barrel, sched, c), dao, inv, barrel, sched, r)

  private def sceneOf(dao: TestHeroDao): Task[Option[GirlScene]] =
    dao.readSceneData(userId).map(_.flatMap(_.as[GirlScene].toOption))

  private def heroOf(dao: TestHeroDao): Task[Hero] = dao.getHeroByUserId(userId).map(_.get)

  private def texts(r: pangea.test.TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  override def spec = suite("Событие «Девушка»")(

    test("встреча: раса бандитов в тексте и в сцене, «Пройти мимо» — назад в лабиринт") {
      for {
        t <- make(hero)
        (state, dao, _, _, _, r) = t
        _      <- TestRandom.feedInts(Race.mortals.indexOf(Race.Orc))
        _      <- state.enter(testUser, r)
        first  <- r.sentScreens.map(_.last)
        scene  <- sceneOf(dao)
        result <- state.action(testUser, tap("PassBy"), r)
        after  <- dao.readSceneData(userId)
      } yield assertTrue(first.text.contains("трое вооружённых Орк")) &&
              assertTrue(first.choices.map(_.id) == List("Help", "PassBy")) &&
              assertTrue(scene.exists(s => s.step == Step.Meet && s.race == Race.Orc.entryName)) &&
              assertTrue(result == StateType.Dungeon) &&
              assertTrue(after.contains(io.circe.Json.Null))
    },

    test("«Помочь» → бандиты; «Извиниться» — в лабиринт; «Достать оружие» — бой с тремя первых трёх тиров") {
      for {
        t <- make(hero, Some(orcScene(Step.Meet)))
        (state, dao, _, _, _, r) = t
        _       <- state.action(testUser, tap("Help"), r)
        bandits <- r.sentScreens.map(_.last)
        // трое: тиры 0/1/2 → Common/Uncommon/Rare; стартовая энергия
        _       <- TestRandom.feedInts(0, 1, 2) *> TestRandom.feedLongs(50L, 50L, 50L)
        result  <- state.action(testUser, tap("Fight"), r)
        battle  <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        routing <- dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption).get)
        t2 <- make(hero, Some(orcScene(Step.Bandits)))
        (state2, _, _, _, _, r2) = t2
        back    <- state2.action(testUser, tap("Apologize"), r2)
      } yield assertTrue(bandits.choices.map(_.id) == List("Fight", "Apologize")) &&
              assertTrue(result == StateType.Battle) &&
              assertTrue(battle.monstersInOrder.map(_.rarity) ==
                List(Rarity.Common.entryName, Rarity.Uncommon.entryName, Rarity.Rare.entryName)) &&
              assertTrue(battle.monstersInOrder.forall(m => m.race == Race.Orc.entryName && m.lvl == 7L)) &&
              assertTrue(routing.returnState.contains(StateType.Girl)) &&
              assertTrue(routing.eventData.flatMap(_.as[GirlScene].toOption).exists(_.step == Step.AfterFight)) &&
              assertTrue(back == StateType.Dungeon)
    },

    test("после боя (бросок 0): благодарность — серебро уровень×2 ±20%, репутация уровень×2, «Дальше» — в лабиринт") {
      for {
        t <- make(hero, Some(orcScene(Step.AfterFight)))
        (state, dao, _, _, _, r) = t
        _      <- TestRandom.feedInts(0) *> TestRandom.feedLongs(120L)
        _      <- state.enter(testUser, r)
        h      <- heroOf(dao)
        screen <- r.sentScreens.map(_.last)
        result <- state.action(testUser, tap("Onward"), r)
      } yield assertTrue(h.silver == 1000L + 24L) &&        // 10 × 2 × 120%
              assertTrue(h.guildReputation == 50L + 20L) &&
              assertTrue(screen.text.contains("+24 серебра") && screen.choices.map(_.id) == List("Onward")) &&
              assertTrue(result == StateType.Dungeon)
    },

    test("после боя (бросок 1): просьба проводить; отказ — в лабиринт без награды") {
      for {
        t <- make(hero, Some(orcScene(Step.AfterFight)))
        (state, dao, _, _, _, r) = t
        _      <- TestRandom.feedInts(1)
        _      <- state.enter(testUser, r)
        screen <- r.sentScreens.map(_.last)
        result <- state.action(testUser, tap("Decline"), r)
        h      <- heroOf(dao)
      } yield assertTrue(screen.choices.map(_.id) == List("Escort", "Decline")) &&
              assertTrue(result == StateType.Dungeon) &&
              assertTrue(h.guildReputation == 50L)
    },

    test("в городе 48%: благодарность; прощание — уровень×3; расспрос — уровень×3, потом ещё уровень×10 ±20%") {
      for {
        t <- make(hero, Some(orcScene(Step.Escort)))
        (state, dao, _, _, _, r) = t
        _      <- TestRandom.feedInts(0)
        _      <- state.action(testUser, tap("Escort"), r)
        screen <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("Ask"), r)
        asked  <- r.sentScreens.map(_.last)
        mid    <- heroOf(dao)
        _      <- TestRandom.feedLongs(80L)
        result <- state.action(testUser, tap("FarewellAsked"), r)
        h      <- heroOf(dao)
        t2 <- make(hero, Some(orcScene(Step.CityThanks)))
        (state2, dao2, _, _, _, r2) = t2
        res2   <- state2.action(testUser, tap("Farewell"), r2)
        h2     <- heroOf(dao2)
      } yield assertTrue(screen.choices.map(_.id) == List("Ask", "Farewell")) &&
              assertTrue(asked.text.contains("приворотного зелья")) &&
              assertTrue(mid.guildReputation == 50L + 30L) &&
              assertTrue(result == StateType.GlobalMap && h.guildReputation == 50L + 30L + 80L) &&   // 10 × 10 × 80%
              assertTrue(res2 == StateType.GlobalMap && h2.guildReputation == 50L + 30L)
    },

    test("в городе 2%: карта отца — цена уровень×110 ±20%, покупка кладёт карту клада и списывает серебро") {
      for {
        t <- make(hero.copy(silver = 2000L), Some(orcScene(Step.Escort)))
        (state, dao, inv, _, _, r) = t
        _      <- TestRandom.feedInts(48) *> TestRandom.feedLongs(100L)
        _      <- state.action(testUser, tap("Escort"), r)
        _      <- state.action(testUser, tap("OfferBuy"), r)
        offer  <- r.sentScreens.map(_.last)
        result <- state.action(testUser, tap("BuyMap"), r)
        h      <- heroOf(dao)
      } yield assertTrue(offer.text.contains("1100 серебра")) &&
              assertTrue(result == StateType.GlobalMap) &&
              assertTrue(h.silver == 2000L - 1100L) &&
              assertTrue(inv.snapshot.exists(_.itemType == ItemType.TreasureMap))
    },

    test("карта отца: без серебра — остаёмся; отказ от покупки — уровень×3; пожелать удачи — уровень×10 ±20%") {
      for {
        t <- make(hero.copy(silver = 100L), Some(orcScene(Step.MapOffer).copy(price = 1100L)))
        (state, dao, inv, _, _, r) = t
        stay   <- state.action(testUser, tap("BuyMap"), r)
        all    <- texts(r)
        res    <- state.action(testUser, tap("CantAfford"), r)
        h      <- heroOf(dao)
        t2 <- make(hero, Some(orcScene(Step.CityMap)))
        (state2, dao2, _, _, _, r2) = t2
        _      <- TestRandom.feedLongs(120L)
        res2   <- state2.action(testUser, tap("WishLuck"), r2)
        h2     <- heroOf(dao2)
      } yield assertTrue(stay == StateType.Girl && all.contains("Столько серебра у вас нет")) &&
              assertTrue(inv.snapshot.isEmpty) &&
              assertTrue(res == StateType.GlobalMap && h.guildReputation == 50L + 30L) &&
              assertTrue(res2 == StateType.GlobalMap && h2.guildReputation == 50L + 120L)   // 10 × 10 × 120%
    },

    test("в городе 48%: таверна; уйти, не поднимаясь, — уровень×2") {
      for {
        t <- make(hero, Some(orcScene(Step.Escort)))
        (state, dao, _, _, _, r) = t
        _      <- TestRandom.feedInts(99)
        _      <- state.action(testUser, tap("Escort"), r)
        _      <- state.action(testUser, tap("ToTavern"), r)
        tavern <- r.sentScreens.map(_.last)
        result <- state.action(testUser, tap("LeaveTavern"), r)
        h      <- heroOf(dao)
      } yield assertTrue(tavern.text.contains("Золотой якорь") && tavern.choices.map(_.id) == List("ToRoom", "LeaveTavern")) &&
              assertTrue(result == StateType.GlobalMap && h.guildReputation == 50L + 20L)
    },

    test("уединение: кошелёк худеет на 25–50%, травмы пройдут через 90 минут, свет гаснет на 30 — без кнопок") {
      val wounded = hero.copy(traumaUntil = Some(10L * 60L * 60L * 1000L), traumaNames = List("Хромота"))
      for {
        t <- make(wounded, Some(orcScene(Step.Room)))
        (state, dao, _, _, sched, r) = t
        _      <- TestRandom.feedLongs(40L)
        result <- state.action(testUser, tap("Intimacy"), r)
        h      <- heroOf(dao)
        dark   <- r.sentScreens.map(_.last)
        tasks  <- sched.scheduled
        again  <- state.action(testUser, tap("LeaveRoom"), r)
        still  <- r.sentScreens.map(_.last)
        _      <- TestClock.adjust(Duration.fromMillis(GirlState.RoomWaitMs + 1000L))
        out    <- state.action(testUser, tap("LeaveRoom"), r)
        all    <- texts(r)
        cancelled <- sched.cancelled
      } yield assertTrue(result == StateType.Girl) &&
              assertTrue(h.silver == 600L) &&
              assertTrue(h.traumaUntil.contains(GirlState.HealDelayMs)) &&      // TestClock на нуле
              assertTrue(dark.text.contains("погас свет") && dark.choices.isEmpty) &&
              assertTrue(tasks.exists(s => s.kind == TaskKind.GirlRoom && s.fireAt == GirlState.RoomWaitMs && s.expectedState == StateType.Girl)) &&
              assertTrue(again == StateType.Girl && still.text.contains("погас свет")) &&
              assertTrue(out == StateType.GlobalMap) &&
              assertTrue(all.contains("оставив даму спать") && all.contains("на 400 серебра")) &&
              assertTrue(cancelled.contains(userId -> TaskKind.GirlRoom))
    },

    test("отказ дважды: брат требует половину серебра с бочкой; отдать — из кошелька и бочки") {
      for {
        t <- make(hero.copy(silver = 300L), Some(orcScene(Step.Room)), barrelSilver = 700L)
        (state, dao, _, barrel, _, r) = t
        _      <- state.action(testUser, tap("Excuse"), r)
        excuse <- r.sentScreens.map(_.last)
        _      <- state.action(testUser, tap("Refuse"), r)
        _      <- state.action(testUser, tap("Explain"), r)
        demand <- r.sentScreens.map(_.last)
        result <- state.action(testUser, tap("PayBrother"), r)
        h      <- heroOf(dao)
      } yield assertTrue(excuse.choices.map(_.id) == List("Intimacy", "Refuse")) &&
              assertTrue(demand.text.contains("500 серебра") && demand.choices.map(_.id) == List("PayBrother", "FightBrother")) &&
              assertTrue(demand.choices.head.label == "Отдать серебро (500)") &&
              assertTrue(result == StateType.GlobalMap) &&
              assertTrue(h.silver == 0L && barrel.silverSnapshot == 500L)
    },

    test("взять оружие: редкий человек уровня этажа, добыча ведёт в город") {
      for {
        t <- make(hero, Some(orcScene(Step.Demand).copy(price = 500L)))
        (state, dao, _, _, _, r) = t
        _       <- TestRandom.feedLongs(50L)
        result  <- state.action(testUser, tap("FightBrother"), r)
        battle  <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        routing <- dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption).get)
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(battle.monsterRace == Race.Human.entryName && battle.monsterRarity == Rarity.Rare.entryName) &&
              assertTrue(battle.monsterLvl == 7L && !battle.isGroup) &&
              assertTrue(routing.returnState.contains(StateType.GlobalMap))
    },

    test("в пуле событий девушка занимает 2%, забранные у ручья") {
      assertTrue(StateType.events.count(_ == StateType.Girl) == 2) &&
      assertTrue(StateType.events.count(_ == StateType.Spring) == 18) &&
      assertTrue(StateType.events.size == 100)
    }
  )
}
