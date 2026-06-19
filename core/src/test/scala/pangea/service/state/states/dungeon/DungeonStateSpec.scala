package pangea.service.state.states.dungeon

import pangea.engine.SceneContent
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.test._
import zio.test.TestRandom
import zio.ZIO

object DungeonStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def isValidFindOutcome(s: StateType): Boolean =
    s == StateType.Battle || s == StateType.FoundItem || s == StateType.Dungeon

  private def makeState(dungeonLevel: Int = 1) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, dungeonLevel = dungeonLevel))
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
      invRepo   = TestInventoryRepository.accepting
    } yield (DungeonState(heroDao, invRepo, content), heroDao, renderer, invRepo)

  override def spec = suite("DungeonState")(

    test("enter → показывает экран с уровнем лабиринта") {
      for {
        quad                      <- makeState()
        (state, _, renderer, _)    = quad
        _                         <- state.enter(testUser, renderer)
        screens                   <- renderer.sentScreens
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.text.contains("лабиринт")) &&
              assertTrue(screens.head.text.contains("1")) &&
              assertTrue(screens.head.choices.map(_.id).contains("FindEvent")) &&
              assertTrue(screens.head.choices.map(_.id).contains("Rest"))
    },

    test("FindEvent → отправляет сообщение про коридоры, переходит в событие") {
      for {
        quad                      <- makeState()
        (state, _, renderer, _)    = quad
        result                    <- state.action(testUser, tap("FindEvent"), renderer)
        screens                   <- renderer.sentScreens
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(isValidFindOutcome(result))
    },

    test("FindEvent Spring (индекс 3) → восстанавливает HP, без засады остаётся в Dungeon") {
      val lowHpHero = TestFixtures.hero(userId).copy(
        fightStats = TestFixtures.hero(userId).fightStats.copy(hp = 10L)
      )
      for {
        heroDao  <- TestHeroDao.withHero(userId, lowHpHero)
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        invRepo   = TestInventoryRepository.accepting
        state     = DungeonState(heroDao, invRepo, content)
        _        <- TestRandom.feedInts(3, 99) // 3 = Spring, 99 → roll 100 = нет засады
        result   <- state.action(testUser, tap("FindEvent"), renderer)
        updated  <- heroDao.getHeroByUserId(userId)
        screens  <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(updated.exists(_.fightStats.hp > 10L)) &&
              assertTrue(screens.exists(_.text.contains("ручеёк")))
    },

    test("FindEvent Spring → засада (roll ≤ 50) → начинается бой") {
      for {
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        invRepo   = TestInventoryRepository.accepting
        state     = DungeonState(heroDao, invRepo, content)
        _        <- TestRandom.feedInts(3, 0) // 3 = Spring, 0 → roll 1 = засада
        _        <- TestRandom.feedLongs(42L)  // seed для монстра
        result   <- state.action(testUser, tap("FindEvent"), renderer)
        screens  <- renderer.sentScreens
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(screens.exists(_.text.contains("ручья")))
    },

    test("FindEvent Spring с экипированной флягой → фляга в слоте пополнена") {
      import pangea.model.item.{FlaskEffect, Item, ItemType, Rarity}
      val flask = Item(1L, "Фляга", 1L, Rarity.Gray, ItemType.Flask,
                   attack=0, accuracy=0, concentration=0, armor=0, defence=0, evasion=0,
                   flaskEffect = Some(FlaskEffect.HealPercent(25)),
                   charges     = Some(0),
                   maxCharges  = Some(8))
      val heroWithEmptyFlask = TestFixtures.hero(userId).copy(
        fightStats = TestFixtures.hero(userId).fightStats.copy(hp = 10L),
        equipment  = TestFixtures.emptyEquipment.copy(flask = flask)
      )
      for {
        heroDao  <- TestHeroDao.withHero(userId, heroWithEmptyFlask)
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        invRepo   = TestInventoryRepository.accepting
        state     = DungeonState(heroDao, invRepo, content)
        _        <- TestRandom.feedInts(3, 99) // без засады
        result   <- state.action(testUser, tap("FindEvent"), renderer)
        updated  <- heroDao.getHeroByUserId(userId)
        screens  <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(updated.exists(_.equipment.flask.charges.contains(8))) &&
              assertTrue(screens.exists(_.text.contains("пополнена")))
    },

    test("FindEvent Spring → фляги в инвентаре пополнены") {
      import pangea.model.item.{FlaskEffect, Item, ItemType, Rarity}
      val flaskInInv = Item(2L, "Запасная фляга", 1L, Rarity.Gray, ItemType.Flask,
                        attack=0, accuracy=0, concentration=0, armor=0, defence=0, evasion=0,
                        flaskEffect = Some(FlaskEffect.HealPercent(25)),
                        charges     = Some(0),
                        maxCharges  = Some(4))
      for {
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        invRepo   = TestInventoryRepository.withItems(List(flaskInInv))
        state     = DungeonState(heroDao, invRepo, content)
        _        <- TestRandom.feedInts(3, 99) // без засады
        _        <- state.action(testUser, tap("FindEvent"), renderer)
      } yield assertTrue(invRepo.snapshot.find(_.id == 2L).exists(_.charges.contains(4)))
    },

    test("GoDarker → увеличивает уровень лабиринта на 1") {
      for {
        quad                          <- makeState(dungeonLevel = 5)
        (state, heroDao, renderer, _)  = quad
        result                        <- state.action(testUser, tap("GoDarker"), renderer)
        screens                       <- renderer.sentScreens
        updatedHero                   <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.text.contains("6")) &&
              assertTrue(updatedHero.exists(_.dungeonLevel == 6))
    },

    test("GoLighter → уменьшает уровень лабиринта на 1") {
      for {
        quad                          <- makeState(dungeonLevel = 5)
        (state, heroDao, renderer, _)  = quad
        result                        <- state.action(testUser, tap("GoLighter"), renderer)
        screens                       <- renderer.sentScreens
        updatedHero                   <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.text.contains("4")) &&
              assertTrue(updatedHero.exists(_.dungeonLevel == 4))
    },

    test("GoLighter на уровне 1 → остаётся на уровне 1") {
      for {
        quad                              <- makeState(dungeonLevel = 1)
        (state, heroDao, renderer, _)      = quad
        _                                 <- state.action(testUser, tap("GoLighter"), renderer)
        updatedHero                       <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(updatedHero.exists(_.dungeonLevel == 1))
    },

    test("GoDarker на уровне 150 → остаётся на уровне 150") {
      for {
        quad                              <- makeState(dungeonLevel = 150)
        (state, heroDao, renderer, _)      = quad
        _                                 <- state.action(testUser, tap("GoDarker"), renderer)
        updatedHero                       <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(updatedHero.exists(_.dungeonLevel == 150))
    },

    test("Rest → переходит в Rest без сообщений") {
      for {
        quad                      <- makeState()
        (state, _, renderer, _)    = quad
        result                    <- state.action(testUser, tap("Rest"), renderer)
        screens                   <- renderer.sentScreens
      } yield assertTrue(result == StateType.Rest) &&
              assertTrue(screens.isEmpty)
    },

    test("OpenInventory → переходит в Inventory без сообщений") {
      for {
        quad                      <- makeState()
        (state, _, renderer, _)    = quad
        result                    <- state.action(testUser, tap("OpenInventory"), renderer)
        screens                   <- renderer.sentScreens
      } yield assertTrue(result == StateType.Inventory) &&
              assertTrue(screens.isEmpty)
    },

    test("CharacterInfo → переходит в HeroStats без сообщений") {
      for {
        quad                      <- makeState()
        (state, _, renderer, _)    = quad
        result                    <- state.action(testUser, tap("CharacterInfo"), renderer)
        screens                   <- renderer.sentScreens
      } yield assertTrue(result == StateType.HeroStats) &&
              assertTrue(screens.isEmpty)
    },

    test("неизвестный ввод → остаётся в Dungeon без сообщений") {
      for {
        quad                      <- makeState()
        (state, _, renderer, _)    = quad
        result                    <- state.action(testUser, UserAction("что угодно", None), renderer)
        screens                   <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.isEmpty)
    }
  )
}
