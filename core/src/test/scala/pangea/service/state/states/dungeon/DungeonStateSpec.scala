package pangea.service.state.states.dungeon

import pangea.engine.SceneContent
import pangea.model.item.{Item, ItemDetails}
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.test._
import zio.test.{TestClock, TestRandom}
import zio.{Duration, ZIO}

object DungeonStateSpec extends ZIOSpecDefault {

  private def flaskCharges(i: Item): Option[Int] = i.details match {
    case f: ItemDetails.Flask => Some(f.charges)
    case _                    => None
  }

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def isValidFindOutcome(s: StateType): Boolean =
    s == StateType.Battle || s == StateType.FoundItem || s == StateType.Dungeon

  private def makeState(dungeonLevel: Int = 1, maxDungeonLevel: Int = 150) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, dungeonLevel = dungeonLevel, maxDungeonLevel = maxDungeonLevel))
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
      invRepo   = TestInventoryRepository.accepting
    } yield (DungeonState(heroDao, invRepo, content), heroDao, renderer, invRepo)

  import pangea.engine.ChoiceColor
  private def colorOf(s: pangea.engine.Screen, id: String): Option[ChoiceColor] =
    s.choices.find(_.id == id).map(_.color)

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

    test("FindEvent Spring (индекс 67) → восстанавливает HP, без засады остаётся в Dungeon") {
      val lowHpHero = TestFixtures.hero(userId).copy(
        fightStats = TestFixtures.hero(userId).fightStats.copy(hp = 10L)
      )
      for {
        heroDao  <- TestHeroDao.withHero(userId, lowHpHero)
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        invRepo   = TestInventoryRepository.accepting
        state     = DungeonState(heroDao, invRepo, content)
        _        <- TestRandom.feedInts(67, 99) // 67 = Spring, 99 → roll 100 = нет засады
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
        _        <- TestRandom.feedInts(67, 0) // 67 = Spring, 0 → roll 1 = засада
        _        <- TestRandom.feedLongs(42L)  // seed для монстра
        result   <- state.action(testUser, tap("FindEvent"), renderer)
        screens  <- renderer.sentScreens
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(screens.exists(_.text.contains("ручья")))
    },

    test("FindEvent Spring с экипированной флягой → фляга в слоте пополнена") {
      import pangea.model.item.{FlaskEffect, Item, ItemType, Rarity}
      val flask = Item(1L, "Фляга", 1L, Rarity.Gray, ItemType.Flask,
                   attack=0, accuracy=0, energy=0, armor=0, defence=0, evasion=0,
                   details = ItemDetails.Flask(FlaskEffect.HealPercent(25), charges = 0, maxCharges = 8))
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
        _        <- TestRandom.feedInts(67, 99) // без засады
        result   <- state.action(testUser, tap("FindEvent"), renderer)
        updated  <- heroDao.getHeroByUserId(userId)
        screens  <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(updated.exists(h => flaskCharges(h.equipment.flask).contains(8))) &&
              assertTrue(screens.exists(_.text.contains("пополнена")))
    },

    test("FindEvent Spring → фляги в инвентаре пополнены") {
      import pangea.model.item.{FlaskEffect, Item, ItemType, Rarity}
      val flaskInInv = Item(2L, "Запасная фляга", 1L, Rarity.Gray, ItemType.Flask,
                        attack=0, accuracy=0, energy=0, armor=0, defence=0, evasion=0,
                        details = ItemDetails.Flask(FlaskEffect.HealPercent(25), charges = 0, maxCharges = 4))
      for {
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        invRepo   = TestInventoryRepository.withItems(List(flaskInInv))
        state     = DungeonState(heroDao, invRepo, content)
        _        <- TestRandom.feedInts(67, 99) // без засады
        _        <- state.action(testUser, tap("FindEvent"), renderer)
      } yield assertTrue(invRepo.snapshot.find(_.id == 2L).exists(i => flaskCharges(i).contains(4)))
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

    test("GoDarker на уровне 150 → дно лабиринта, выслеживание не запускается") {
      for {
        quad                              <- makeState(dungeonLevel = 150)
        (state, heroDao, renderer, _)      = quad
        result                            <- state.action(testUser, tap("GoDarker"), renderer)
        screens                           <- renderer.sentScreens
        updatedHero                       <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.exists(_.text.contains("самого дна"))) &&
              // экран обычный (не ожидание выслеживания)
              assertTrue(screens.last.choices.map(_.id).contains("FindEvent")) &&
              assertTrue(updatedHero.exists(_.dungeonLevel == 150))
    },

    test("enter на максимально доступном этаже → кнопка «к тьме» красная") {
      for {
        quad                      <- makeState(dungeonLevel = 5, maxDungeonLevel = 5)
        (state, _, renderer, _)    = quad
        _                         <- state.enter(testUser, renderer)
        screens                   <- renderer.sentScreens
      } yield assertTrue(colorOf(screens.head, "GoDarker").contains(ChoiceColor.Negative)) &&
              assertTrue(colorOf(screens.head, "GoLighter").contains(ChoiceColor.Positive))
    },

    test("enter на первом этаже → кнопка «к свету» красная") {
      for {
        quad                      <- makeState(dungeonLevel = 1, maxDungeonLevel = 3)
        (state, _, renderer, _)    = quad
        _                         <- state.enter(testUser, renderer)
        screens                   <- renderer.sentScreens
      } yield assertTrue(colorOf(screens.head, "GoLighter").contains(ChoiceColor.Negative)) &&
              assertTrue(colorOf(screens.head, "GoDarker").contains(ChoiceColor.Positive))
    },

    test("GoDarker без поверженной тьмы → запускает выслеживание (миазмы), не двигается") {
      for {
        quad                          <- makeState(dungeonLevel = 5, maxDungeonLevel = 5)
        (state, heroDao, renderer, _)  = quad
        result                        <- state.action(testUser, tap("GoDarker"), renderer)
        screens                       <- renderer.sentScreens
        updatedHero                   <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.exists(_.text.contains("миазмы тьмы"))) &&
              // на экране ожидания — «идти по следу» и «перестать искать», без обычных действий
              assertTrue(screens.last.choices.map(_.id) == List("GoDarker", "StopTracking")) &&
              assertTrue(updatedHero.exists(_.dungeonLevel == 5))
    },

    test("выслеживание идёт, время не вышло → повторный GoDarker сообщает, что след впереди") {
      for {
        quad                          <- makeState(dungeonLevel = 5, maxDungeonLevel = 5)
        (state, heroDao, renderer, _)  = quad
        _                             <- state.action(testUser, tap("GoDarker"), renderer) // старт таймера (2–5 мин)
        result                        <- state.action(testUser, tap("GoDarker"), renderer) // время ещё не вышло
        screens                       <- renderer.sentScreens
        updatedHero                   <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.exists(_.text.contains("всё ещё где-то впереди"))) &&
              assertTrue(updatedHero.exists(_.dungeonLevel == 5))
    },

    test("время выслеживания вышло → GoDarker выводит на Отмеченного целевого уровня (бой)") {
      import pangea.model.battle.SoloPveBattle
      for {
        quad                          <- makeState(dungeonLevel = 5, maxDungeonLevel = 5)
        (state, heroDao, renderer, _)  = quad
        _                             <- state.action(testUser, tap("GoDarker"), renderer) // старт таймера
        _                             <- TestClock.adjust(Duration.fromMillis(5L * 60L * 1000L)) // проматываем > макс. 5 мин
        result                        <- state.action(testUser, tap("GoDarker"), renderer)
        battleJson                    <- heroDao.readActiveBattle(userId)
        battle                         = battleJson.flatMap(_.as[SoloPveBattle].toOption)
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(battle.exists(_.monsterMarked)) &&
              assertTrue(battle.exists(_.monsterLvl == 6L)) // цель = текущий (5) + 1
    },

    test("перестать искать → сбрасывает выслеживание, возвращает обычный экран этажа") {
      for {
        quad                          <- makeState(dungeonLevel = 5, maxDungeonLevel = 5)
        (state, heroDao, renderer, _)  = quad
        _                             <- state.action(testUser, tap("GoDarker"), renderer)       // старт
        result                        <- state.action(testUser, tap("StopTracking"), renderer)   // без переспроса
        screens                       <- renderer.sentScreens
        updatedHero                   <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.exists(_.text.contains("рассеиваются"))) &&
              assertTrue(screens.last.choices.map(_.id).contains("FindEvent")) &&
              assertTrue(updatedHero.exists(_.dungeonLevel == 5))
    },

    test("GoDarker с поверженной тьмой (этаж < max) → спускается глубже") {
      for {
        quad                          <- makeState(dungeonLevel = 5, maxDungeonLevel = 6)
        (state, heroDao, renderer, _)  = quad
        result                        <- state.action(testUser, tap("GoDarker"), renderer)
        updatedHero                   <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(updatedHero.exists(_.dungeonLevel == 6))
    },

    test("GoLighter на первом этаже → не двигается, сообщение «выше некуда»") {
      for {
        quad                          <- makeState(dungeonLevel = 1, maxDungeonLevel = 3)
        (state, heroDao, renderer, _)  = quad
        result                        <- state.action(testUser, tap("GoLighter"), renderer)
        screens                       <- renderer.sentScreens
        updatedHero                   <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.exists(_.text.contains("Выше уже некуда"))) &&
              assertTrue(updatedHero.exists(_.dungeonLevel == 1))
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

    test("OpenCharacter → переходит в HeroStats и запоминает Dungeon как return_state") {
      for {
        quad                            <- makeState()
        (state, heroDao, renderer, _)    = quad
        result                          <- state.action(testUser, tap("OpenCharacter"), renderer)
        returnState                     <- heroDao.readReturnState(userId)
        screens                         <- renderer.sentScreens
      } yield assertTrue(result == StateType.HeroStats) &&
              assertTrue(returnState.contains(StateType.Dungeon)) &&
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
