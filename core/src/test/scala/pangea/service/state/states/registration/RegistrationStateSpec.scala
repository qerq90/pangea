package pangea.service.state.states.registration

import pangea.engine.SceneContent
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestJournal, TestPlayers, TestRenderer}
import zio.test._
import zio.ZIO

object RegistrationStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(actionId: String): UserAction =
    UserAction("", Some(s"""{"action":"$actionId"}"""))

  private def raceDescription(race: Race): UserAction =
    UserAction(race.entryName, Some("""{"action":"RaceDescription"}"""))

  private def confirmRace(race: Race): UserAction =
    UserAction("", Some(s"""{"action":"Travel","race":"${race.entryName}"}"""))

  private def makeState =
    for {
      renderer <- TestRenderer.make
      heroDao  <- TestHeroDao.make
      journal  <- TestJournal.make
      content  <- ZIO.attempt(SceneContent.load())
      invRepo   = TestInventoryRepository.accepting
    } yield (RegistrationState(new TestPlayers, heroDao, invRepo, TestItemRepository.make, journal, content), renderer, heroDao, invRepo)

  private def makeStateWithHero =
    for {
      renderer <- TestRenderer.make
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
      journal  <- TestJournal.make
      content  <- ZIO.attempt(SceneContent.load())
      invRepo   = TestInventoryRepository.accepting
    } yield (RegistrationState(new TestPlayers, heroDao, invRepo, TestItemRepository.make, journal, content), renderer, heroDao, invRepo)

  override def spec = suite("RegistrationState")(

    test("неизвестный ввод → приветствие, остаётся в Registration") {
      for {
        quad                        <- makeState
        (state, renderer, _, _)      = quad
        result                      <- state.action(testUser, UserAction("привет", None), renderer)
        screens                     <- renderer.sentScreens
      } yield assertTrue(result == StateType.Registration) &&
              assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.text.startsWith("Добро пожаловать на Пангею!"))
    },

    test("Race → показывает выбор расы, остаётся в Registration") {
      for {
        quad                   <- makeState
        (state, renderer, _, _) = quad
        result                 <- state.action(testUser, tap("Race"), renderer)
        screens                <- renderer.sentScreens
      } yield assertTrue(result == StateType.Registration) &&
              assertTrue(screens.nonEmpty)
    },

    test("RaceDescription → показывает описание выбранной расы") {
      for {
        quad                   <- makeState
        (state, renderer, _, _) = quad
        result                 <- state.action(testUser, raceDescription(Race.Human), renderer)
        screens                <- renderer.sentScreens
      } yield assertTrue(result == StateType.Registration) &&
              assertTrue(screens.head.text == Race.Human.description)
    },

    test("confirmRace → сохраняет расу героя, остаётся в Registration") {
      for {
        quad                        <- makeState
        (state, renderer, heroDao, _) = quad
        result                      <- state.action(testUser, confirmRace(Race.Elf), renderer)
        snap                        <- heroDao.raceSnapshot
      } yield assertTrue(result == StateType.Registration) &&
              assertTrue(snap.get(testUser.userId).contains(Race.Elf))
    },

    test("Travel1..Travel9 → каждый шаг остаётся в Registration") {
      val travels = List("Travel1", "Travel2", "Travel3", "Travel4", "Travel5",
                         "Travel6", "Travel7", "Travel8", "Travel9")
      for {
        quad                   <- makeState
        (state, renderer, _, _) = quad
        results                <- ZIO.foreach(travels)(id => state.action(testUser, tap(id), renderer))
      } yield assertTrue(results.forall(_ == StateType.Registration))
    },

    test("EndOfTravel → переходит в Dungeon") {
      for {
        quad                   <- makeState
        (state, renderer, _, _) = quad
        result                 <- state.action(testUser, tap("EndOfTravel"), renderer)
      } yield assertTrue(result == StateType.Dungeon)
    },

    test("EndOfTravel с героем → добавляет стартовые предметы (меч + фляга) в инвентарь") {
      for {
        quad                           <- makeStateWithHero
        (state, renderer, _, invRepo)   = quad
        result                         <- state.action(testUser, tap("EndOfTravel"), renderer)
        screens                        <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(invRepo.snapshot.length == 2) &&
              assertTrue(invRepo.snapshot.map(_.name).contains("Меч новобранца")) &&
              assertTrue(invRepo.snapshot.map(_.name).contains("Фляга начинающего исследователя")) &&
              assertTrue(screens.exists(_.text.contains("снаряжение")))
    },

    test("полный флоу регистрации завершается в Dungeon") {
      for {
        quad                            <- makeState
        (state, renderer, heroDao, _)    = quad
        _                               <- state.action(testUser, tap("Race"), renderer)
        _                               <- state.action(testUser, raceDescription(Race.Human), renderer)
        _                               <- state.action(testUser, confirmRace(Race.Human), renderer)
        _                               <- ZIO.foreach(List("Travel1", "Travel2", "Travel3", "Travel4",
                                                            "Travel5", "Travel6", "Travel7", "Travel8",
                                                            "Travel9"))(id => state.action(testUser, tap(id), renderer))
        result                          <- state.action(testUser, tap("EndOfTravel"), renderer)
        snap                            <- heroDao.raceSnapshot
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(snap.get(testUser.userId).contains(Race.Human))
    }
  )
}
