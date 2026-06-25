package pangea.service.state.states.tavern

import pangea.engine.SceneContent
import pangea.model.quest.QuestData
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.{Task, ZIO}
import zio.test._

import java.time.Duration

object QuestBoardStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def makeState =
    for {
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId, state = StateType.QuestBoard))
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (QuestBoardState(heroDao, content), heroDao, renderer)

  private def readQuests(heroDao: TestHeroDao): Task[QuestData] =
    heroDao.readQuestData(userId)
      .map(_.flatMap(_.as[QuestData].toOption))
      .flatMap(o => ZIO.fromOption(o).orElseFail(new Throwable("no quest data")))

  override def spec = suite("QuestBoardState")(

    test("enter → катает пул из 3 заданий и кнопки Взять/Отказаться/Вернуться") {
      for {
        t <- makeState
        (state, heroDao, renderer) = t
        _       <- state.enter(testUser, renderer)
        data    <- readQuests(heroDao)
        screens <- renderer.sentScreens
        ids      = screens.last.choices.map(_.id)
      } yield assertTrue(data.remaining == 3) &&
              assertTrue(data.current.isDefined) &&
              assertTrue(data.active.isEmpty) &&
              assertTrue(ids.toSet == Set("TakeQuest", "AbandonQuest", "OpenCharacter", "BackFromQuest"))
    },

    test("TakeQuest → списывает слот, делает задание активным, роллит следующее") {
      for {
        t <- makeState
        (state, heroDao, renderer) = t
        _       <- state.enter(testUser, renderer)
        before  <- readQuests(heroDao)
        _       <- state.action(testUser, tap("TakeQuest"), renderer)
        after   <- readQuests(heroDao)
        screens <- renderer.sentScreens
      } yield assertTrue(after.remaining == 2) &&
              assertTrue(after.active.contains(before.current.get)) &&
              assertTrue(after.current.isDefined) &&
              assertTrue(screens.exists(_.text.contains("Задание принято")))
    },

    test("второй TakeQuest → предупреждение про один активный квест, активный заменён") {
      for {
        t <- makeState
        (state, heroDao, renderer) = t
        _       <- state.enter(testUser, renderer)
        _       <- state.action(testUser, tap("TakeQuest"), renderer)
        mid     <- readQuests(heroDao)
        _       <- state.action(testUser, tap("TakeQuest"), renderer)
        after   <- readQuests(heroDao)
        screens <- renderer.sentScreens
      } yield assertTrue(after.remaining == 1) &&
              assertTrue(after.active.contains(mid.current.get)) &&
              assertTrue(screens.exists(_.text.contains("Активным может быть только одно задание")))
    },

    test("AbandonQuest → сбрасывает активное задание") {
      for {
        t <- makeState
        (state, heroDao, renderer) = t
        _       <- state.enter(testUser, renderer)
        _       <- state.action(testUser, tap("TakeQuest"), renderer)
        _       <- state.action(testUser, tap("AbandonQuest"), renderer)
        after   <- readQuests(heroDao)
        screens <- renderer.sentScreens
      } yield assertTrue(after.active.isEmpty) &&
              assertTrue(screens.exists(_.text.contains("отказались")))
    },

    test("AbandonQuest без активного → сообщение, что отказываться не от чего") {
      for {
        t <- makeState
        (state, _, renderer) = t
        _       <- state.enter(testUser, renderer)
        _       <- state.action(testUser, tap("AbandonQuest"), renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(screens.exists(_.text.contains("нет активного задания")))
    },

    test("взяты все 3 задания → пустая доска с обратным отсчётом, без кнопки Взять") {
      for {
        t <- makeState
        (state, heroDao, renderer) = t
        _       <- state.enter(testUser, renderer)
        _       <- state.action(testUser, tap("TakeQuest"), renderer)
        _       <- state.action(testUser, tap("TakeQuest"), renderer)
        _       <- state.action(testUser, tap("TakeQuest"), renderer)
        after   <- readQuests(heroDao)
        screens <- renderer.sentScreens
        ids      = screens.last.choices.map(_.id)
      } yield assertTrue(after.remaining == 0) &&
              assertTrue(after.current.isEmpty) &&
              assertTrue(!ids.contains("TakeQuest")) &&
              assertTrue(screens.last.text.contains("появятся через"))
    },

    test("по истечении 20 часов пул обновляется до 3, активное задание сохраняется") {
      for {
        t <- makeState
        (state, heroDao, renderer) = t
        _       <- state.enter(testUser, renderer)
        _       <- state.action(testUser, tap("TakeQuest"), renderer) // remaining 2, active set
        active  <- readQuests(heroDao).map(_.active)
        _       <- TestClock.adjust(Duration.ofHours(20).plusSeconds(1))
        _       <- state.enter(testUser, renderer)
        after   <- readQuests(heroDao)
      } yield assertTrue(after.remaining == 3) &&
              assertTrue(after.active == active)
    },

    test("BackFromQuest → переход в Tavern") {
      for {
        t <- makeState
        (state, _, renderer) = t
        result <- state.action(testUser, tap("BackFromQuest"), renderer)
      } yield assertTrue(result == StateType.Tavern)
    }
  )
}
