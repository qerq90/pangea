package pangea.service.state.states.events

import io.circe.Json
import pangea.engine.SceneContent
import pangea.model.item.GemKind
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.events.ElementalSearchState.SearchScene
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer, TestScheduler}
import zio.ZIO
import zio.test._
import zio.test.TestRandom

/** Осмотр логова: находки приходят сами по таймеру, время игроку не показывают. */
object ElementalSearchStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def makeState(heroLvl: Long = 15L) = // BossLvL = 2
    for {
      dao       <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(lvl = heroLvl))
      invRepo    = TestInventoryRepository.accepting
      itemRepo   = TestItemRepository.make
      scheduler <- TestScheduler.make
      renderer  <- TestRenderer.make
      content   <- ZIO.attempt(SceneContent.load())
    } yield (ElementalSearchState(dao, invRepo, itemRepo, scheduler, content), dao, invRepo, scheduler, renderer)

  private def sceneOf(dao: TestHeroDao) =
    dao.readSceneData(userId).map(_.flatMap(_.as[SearchScene].toOption))

  override def spec = suite("ElementalSearchState")(

    test("вход: считает попытки по BossLvL и ставит таймер, не показывая время") {
      for {
        t <- makeState()
        (state, dao, _, scheduler, renderer) = t
        _        <- TestRandom.feedLongs(1L, 0L) // бонус попыток = 1, задержка = минимум
        _        <- state.enter(testUser, renderer)
        scene    <- sceneOf(dao)
        tasks    <- scheduler.scheduled
        screens  <- renderer.sentScreens
      } yield assertTrue(scene.exists(_.triesLeft == 3)) && // BossLvL 2 + бонус 1
              assertTrue(tasks.exists(_.kind == TaskKind.ElementalSearch)) &&
              assertTrue(screens.last.text.contains("изучаете окрестности")) &&
              // время до находки не раскрываем ни в тексте, ни в кнопках
              assertTrue(!screens.last.text.contains("мин")) &&
              assertTrue(screens.last.choices.map(_.id) == List("LeaveSearch"))
    },

    test("находка кладёт камень в сумку и ставит следующий таймер") {
      for {
        t <- makeState()
        (state, dao, invRepo, scheduler, renderer) = t
        _       <- TestRandom.feedLongs(1L, 0L)
        _       <- state.enter(testUser, renderer)
        // Находка: вид камня, качество, затем задержка следующего таймера.
        _       <- TestRandom.feedInts(0, 50)
        _       <- TestRandom.feedLongs(0L)
        result  <- state.action(testUser, tap("ElementalFind"), renderer)
        scene   <- sceneOf(dao)
        screens <- renderer.sentScreens
      } yield assertTrue(result == StateType.ElementalSearch) &&
              assertTrue(invRepo.snapshot.size == 1) &&
              assertTrue(scene.exists(_.triesLeft == 2)) &&
              assertTrue(screens.map(_.text).mkString.contains("Вы нашли"))
    },

    test("на последней попытке поиск заканчивается и уводит в лабиринт") {
      for {
        t <- makeState()
        (state, dao, _, _, renderer) = t
        _       <- dao.writeSceneData(userId, Json.obj("triesLeft" -> Json.fromInt(1)))
        _       <- TestRandom.feedInts(0, 50)
        result  <- state.action(testUser, tap("ElementalFind"), renderer)
        scene   <- dao.readSceneData(userId)
        screens <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(scene.contains(Json.Null)) &&
              assertTrue(screens.map(_.text).mkString.contains("нашёл все что мог"))
    },

    test("уйти можно в любой момент — таймер снимается, попытки пропадают") {
      for {
        t <- makeState()
        (state, dao, _, scheduler, renderer) = t
        _         <- TestRandom.feedLongs(1L, 0L)
        _         <- state.enter(testUser, renderer)
        result    <- state.action(testUser, tap("LeaveSearch"), renderer)
        scene     <- dao.readSceneData(userId)
        cancelled <- scheduler.cancelled
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(scene.contains(Json.Null)) &&
              assertTrue(cancelled.map(_._2).contains(TaskKind.ElementalSearch))
    },

    test("череп в логове не находится, а качество катается 90/9/1") {
      // Прогоняем все виды и границы качества через саму таблицу состояния.
      val kinds = ElementalSearchState.DroppableKinds
      assertTrue(!kinds.contains(GemKind.Skull)) &&
      assertTrue(kinds.size == GemKind.values.size - 1) &&
      assertTrue(ElementalSearchState.CrackedPct == 90) &&
      assertTrue(ElementalSearchState.DamagedPct == 9)
    },

    test("качество: 90 — надколотый, 99 — повреждённый, 100 — цельный") {
      def gradeFor(roll: Int) =
        for {
          t <- makeState()
          (state, _, invRepo, _, renderer) = t
          _ <- t._2.writeSceneData(userId, Json.obj("triesLeft" -> Json.fromInt(5)))
          _ <- TestRandom.feedInts(0, roll)
          _ <- TestRandom.feedLongs(0L)
          _ <- state.action(testUser, tap("ElementalFind"), renderer)
        } yield invRepo.snapshot.head.gem.map(_.grade)
      for {
        cracked <- gradeFor(90)
        damaged <- gradeFor(99)
        whole   <- gradeFor(100)
      } yield assertTrue(cracked.contains(1)) &&
              assertTrue(damaged.contains(2)) &&
              assertTrue(whole.contains(ElementalSearchState.WholeGrade))
    }
  )
}
