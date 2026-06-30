package pangea.service.state.states.events.treasure

import pangea.engine.SceneContent
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.LootState.LootData
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer, TestScheduler}
import zio.ZIO
import zio.test._

object TreasureDigStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def makeState =
    for {
      renderer  <- TestRenderer.make
      heroDao   <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(dungeonLevel = 20))
      scheduler <- TestScheduler.make
      content   <- ZIO.attempt(SceneContent.load())
      state      = TreasureDigState(heroDao, scheduler, content)
    } yield (state, renderer, heroDao, scheduler)

  override def spec = suite("TreasureDigState")(

    test("enter → планирует SchronDig, пишет прогресс в scene_data, показывает экран раскопок с «Уйти»") {
      for {
        t <- makeState
        (state, renderer, heroDao, scheduler) = t
        _         <- state.enter(testUser, renderer)
        screens   <- renderer.sentScreens
        scheduled <- scheduler.scheduled
        progress  <- heroDao.readSceneData(userId).map(_.flatMap(_.as[TreasureDigProgress].toOption))
      } yield assertTrue(progress.isDefined) &&
              assertTrue(scheduled.exists(s => s.kind == TaskKind.SchronDig && s.expectedState == StateType.TreasureDig)) &&
              assertTrue(screens.head.choices.map(_.id).contains("LeaveDig"))
    },

    test("LeaveDig до конца → экран подтверждения, остаётся в TreasureDig") {
      for {
        t <- makeState
        (state, renderer, _, _) = t
        _      <- state.enter(testUser, renderer)
        result <- state.action(testUser, tap("LeaveDig"), renderer)
        screens <- renderer.sentScreens
      } yield assertTrue(result == StateType.TreasureDig) &&
              assertTrue(screens.exists(_.choices.map(_.id).toSet == Set("ConfirmLeaveDig", "CancelLeaveDig")))
    },

    test("ConfirmLeaveDig → снимает SchronDig, чистит scene_data, уходит в Dungeon") {
      for {
        t <- makeState
        (state, renderer, heroDao, scheduler) = t
        _         <- state.enter(testUser, renderer)
        result    <- state.action(testUser, tap("ConfirmLeaveDig"), renderer)
        cancelled <- scheduler.cancelled
        scene     <- heroDao.readSceneData(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(cancelled.contains(userId -> TaskKind.SchronDig)) &&
              assertTrue(scene.flatMap(_.as[TreasureDigProgress].toOption).isEmpty)
    },

    test("DigDone успех (roll≤80) → схрон в scene_data, уход в Loot, SchronDig снят") {
      for {
        t <- makeState
        (state, renderer, heroDao, scheduler) = t
        _         <- state.enter(testUser, renderer)
        _         <- TestRandom.feedInts(0, 0) // roll=1 (успех), раса
        _         <- TestRandom.feedLongs(5L)  // seed схрона
        result    <- state.action(testUser, tap("DigDone"), renderer)
        screens   <- renderer.sentScreens
        cancelled <- scheduler.cancelled
        loot      <- heroDao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
      } yield assertTrue(result == StateType.Loot) &&
              assertTrue(screens.exists(_.text.contains("ровно так"))) &&
              assertTrue(cancelled.contains(userId -> TaskKind.SchronDig)) &&
              assertTrue(loot.exists(l => l.items.nonEmpty || l.golds.nonEmpty)) &&
              assertTrue(loot.exists(l => l.doubloons == 0L || (l.doubloons >= 1L && l.doubloons <= 2L)))
    },

    test("DigDone провал (roll>80) → «свежая могила» с кнопкой Уйти, scene_data очищен, остаётся в TreasureDig") {
      for {
        t <- makeState
        (state, renderer, heroDao, scheduler) = t
        _         <- state.enter(testUser, renderer)
        _         <- TestRandom.feedInts(90, 0) // roll=91 (провал), раса
        result    <- state.action(testUser, tap("DigDone"), renderer)
        screens   <- renderer.sentScreens
        cancelled <- scheduler.cancelled
        scene     <- heroDao.readSceneData(userId)
      } yield assertTrue(result == StateType.TreasureDig) &&
              assertTrue(screens.exists(s => s.text.contains("могила") && s.choices.map(_.id).contains("GraveLeave"))) &&
              assertTrue(cancelled.contains(userId -> TaskKind.SchronDig)) &&
              assertTrue(scene.flatMap(_.as[TreasureDigProgress].toOption).isEmpty)
    },

    test("GraveLeave → переход в Dungeon") {
      for {
        t <- makeState
        (state, renderer, _, _) = t
        result <- state.action(testUser, tap("GraveLeave"), renderer)
      } yield assertTrue(result == StateType.Dungeon)
    }
  )
}
