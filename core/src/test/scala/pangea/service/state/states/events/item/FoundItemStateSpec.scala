package pangea.service.state.states.events.item

import pangea.engine.SceneContent
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestJournal, TestRenderer}
import zio.test._
import zio.ZIO

object FoundItemStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def makeState(canAddItem: Boolean = true) =
    for {
      renderer <- TestRenderer.make
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
      journal  <- TestJournal.make
      content  <- ZIO.attempt(SceneContent.load())
      invRepo   = if (canAddItem) TestInventoryRepository.accepting else TestInventoryRepository.full
      state     = FoundItemState(heroDao, invRepo, journal, content)
    } yield (state, renderer, heroDao)

  override def spec = suite("FoundItemState")(

    test("enter → сохраняет предмет в scene_data, показывает экран находки") {
      for {
        triple                    <- makeState()
        (state, renderer, heroDao) = triple
        _                         <- state.enter(testUser, renderer)
        screens                   <- renderer.sentScreens
        sceneData                 <- heroDao.readSceneData(userId)
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.text.contains("находите предмет")) &&
              assertTrue(sceneData.isDefined)
    },

    test("TakeItem → берёт предмет из scene_data, отправляет подтверждение, переходит в Dungeon") {
      for {
        triple              <- makeState()
        (state, renderer, _) = triple
        _                   <- state.enter(testUser, renderer)
        result              <- state.action(testUser, tap("TakeItem"), renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.exists(_.text.startsWith("Вы получили")))
    },

    test("TakeItem при полном инвентаре → сообщает об ошибке, переходит в Dungeon") {
      for {
        triple              <- makeState(canAddItem = false)
        (state, renderer, _) = triple
        _                   <- state.enter(testUser, renderer)
        result              <- state.action(testUser, tap("TakeItem"), renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.exists(_.text.contains("нет места")))
    },

    test("DontTakeItem → отказывается от предмета, переходит в Dungeon") {
      for {
        triple              <- makeState()
        (state, renderer, _) = triple
        _                   <- state.enter(testUser, renderer)
        result              <- state.action(testUser, tap("DontTakeItem"), renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(screens.exists(_.text.contains("уходите")))
    },

    test("неизвестный ввод → перепоказывает экран находки, остаётся в FoundItem") {
      for {
        triple              <- makeState()
        (state, renderer, _) = triple
        result              <- state.action(testUser, UserAction("что угодно", None), renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.FoundItem) &&
              assertTrue(screens.nonEmpty)
    },

    test("enter → экран содержит имя предмета и кнопки Взять/Оставить") {
      for {
        triple                    <- makeState()
        (state, renderer, heroDao) = triple
        _                         <- state.enter(testUser, renderer)
        screens                   <- renderer.sentScreens
        sceneData                 <- heroDao.readSceneData(userId)
      } yield assertTrue(screens.head.choices.map(_.id).contains("TakeItem")) &&
              assertTrue(screens.head.choices.map(_.id).contains("DontTakeItem")) &&
              assertTrue(sceneData.isDefined)
    }
  )
}
