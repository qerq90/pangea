package pangea.service.state.states.tavern

import pangea.engine.SceneContent
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._

import java.time.Duration

object TavernStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def makeState(hero: pangea.model.hero.Hero) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (TavernState(heroDao, content), heroDao, renderer)

  private val baseHero   = TestFixtures.hero(userId, state = StateType.Tavern)
  private val richHero   = baseHero.copy(gold = 500L)
  private val poorHero   = baseHero.copy(gold = 0L)
  private val traumaHero = baseHero.copy(gold = 500L, traumaUntil = Some(Long.MaxValue))

  override def spec = suite("TavernState")(

    test("enter → меню таверны с кнопками RentRoom, QuestBoard, Innkeeper") {
      for {
        triple              <- makeState(richHero)
        (state, _, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(screens.head.choices.map(_.id).contains("RentRoom")) &&
              assertTrue(screens.head.choices.map(_.id).contains("QuestBoard")) &&
              assertTrue(screens.head.choices.map(_.id).contains("Innkeeper"))
    },

    test("QuestBoard → переходит в QuestBoard") {
      for {
        triple              <- makeState(richHero)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("QuestBoard"), renderer)
      } yield assertTrue(result == StateType.QuestBoard)
    },

    test("Innkeeper → переходит в Innkeeper") {
      for {
        triple              <- makeState(richHero)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("Innkeeper"), renderer)
      } yield assertTrue(result == StateType.Innkeeper)
    },

    test("LeaveTavern → переходит в GlobalMap") {
      for {
        triple              <- makeState(richHero)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("LeaveTavern"), renderer)
      } yield assertTrue(result == StateType.GlobalMap)
    },

    test("RentRoom → списывает gold, показывает комнату с кнопкой «Уйти»") {
      for {
        triple                     <- makeState(richHero)
        (state, heroDao, renderer)  = triple
        result                     <- state.action(testUser, tap("RentRoom"), renderer)
        updated                    <- heroDao.getHeroByUserId(userId)
        screens                    <- renderer.sentScreens
      } yield assertTrue(result == StateType.Tavern) &&
              assertTrue(updated.exists(_.gold < 500L)) &&
              assertTrue(screens.exists(_.text.contains("сняли комнату"))) &&
              assertTrue(screens.last.choices.map(_.id) == List("LeaveRoom"))
    },

    test("RentRoom без золота → ошибка, gold не меняется") {
      for {
        triple                     <- makeState(poorHero)
        (state, heroDao, renderer)  = triple
        result                     <- state.action(testUser, tap("RentRoom"), renderer)
        updated                    <- heroDao.getHeroByUserId(userId)
        screens                    <- renderer.sentScreens
      } yield assertTrue(result == StateType.Tavern) &&
              assertTrue(updated.exists(_.gold == 0L)) &&
              assertTrue(screens.exists(_.text.contains("Недостаточно")))
    },

    test("LeaveRoom раньше 3 часов → подтверждение Да/Нет, травма не снята") {
      for {
        triple                     <- makeState(traumaHero)
        (state, heroDao, renderer)  = triple
        _                          <- state.action(testUser, tap("RentRoom"), renderer)
        result                     <- state.action(testUser, tap("LeaveRoom"), renderer)
        updated                    <- heroDao.getHeroByUserId(userId)
        screens                    <- renderer.sentScreens
      } yield assertTrue(result == StateType.Tavern) &&
              assertTrue(screens.last.text.contains("уверены")) &&
              assertTrue(screens.last.choices.map(_.id).toSet == Set("ConfirmLeaveRoom", "CancelLeaveRoom")) &&
              assertTrue(updated.exists(_.traumaUntil.isDefined))
    },

    test("ConfirmLeaveRoom → прерывает без исцеления, травма остаётся") {
      for {
        triple                     <- makeState(traumaHero)
        (state, heroDao, renderer)  = triple
        _                          <- state.action(testUser, tap("RentRoom"), renderer)
        _                          <- state.action(testUser, tap("ConfirmLeaveRoom"), renderer)
        updated                    <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(updated.exists(_.traumaUntil.isDefined))
    },

    test("LeaveRoom спустя 3 часа → лечит травмы и возвращает в меню таверны") {
      for {
        triple                     <- makeState(traumaHero)
        (state, heroDao, renderer)  = triple
        _                          <- state.action(testUser, tap("RentRoom"), renderer)
        _                          <- TestClock.adjust(Duration.ofHours(3).plusSeconds(1))
        result                     <- state.action(testUser, tap("LeaveRoom"), renderer)
        updated                    <- heroDao.getHeroByUserId(userId)
        screens                    <- renderer.sentScreens
      } yield assertTrue(result == StateType.Tavern) &&
              assertTrue(updated.exists(_.traumaUntil.isEmpty)) &&
              assertTrue(screens.exists(_.text.contains("излечены")))
    }
  )
}
