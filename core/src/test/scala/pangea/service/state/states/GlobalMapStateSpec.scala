package pangea.service.state.states

import pangea.engine.SceneContent
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._

import java.time.Duration

object GlobalMapStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def makeState(hero: pangea.model.hero.Hero) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (GlobalMapState(heroDao, content), heroDao, renderer)

  private val baseHero    = TestFixtures.hero(userId)
  private val fullHp      = baseHero.baseStats.vit * 24L  // 240
  private val richHero    = baseHero.copy(gold = 500L, fightStats = baseHero.fightStats.copy(hp = fullHp))
  private val poorHero    = baseHero.copy(gold = 0L,   fightStats = baseHero.fightStats.copy(hp = 10L))
  private val traumaHero  = baseHero.copy(gold = 500L, traumaUntil = Some(Long.MaxValue))

  override def spec = suite("GlobalMapState")(

    test("enter → показывает HP, gold, кнопки Tavern и ReturnToDungeon") {
      for {
        triple              <- makeState(richHero)
        (state, _, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.text.contains("Кинэт")) &&
              assertTrue(screens.head.choices.map(_.id).contains("Tavern")) &&
              assertTrue(screens.head.choices.map(_.id).contains("ReturnToDungeon"))
    },

    test("ReturnToDungeon → переходит в Dungeon") {
      for {
        triple              <- makeState(richHero)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("ReturnToDungeon"), renderer)
      } yield assertTrue(result == StateType.Dungeon)
    },

    test("Tavern → экран таверны с кнопкой «Снять комнату» (RentRoom)") {
      for {
        triple              <- makeState(richHero)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("Tavern"), renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.GlobalMap) &&
              assertTrue(screens.exists(_.choices.map(_.id).contains("RentRoom")))
    },

    test("RentRoom → списывает gold, показывает комнату с кнопкой «Уйти»") {
      for {
        triple               <- makeState(richHero)
        (state, heroDao, renderer) = triple
        result               <- state.action(testUser, tap("RentRoom"), renderer)
        updated              <- heroDao.getHeroByUserId(userId)
        screens              <- renderer.sentScreens
      } yield assertTrue(result == StateType.GlobalMap) &&
              assertTrue(updated.exists(_.gold < 500L)) &&
              assertTrue(screens.exists(_.text.contains("сняли комнату"))) &&
              assertTrue(screens.last.choices.map(_.id) == List("LeaveRoom"))
    },

    test("RentRoom без золота → ошибка, gold не меняется") {
      for {
        triple               <- makeState(poorHero)
        (state, heroDao, renderer) = triple
        result               <- state.action(testUser, tap("RentRoom"), renderer)
        updated              <- heroDao.getHeroByUserId(userId)
        screens              <- renderer.sentScreens
      } yield assertTrue(result == StateType.GlobalMap) &&
              assertTrue(updated.exists(_.gold == 0L)) &&
              assertTrue(screens.exists(_.text.contains("Недостаточно")))
    },

    test("LeaveRoom раньше 3 часов → подтверждение Да/Нет, травма не снята") {
      for {
        triple               <- makeState(traumaHero)
        (state, heroDao, renderer) = triple
        _                    <- state.action(testUser, tap("RentRoom"), renderer)
        result               <- state.action(testUser, tap("LeaveRoom"), renderer)
        updated              <- heroDao.getHeroByUserId(userId)
        screens              <- renderer.sentScreens
      } yield assertTrue(result == StateType.GlobalMap) &&
              assertTrue(screens.last.text.contains("уверены")) &&
              assertTrue(screens.last.choices.map(_.id).toSet == Set("ConfirmLeaveRoom", "CancelLeaveRoom")) &&
              assertTrue(updated.exists(_.traumaUntil.isDefined))
    },

    test("ConfirmLeaveRoom → прерывает без исцеления, травма остаётся") {
      for {
        triple               <- makeState(traumaHero)
        (state, heroDao, renderer) = triple
        _                    <- state.action(testUser, tap("RentRoom"), renderer)
        _                    <- state.action(testUser, tap("ConfirmLeaveRoom"), renderer)
        updated              <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(updated.exists(_.traumaUntil.isDefined))
    },

    test("LeaveRoom спустя 3 часа → лечит травмы и возвращает в таверну") {
      for {
        triple               <- makeState(traumaHero)
        (state, heroDao, renderer) = triple
        _                    <- state.action(testUser, tap("RentRoom"), renderer)
        _                    <- TestClock.adjust(Duration.ofHours(3).plusSeconds(1))
        result               <- state.action(testUser, tap("LeaveRoom"), renderer)
        updated              <- heroDao.getHeroByUserId(userId)
        screens              <- renderer.sentScreens
      } yield assertTrue(result == StateType.GlobalMap) &&
              assertTrue(updated.exists(_.traumaUntil.isEmpty)) &&
              assertTrue(screens.exists(_.text.contains("излечены")))
    },

    test("LeaveTavern → возвращается к экрану города") {
      for {
        triple              <- makeState(richHero)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("LeaveTavern"), renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.GlobalMap) &&
              assertTrue(screens.exists(_.text.contains("Кинэт")))
    },

    test("enter → показывает кнопки Guild и Construction") {
      for {
        triple              <- makeState(baseHero)
        (state, _, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(screens.head.choices.map(_.id).contains("Guild")) &&
              assertTrue(screens.head.choices.map(_.id).contains("Construction"))
    },

    test("Guild → показывает заглушку, остаётся в GlobalMap") {
      for {
        triple              <- makeState(baseHero)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("Guild"), renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.GlobalMap) &&
              assertTrue(screens.exists(_.text.contains("Гильдия")))
    },

    test("Construction → показывает заглушку, остаётся в GlobalMap") {
      for {
        triple              <- makeState(baseHero)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("Construction"), renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.GlobalMap) &&
              assertTrue(screens.exists(_.text.contains("Стройка")))
    }
  )
}
