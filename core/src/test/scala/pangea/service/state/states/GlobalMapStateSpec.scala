package pangea.service.state.states

import pangea.engine.SceneContent
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._

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

    test("Tavern → переходит в Tavern") {
      for {
        triple              <- makeState(richHero)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("Tavern"), renderer)
      } yield assertTrue(result == StateType.Tavern)
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

    test("Construction → переходит в Construction") {
      for {
        triple              <- makeState(baseHero)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("Construction"), renderer)
      } yield assertTrue(result == StateType.Construction)
    }
  )
}
