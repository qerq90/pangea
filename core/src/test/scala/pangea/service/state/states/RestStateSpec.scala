package pangea.service.state.states

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._
import zio.test.TestClock

object RestStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private val anyAction: UserAction = UserAction("", Some("""{"action":"Wakeup"}"""))

  private def makeState(hero: pangea.model.hero.Hero) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (RestState(heroDao, content), heroDao, renderer)

  override def spec = suite("RestState")(

    test("enter → показывает экран отдыха, записывает restStartedAt") {
      for {
        triple              <- makeState(TestFixtures.hero(userId))
        (state, heroDao, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        screens             <- renderer.sentScreens
        sceneData           <- heroDao.readSceneData(userId)
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.choices.map(_.id).contains("Wakeup")) &&
              assertTrue(sceneData.isDefined) &&
              assertTrue(sceneData.flatMap(_.hcursor.get[Long]("restStartedAt").toOption).isDefined)
    },

    test("action до 30с → показывает 'ещё X', остаётся в Rest") {
      for {
        triple              <- makeState(TestFixtures.hero(userId))
        (state, _, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        result              <- state.action(testUser, anyAction, renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.Rest) &&
              assertTrue(screens.exists(s => s.text.contains("с") || s.text.contains("м")))
    },

    test("action после 30с → восстанавливает HP, переходит в Dungeon") {
      for {
        triple              <- makeState(TestFixtures.hero(userId))
        (state, heroDao, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        _                   <- TestClock.adjust(java.time.Duration.ofSeconds(31))
        result              <- state.action(testUser, anyAction, renderer)
        updatedHero         <- heroDao.getHeroByUserId(userId)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(updatedHero.exists(_.fightStats.hp == updatedHero.get.baseStats.vit * 16L)) &&
              assertTrue(screens.exists(_.text.contains("отдохнули")))
    },

    test("action после 30с → восстанавливает armor до maxArmor") {
      val depletedHero = TestFixtures.hero(userId).copy(
        fightStats = TestFixtures.hero(userId).fightStats.copy(armor = 0L)
      )
      for {
        triple              <- makeState(depletedHero)
        (state, heroDao, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        _                   <- TestClock.adjust(java.time.Duration.ofSeconds(31))
        _                   <- state.action(testUser, anyAction, renderer)
        updatedHero         <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(updatedHero.exists(_.fightStats.armor == updatedHero.get.maxArmor))
    },

    test("post-death recovery: если DeathState записал restDurationMs=2мин, enter читает его и action ждёт полные 2 мин") {
      val deathRestMs = 2L * 60L * 1000L  // level 1 death recovery
      for {
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        // simulate what DeathState writes before transitioning to Rest
        _        <- heroDao.writeSceneData(userId, Json.obj("restDurationMs" -> deathRestMs.asJson))
        state     = RestState(heroDao, content)
        _        <- state.enter(testUser, renderer)
        // 30 seconds not enough for post-death recovery
        _        <- TestClock.adjust(java.time.Duration.ofSeconds(31))
        resultEarly <- state.action(testUser, anyAction, renderer)
        // advance past the full 2 minutes
        _        <- TestClock.adjust(java.time.Duration.ofSeconds(91))
        resultDone  <- state.action(testUser, anyAction, renderer)
      } yield assertTrue(resultEarly == StateType.Rest) &&
              assertTrue(resultDone  == StateType.Dungeon)
    }
  )
}
