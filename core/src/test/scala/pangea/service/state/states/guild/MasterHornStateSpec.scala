package pangea.service.state.states.guild

import pangea.engine.SceneContent
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.ZIO
import zio.test._

object MasterHornStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  // Первая прокачка любого стата стоит 5 (см. MasterHornState.cost, n=1).
  private def hero(reputation: Long, silver: Long) =
    TestFixtures.hero(userId).copy(guildReputation = reputation, silver = silver)

  private def makeState(reputation: Long, silver: Long) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero(reputation, silver))
      invRepo   = TestInventoryRepository.accepting
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (MasterHornState(heroDao, invRepo, content), heroDao, renderer)

  override def spec = suite("MasterHornState")(

    test("хватает и репутации, и серебра → списывает поровну обе валюты, характеристика растёт") {
      for {
        t <- makeState(reputation = 100L, silver = 100L)
        (state, heroDao, renderer) = t
        _    <- state.action(testUser, tap("ImproveArmor"), renderer)
        _    <- state.action(testUser, tap("ConfirmImprove"), renderer)
        hero <- heroDao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(hero.guildReputation == 95L) &&
              assertTrue(hero.silver == 95L) &&
              assertTrue(hero.masterHornBoosts.armor == 3L)
    },

    test("не хватает репутации → отказ, ни репутация, ни серебро не списаны") {
      for {
        t <- makeState(reputation = 4L, silver = 100L)
        (state, heroDao, renderer) = t
        _       <- state.action(testUser, tap("ImproveArmor"), renderer)
        _       <- state.action(testUser, tap("ConfirmImprove"), renderer)
        hero    <- heroDao.getHeroByUserId(userId).map(_.get)
        screens <- renderer.sentScreens
      } yield assertTrue(hero.guildReputation == 4L) &&
              assertTrue(hero.silver == 100L) &&
              assertTrue(hero.masterHornBoosts.armor == 0L) &&
              assertTrue(screens.exists(_.text.contains("Недостаточно репутации")))
    },

    test("хватает репутации, но не хватает серебра → отказ, ни репутация, ни серебро не списаны") {
      for {
        t <- makeState(reputation = 100L, silver = 4L)
        (state, heroDao, renderer) = t
        _       <- state.action(testUser, tap("ImproveArmor"), renderer)
        _       <- state.action(testUser, tap("ConfirmImprove"), renderer)
        hero    <- heroDao.getHeroByUserId(userId).map(_.get)
        screens <- renderer.sentScreens
      } yield assertTrue(hero.guildReputation == 100L) &&
              assertTrue(hero.silver == 4L) &&
              assertTrue(hero.masterHornBoosts.armor == 0L) &&
              assertTrue(screens.exists(_.text.contains("Недостаточно серебра")))
    },

    test("LeaveMasterHorn → переход в TrainingHall") {
      for {
        t <- makeState(reputation = 0L, silver = 0L)
        (state, _, renderer) = t
        result <- state.action(testUser, tap("LeaveMasterHorn"), renderer)
      } yield assertTrue(result == StateType.TrainingHall)
    }
  )
}
