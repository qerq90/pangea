package pangea.service.state.states.temple

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.hero.{AzatState, CubeStatus}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._

object TempleAzatSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero(gold: Long = 0L, doubloons: Long = 0L) =
    TestFixtures.hero(userId).copy(gold = gold, doubloons = doubloons)

  private def readAzat(dao: TestHeroDao) =
    dao.readAzatData(userId).map(_.flatMap(_.as[AzatState].toOption).getOrElse(AzatState.empty))

  private def doubloonsOf(dao: TestHeroDao) =
    dao.getHeroByUserId(userId).map(_.get.doubloons)

  override def spec = suite("TempleAzat / HallAzat")(

    test("Donate: 250 дублонов → недельное благословение + 250 отдыхов, дублоны списаны") {
      for {
        dao      <- TestHeroDao.withHero(userId, hero(doubloons = 300L))
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = TempleAzatState(dao, content)
        _        <- state.action(testUser, tap("Donate"), renderer)
        azat     <- readAzat(dao)
        left     <- doubloonsOf(dao)
      } yield assertTrue(azat.blessingUntil.isDefined) &&
              assertTrue(azat.instantRests == AzatState.BlessingInstantRests) &&
              assertTrue(left == 50L)
    },

    test("Donate без дублонов → благословение не выдаётся") {
      for {
        dao      <- TestHeroDao.withHero(userId, hero(doubloons = 10L))
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = TempleAzatState(dao, content)
        _        <- state.action(testUser, tap("Donate"), renderer)
        azat     <- readAzat(dao)
        left     <- doubloonsOf(dao)
      } yield assertTrue(azat.blessingUntil.isEmpty) && assertTrue(left == 10L)
    },

    test("BuyCube: 200 дублонов → активный куб с 50 зарядами, дублоны списаны") {
      for {
        dao      <- TestHeroDao.withHero(userId, hero(doubloons = 300L))
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = HallAzatState(dao, content)
        _        <- state.action(testUser, tap("BuyCube"), renderer)
        azat     <- readAzat(dao)
        left     <- doubloonsOf(dao)
      } yield assertTrue(azat.cube == CubeStatus.Active) &&
              assertTrue(azat.cubeCharges == AzatState.MaxCharges) &&
              assertTrue(left == 100L)
    },

    test("ActivateCube из найденного: 20 дублонов + 10000 серебра → активен, 50 зарядов") {
      for {
        dao      <- TestHeroDao.withHero(userId, hero(gold = 10000L, doubloons = 20L))
        _        <- dao.writeAzatData(userId, AzatState(cube = CubeStatus.FoundInactive).asJson)
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = HallAzatState(dao, content)
        _        <- state.action(testUser, tap("ActivateCube"), renderer)
        azat     <- readAzat(dao)
        goldLeft <- dao.getHeroByUserId(userId).map(_.get.gold)
      } yield assertTrue(azat.cube == CubeStatus.Active) &&
              assertTrue(azat.cubeCharges == AzatState.MaxCharges) &&
              assertTrue(goldLeft == 0L)
    },

    test("RechargeFull: +50 зарядов до максимума, серебро списано") {
      for {
        dao      <- TestHeroDao.withHero(userId, hero(gold = 20000L))
        _        <- dao.writeAzatData(userId, AzatState(cube = CubeStatus.Active, cubeCharges = 10).asJson)
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = HallAzatState(dao, content)
        _        <- state.action(testUser, tap("RechargeFull"), renderer)
        azat     <- readAzat(dao)
        goldLeft <- dao.getHeroByUserId(userId).map(_.get.gold)
      } yield assertTrue(azat.cubeCharges == AzatState.MaxCharges) &&
              assertTrue(goldLeft == 10000L)
    }
  )
}
