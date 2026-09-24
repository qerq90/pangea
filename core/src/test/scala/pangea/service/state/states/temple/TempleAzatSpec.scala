package pangea.service.state.states.temple

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.hero.{AzatState, CubeStatus}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

object TempleAzatSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def hero(silver: Long = 0L, doubloons: Long = 0L) =
    TestFixtures.hero(userId).copy(silver = silver, doubloons = doubloons)

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
        state     = TempleAzatState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content)
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
        state     = TempleAzatState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content)
        _        <- state.action(testUser, tap("Donate"), renderer)
        azat     <- readAzat(dao)
        left     <- doubloonsOf(dao)
      } yield assertTrue(azat.blessingUntil.isEmpty) && assertTrue(left == 10L)
    },

    test("ApproachCube без куба → кнопка «Купить куб» с подставленной ценой, не сырым {price}") {
      for {
        dao      <- TestHeroDao.withHero(userId, hero())
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = HallAzatState(dao, content)
        _        <- state.action(testUser, tap("ApproachCube"), renderer)
        screens  <- renderer.sentScreens
        buyBtn    = screens.last.choices.find(_.id == "BuyCube")
      } yield assertTrue(buyBtn.exists(_.label == "Купить куб за 200 дублонов.")) &&
              assertTrue(buyBtn.exists(b => !b.label.contains("{")))
    },

    test("BuyCube: 200 дублонов → активный куб с полными зарядами, дублоны списаны") {
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

    test("ActivateCube из найденного: 20 дублонов + 10000 серебра → активен и полон зарядов") {
      for {
        dao      <- TestHeroDao.withHero(userId, hero(silver = 10000L, doubloons = 20L))
        _        <- dao.writeAzatData(userId, AzatState(cube = CubeStatus.FoundInactive).asJson)
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = HallAzatState(dao, content)
        _        <- state.action(testUser, tap("ActivateCube"), renderer)
        azat     <- readAzat(dao)
        silverLeft <- dao.getHeroByUserId(userId).map(_.get.silver)
      } yield assertTrue(azat.cube == CubeStatus.Active) &&
              assertTrue(azat.cubeCharges == AzatState.MaxCharges) &&
              assertTrue(silverLeft == 0L)
    },

    test("Жрец заряжает Ларец Азата за 5000 серебра") {
      import pangea.model.artifact.{ArtifactKind, HeroArtifacts}
      val artifacts = pangea.test.TestArtifactRepository.of(
        casket = pangea.test.TestArtifactRepository.artifact(ArtifactKind.Casket, tier = 2, charges = 0))
      for {
        dao      <- TestHeroDao.withHero(userId, hero(silver = 7000L))
        _        <- dao.writeAzatData(userId, AzatState(cube = CubeStatus.Active, cubeCharges = 10).asJson)
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = HallAzatState(dao, content, None, Some(artifacts))
        _        <- state.action(testUser, tap("Recharge"), renderer)
        screen   <- renderer.sentScreens.map(_.last)
        _        <- state.action(testUser, UserAction("", Some("""{"action":"RechargeArtifact","kind":"Casket"}""")), renderer)
        silver   <- dao.getHeroByUserId(userId).map(_.get.silver)
      } yield assertTrue(screen.choices.map(_.id).contains("RechargeArtifact")) &&
              assertTrue(artifacts.snapshot.of(ArtifactKind.Casket).charges == HeroArtifacts.MaxCharges) &&
              assertTrue(silver == 2000L)
    },

    test("RechargeFull: +50 зарядов к десяти, серебро списано") {
      for {
        dao      <- TestHeroDao.withHero(userId, hero(silver = 20000L))
        _        <- dao.writeAzatData(userId, AzatState(cube = CubeStatus.Active, cubeCharges = 10).asJson)
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = HallAzatState(dao, content)
        _        <- state.action(testUser, tap("RechargeFull"), renderer)
        azat     <- readAzat(dao)
        silverLeft <- dao.getHeroByUserId(userId).map(_.get.silver)
      } yield assertTrue(azat.cubeCharges == 60 && AzatState.MaxCharges == 100) &&
              assertTrue(silverLeft == 10000L)
    }
  )
}
