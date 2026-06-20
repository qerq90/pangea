package pangea.service.state.states.hero

import pangea.engine.SceneContent
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.test._
import zio.ZIO

object HeroStatsStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val heroWithPoints = TestFixtures.hero(userId).copy(upgradePoints = 3L)

  override def spec = suite("HeroStatsState")(

    test("Back → переходит в Dungeon") {
      for {
        renderer <- TestRenderer.make
        heroDao  <- TestHeroDao.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = HeroStatsState(heroDao, content)
        result   <- state.action(testUser, tap("Back"), renderer)
      } yield assertTrue(result == StateType.Dungeon)
    },

    test("неизвестный ввод → перерисовывает статы, остаётся в HeroStats") {
      for {
        renderer <- TestRenderer.make
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        content  <- ZIO.attempt(SceneContent.load())
        state     = HeroStatsState(heroDao, content)
        result   <- state.action(testUser, UserAction("что угодно", None), renderer)
        screens  <- renderer.sentScreens
      } yield assertTrue(result == StateType.HeroStats) &&
              assertTrue(screens.nonEmpty)
    },

    test("enter → показывает статы героя с уровнем и INT") {
      for {
        renderer <- TestRenderer.make
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        content  <- ZIO.attempt(SceneContent.load())
        state     = HeroStatsState(heroDao, content)
        _        <- state.enter(testUser, renderer)
        screens  <- renderer.sentScreens
      } yield assertTrue(screens.nonEmpty) &&
              assertTrue(screens.head.text.contains("Уровень")) &&
              assertTrue(screens.head.text.contains("ИНТ"))
    },

    test("enter с травмой → показывает текущее/максимум подебаффленного стата (СИЛ 5/10)") {
      // «Сломанная рука» — -50% к силе; базовая СИЛ 10 → 5/10
      val injured = TestFixtures.hero(userId).copy(
        traumaUntil = Some(Long.MaxValue),
        traumaNames = List("Сломанная рука")
      )
      for {
        renderer <- TestRenderer.make
        heroDao  <- TestHeroDao.withHero(userId, injured)
        content  <- ZIO.attempt(SceneContent.load())
        state     = HeroStatsState(heroDao, content)
        _        <- state.enter(testUser, renderer)
        screens  <- renderer.sentScreens
      } yield assertTrue(screens.head.text.contains("СИЛ 5/10"))
    },

    test("статы зажаты снизу единицей (атака/точность/защита/уклонение, HP)") {
      // у фикстуры defence = 0 → после зажима показывается 1; ни один стат не ниже 1
      val eff = TestFixtures.hero(userId).effectiveFightStats(0L)
      assertTrue(
        eff.atk >= 1L, eff.accuracy >= 1L, eff.defence >= 1L, eff.evasion >= 1L,
        eff.defence == 1L,                                   // был 0, стал 1
        TestFixtures.hero(userId).effectiveMaxHp(0L) >= 1L
      )
    },

    test("enter с upgrade points → показывает кнопку Upgrade") {
      for {
        renderer <- TestRenderer.make
        heroDao  <- TestHeroDao.withHero(userId, heroWithPoints)
        content  <- ZIO.attempt(SceneContent.load())
        state     = HeroStatsState(heroDao, content)
        _        <- state.enter(testUser, renderer)
        screens  <- renderer.sentScreens
      } yield assertTrue(screens.head.choices.map(_.id).contains("Upgrade"))
    },

    test("enter без upgrade points → НЕ показывает кнопку Upgrade") {
      for {
        renderer <- TestRenderer.make
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        content  <- ZIO.attempt(SceneContent.load())
        state     = HeroStatsState(heroDao, content)
        _        <- state.enter(testUser, renderer)
        screens  <- renderer.sentScreens
      } yield assertTrue(!screens.head.choices.map(_.id).contains("Upgrade"))
    },

    test("OpenInventory → переходит в Inventory") {
      for {
        renderer <- TestRenderer.make
        heroDao  <- TestHeroDao.make
        content  <- ZIO.attempt(SceneContent.load())
        state     = HeroStatsState(heroDao, content)
        result   <- state.action(testUser, tap("OpenInventory"), renderer)
      } yield assertTrue(result == StateType.Inventory)
    },

    test("Upgrade → показывает экран распределения с 4 кнопками статов") {
      for {
        renderer <- TestRenderer.make
        heroDao  <- TestHeroDao.withHero(userId, heroWithPoints)
        content  <- ZIO.attempt(SceneContent.load())
        state     = HeroStatsState(heroDao, content)
        result   <- state.action(testUser, tap("Upgrade"), renderer)
        screens  <- renderer.sentScreens
      } yield assertTrue(result == StateType.HeroStats) &&
              assertTrue(screens.exists(_.choices.map(_.id).contains("UpgradeStr"))) &&
              assertTrue(screens.exists(_.choices.map(_.id).contains("UpgradeVit"))) &&
              assertTrue(screens.exists(_.choices.map(_.id).contains("UpgradeAgi"))) &&
              assertTrue(screens.exists(_.choices.map(_.id).contains("UpgradeInt")))
    },

    test("UpgradeStr → увеличивает STR на 1, уменьшает upgradePoints на 1") {
      for {
        renderer <- TestRenderer.make
        heroDao  <- TestHeroDao.withHero(userId, heroWithPoints)
        content  <- ZIO.attempt(SceneContent.load())
        state     = HeroStatsState(heroDao, content)
        result   <- state.action(testUser, tap("UpgradeStr"), renderer)
        updated  <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(result == StateType.HeroStats) &&
              assertTrue(updated.exists(_.baseStats.str == heroWithPoints.baseStats.str + 1)) &&
              assertTrue(updated.exists(_.upgradePoints == heroWithPoints.upgradePoints - 1))
    },

    test("UpgradeVit → увеличивает VIT на 1") {
      for {
        renderer <- TestRenderer.make
        heroDao  <- TestHeroDao.withHero(userId, heroWithPoints)
        content  <- ZIO.attempt(SceneContent.load())
        state     = HeroStatsState(heroDao, content)
        _        <- state.action(testUser, tap("UpgradeVit"), renderer)
        updated  <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(updated.exists(_.baseStats.vit == heroWithPoints.baseStats.vit + 1))
    },

    test("UpgradeAgi → увеличивает AGI на 1") {
      for {
        renderer <- TestRenderer.make
        heroDao  <- TestHeroDao.withHero(userId, heroWithPoints)
        content  <- ZIO.attempt(SceneContent.load())
        state     = HeroStatsState(heroDao, content)
        _        <- state.action(testUser, tap("UpgradeAgi"), renderer)
        updated  <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(updated.exists(_.baseStats.agi == heroWithPoints.baseStats.agi + 1))
    },

    test("UpgradeInt → увеличивает INT на 1") {
      for {
        renderer <- TestRenderer.make
        heroDao  <- TestHeroDao.withHero(userId, heroWithPoints)
        content  <- ZIO.attempt(SceneContent.load())
        state     = HeroStatsState(heroDao, content)
        _        <- state.action(testUser, tap("UpgradeInt"), renderer)
        updated  <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(updated.exists(_.baseStats.int == heroWithPoints.baseStats.int + 1))
    },

    test("Upgrade при 0 points → показывает сообщение об ошибке") {
      for {
        renderer <- TestRenderer.make
        heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
        content  <- ZIO.attempt(SceneContent.load())
        state     = HeroStatsState(heroDao, content)
        result   <- state.action(testUser, tap("Upgrade"), renderer)
        screens  <- renderer.sentScreens
      } yield assertTrue(result == StateType.HeroStats) &&
              assertTrue(screens.exists(_.text.contains("Нет")))
    },

    test("BackToStats → возвращается к экрану статов") {
      for {
        renderer <- TestRenderer.make
        heroDao  <- TestHeroDao.withHero(userId, heroWithPoints)
        content  <- ZIO.attempt(SceneContent.load())
        state     = HeroStatsState(heroDao, content)
        result   <- state.action(testUser, tap("BackToStats"), renderer)
        screens  <- renderer.sentScreens
      } yield assertTrue(result == StateType.HeroStats) &&
              assertTrue(screens.exists(_.text.contains("Уровень")))
    }
  )
}
