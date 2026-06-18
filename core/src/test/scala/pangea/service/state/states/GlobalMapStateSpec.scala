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
  private val fullHp      = baseHero.baseStats.vit * 16L  // 160
  private val richHero    = baseHero.copy(gold = 500L, fightStats = baseHero.fightStats.copy(hp = fullHp))
  private val poorHero    = baseHero.copy(gold = 0L,   fightStats = baseHero.fightStats.copy(hp = 10L))
  private val damagedHero = baseHero.copy(gold = 500L, fightStats = baseHero.fightStats.copy(hp = 10L))
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

    test("Tavern → показывает экран таверны с ценой и кнопкой Heal") {
      for {
        triple              <- makeState(richHero)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("Tavern"), renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.GlobalMap) &&
              assertTrue(screens.exists(_.choices.map(_.id).contains("Heal")))
    },

    test("Heal при достаточном gold → восстанавливает HP, снимает травму, списывает gold") {
      for {
        triple               <- makeState(damagedHero)
        (state, heroDao, renderer) = triple
        result               <- state.action(testUser, tap("Heal"), renderer)
        updated              <- heroDao.getHeroByUserId(userId)
        screens              <- renderer.sentScreens
      } yield assertTrue(result == StateType.GlobalMap) &&
              assertTrue(updated.exists(_.fightStats.hp > 10L)) &&
              assertTrue(updated.exists(_.gold < damagedHero.gold)) &&
              assertTrue(screens.exists(_.text.contains("исцелились")))
    },

    test("Heal снимает травму") {
      for {
        triple               <- makeState(traumaHero)
        (state, heroDao, renderer) = triple
        _                    <- state.action(testUser, tap("Heal"), renderer)
        updated              <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(updated.exists(_.traumaUntil.isEmpty))
    },

    test("Heal при недостаточном gold → показывает ошибку, gold не меняется") {
      for {
        triple               <- makeState(poorHero)
        (state, heroDao, renderer) = triple
        result               <- state.action(testUser, tap("Heal"), renderer)
        updated              <- heroDao.getHeroByUserId(userId)
        screens              <- renderer.sentScreens
      } yield assertTrue(result == StateType.GlobalMap) &&
              assertTrue(updated.exists(_.gold == 0L)) &&
              assertTrue(screens.exists(_.text.contains("Недостаточно")))
    },

    test("Heal при полном HP и без травмы → показывает «и так здоровы»") {
      for {
        triple              <- makeState(richHero)
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("Heal"), renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.GlobalMap) &&
              assertTrue(screens.exists(_.text.contains("здоровы")))
    },

    test("Heal восстанавливает armor до maxArmor") {
      val depletedArmor = damagedHero.copy(
        fightStats = damagedHero.fightStats.copy(armor = 0L)
      )
      for {
        triple               <- makeState(depletedArmor)
        (state, heroDao, renderer) = triple
        hero                 <- heroDao.getHeroByUserId(userId)
        _                    <- state.action(testUser, tap("Heal"), renderer)
        updated              <- heroDao.getHeroByUserId(userId)
      } yield assertTrue(updated.exists(_.fightStats.armor == updated.get.maxArmor))
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
