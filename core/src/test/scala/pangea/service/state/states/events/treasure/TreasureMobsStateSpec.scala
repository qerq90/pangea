package pangea.service.state.states.events.treasure

import pangea.engine.SceneContent
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._

object TreasureMobsStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def makeState =
    for {
      renderer <- TestRenderer.make
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
      content  <- ZIO.attempt(SceneContent.load())
      state     = TreasureMobsState(heroDao, content)
    } yield (state, renderer, heroDao)

  override def spec = suite("TreasureMobsState")(

    test("enter → пишет цепочку (2-3 боя, диапазон дублонов) в scene_data и показывает Атаковать/Уйти") {
      for {
        t <- makeState
        (state, renderer, heroDao) = t
        _        <- state.enter(testUser, renderer)
        screens  <- renderer.sentScreens
        raw      <- heroDao.readSceneData(userId)
        chain     = raw.flatMap(_.as[TreasureMobsChain].toOption)
        validRaces = Race.values.map(_.entryName).toSet
      } yield assertTrue(chain.isDefined) &&
              assertTrue(chain.exists(c => c.remaining == 2 || c.remaining == 3)) &&
              assertTrue(chain.exists(c => validRaces.contains(c.race))) &&
              assertTrue(chain.exists(c => c.doubloonMin == 2 && c.doubloonMax == 3)) &&
              assertTrue(screens.head.choices.map(_.id).toSet == Set("AttackMobs", "LeaveMobs"))
    },

    test("Атаковать → переход в TreasureMobsFight (цепочка уже в scene_data)") {
      for {
        t <- makeState
        (state, renderer, _) = t
        _      <- state.enter(testUser, renderer)
        result <- state.action(testUser, tap("AttackMobs"), renderer)
      } yield assertTrue(result == StateType.TreasureMobsFight)
    },

    test("Уйти → scene_data очищается, переход в Dungeon") {
      for {
        t <- makeState
        (state, renderer, heroDao) = t
        _      <- state.enter(testUser, renderer)
        result <- state.action(testUser, tap("LeaveMobs"), renderer)
        raw    <- heroDao.readSceneData(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(raw.flatMap(_.as[TreasureMobsChain].toOption).isEmpty)
    }
  )
}
