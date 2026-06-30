package pangea.service.state.states.events.treasure

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.LootState.LootData
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._

object TreasureSchronStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def setup(chain: TreasureMobsChain) =
    for {
      renderer <- TestRenderer.make
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(dungeonLevel = 20))
      _        <- heroDao.writeSceneData(userId, chain.asJson)
      content  <- ZIO.attempt(SceneContent.load())
      state     = TreasureSchronState(heroDao, content)
    } yield (state, renderer, heroDao)

  override def spec = suite("TreasureSchronState")(

    test("autoAdvance ведёт в Loot") {
      for {
        t <- setup(TreasureMobsChain(Race.Orc.entryName, remaining = 0, 2, 3))
        (state, _, _) = t
      } yield assertTrue(state.autoAdvance.contains(StateType.Loot))
    },

    test("enter → показывает «схрон», кладёт непустую добычу в scene_data (дублоны 2-3 при золоте)") {
      for {
        t <- setup(TreasureMobsChain(Race.Orc.entryName, remaining = 0, 2, 3))
        (state, renderer, heroDao) = t
        _       <- TestRandom.feedLongs(123L)
        _       <- state.enter(testUser, renderer)
        screens <- renderer.sentScreens
        loot    <- heroDao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
      } yield assertTrue(screens.exists(_.text.contains("схрон"))) &&
              assertTrue(loot.isDefined) &&
              assertTrue(loot.exists(l => l.items.nonEmpty || l.golds.nonEmpty)) &&
              assertTrue(loot.exists(l => l.doubloons == 0L || (l.doubloons >= 2L && l.doubloons <= 3L)))
    }
  )
}
