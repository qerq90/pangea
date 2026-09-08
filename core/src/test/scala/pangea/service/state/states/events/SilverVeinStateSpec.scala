package pangea.service.state.states.events

import io.circe.Json
import pangea.engine.SceneContent
import pangea.model.item.GemKind
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.LootState.LootData
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer, TestScheduler}
import zio.ZIO
import zio.test._
import zio.test.TestRandom

object SilverVeinStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def makeState =
    for {
      heroDao   <- TestHeroDao.withHero(userId, TestFixtures.hero(userId).copy(dungeonLevel = 10))
      scheduler <- TestScheduler.make
      renderer  <- TestRenderer.make
      content   <- ZIO.attempt(SceneContent.load())
      state      = SilverVeinState(heroDao, scheduler, content)
    } yield (state, heroDao, renderer)

  override def spec = suite("SilverVeinState")(

    test("DroppableGemKinds не содержит Череп (Надколотый череп не должен выпадать из жилы)") {
      assertTrue(!SilverVeinState.DroppableGemKinds.contains(GemKind.Skull)) &&
      assertTrue(SilverVeinState.DroppableGemKinds.size == GemKind.values.size - 1)
    },

    test("Harvest с выпадением камня → камень никогда не Череп, на любом ролле выбора вида") {
      for {
        t <- makeState
        (state, heroDao, renderer) = t
        results <- ZIO.foreach(SilverVeinState.DroppableGemKinds.indices.toList) { idx =>
          for {
            _      <- TestRandom.feedInts(0, 0, idx) // delta, gemRoll(<=20 → дроп), kindIdx=idx
            _      <- TestRandom.feedBooleans(true)  // sign
            result <- state.action(testUser, tap("Harvest"), renderer)
            _      <- ZIO.fail(new Throwable("expected Loot")).unless(result == StateType.Loot)
            raw    <- heroDao.readSceneData(userId)
            loot   <- ZIO.fromOption(raw.flatMap(_.as[LootData].toOption))
                        .orElseFail(new Throwable("no loot data"))
            _      <- heroDao.writeSceneData(userId, Json.Null) // сброс перед следующей итерацией
          } yield loot.items.headOption.flatMap(_.gem).map(_.kind)
        }
      } yield assertTrue(results.forall(_.isDefined)) &&
              assertTrue(!results.flatten.contains(GemKind.Skull))
    }
  )
}
