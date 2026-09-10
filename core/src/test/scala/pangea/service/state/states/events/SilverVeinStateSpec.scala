package pangea.service.state.states.events

import io.circe.Json
import pangea.engine.SceneContent
import pangea.model.item.{GemKind, MaterialKind}
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

    test("DroppableDusts — та же порода без черепа: чёрного порошка в жиле нет") {
      assertTrue(!SilverVeinState.DroppableDusts.contains(MaterialKind.BlackPowder)) &&
      assertTrue(SilverVeinState.DroppableDusts.size == MaterialKind.dusts.size - 1) &&
      assertTrue(SilverVeinState.DroppableDusts.contains(MaterialKind.RubyDust))
    },

    test("шанс пыли — те же 20%, что и у камня, и бросок отдельный") {
      assertTrue(SilverVeinState.DustDropChancePct == 20) &&
      assertTrue(SilverVeinState.GemDropChancePct == 20)
    },

    test("Harvest: пыль прокнула, камень нет → в добыче серебро и одна горсть пыли") {
      for {
        t <- makeState
        (state, heroDao, renderer) = t
        // delta, gemRoll(99 > 20 → камня нет), dustRoll(1 ≤ 20 → пыль), dustIdx
        _      <- TestRandom.feedInts(0, 99, 1, 0)
        _      <- TestRandom.feedBooleans(true)
        result <- state.action(testUser, tap("Harvest"), renderer)
        raw    <- heroDao.readSceneData(userId)
        loot   <- ZIO.fromOption(raw.flatMap(_.as[LootData].toOption))
                    .orElseFail(new Throwable("no loot data"))
      } yield assertTrue(result == StateType.Loot) &&
              assertTrue(loot.items.size == 1) &&
              assertTrue(loot.items.head.material.contains(SilverVeinState.DroppableDusts.head)) &&
              assertTrue(loot.silvers.size == 1)
    },

    test("Harvest: прокнули оба — в добыче и камень, и пыль") {
      for {
        t <- makeState
        (state, heroDao, renderer) = t
        // delta, gemRoll(1 → камень), kindIdx, dustRoll(1 → пыль), dustIdx
        _      <- TestRandom.feedInts(0, 1, 0, 1, 1)
        _      <- TestRandom.feedBooleans(true)
        result <- state.action(testUser, tap("Harvest"), renderer)
        raw    <- heroDao.readSceneData(userId)
        loot   <- ZIO.fromOption(raw.flatMap(_.as[LootData].toOption))
                    .orElseFail(new Throwable("no loot data"))
      } yield assertTrue(result == StateType.Loot) &&
              assertTrue(loot.items.size == 2) &&
              assertTrue(loot.items.exists(_.gem.isDefined)) &&
              assertTrue(loot.items.exists(_.material.exists(_.gem.isDefined)))
    },

    test("Harvest: не прокнуло ничего → серебро сразу, без экрана добычи") {
      for {
        t <- makeState
        (state, heroDao, renderer) = t
        before <- heroDao.getHeroByUserId(userId).map(_.get.silver)
        // delta, gemRoll(99), dustRoll(99) — обе находки мимо
        _      <- TestRandom.feedInts(0, 99, 99)
        _      <- TestRandom.feedBooleans(true)
        result <- state.action(testUser, tap("Harvest"), renderer)
        after  <- heroDao.getHeroByUserId(userId).map(_.get.silver)
        scene  <- heroDao.readSceneData(userId)
      } yield assertTrue(result == StateType.Dungeon) &&
              assertTrue(after > before) &&
              assertTrue(scene.contains(Json.Null))
    },

    test("пыль из жилы — всегда одна горсть, на любом ролле вида") {
      for {
        t <- makeState
        (state, heroDao, renderer) = t
        kinds <- ZIO.foreach(SilverVeinState.DroppableDusts.indices.toList) { idx =>
          for {
            _      <- TestRandom.feedInts(0, 99, 1, idx)
            _      <- TestRandom.feedBooleans(true)
            _      <- state.action(testUser, tap("Harvest"), renderer)
            raw    <- heroDao.readSceneData(userId)
            loot   <- ZIO.fromOption(raw.flatMap(_.as[LootData].toOption))
                        .orElseFail(new Throwable("no loot data"))
            _      <- heroDao.writeSceneData(userId, Json.Null)
          } yield loot.items.flatMap(_.material)
        }
      } yield assertTrue(kinds.forall(_.size == 1)) &&
              assertTrue(!kinds.flatten.contains(MaterialKind.BlackPowder)) &&
              assertTrue(kinds.flatten.toSet == SilverVeinState.DroppableDusts.toSet)
    },

    test("Harvest с выпадением камня → камень никогда не Череп, на любом ролле выбора вида") {
      for {
        t <- makeState
        (state, heroDao, renderer) = t
        results <- ZIO.foreach(SilverVeinState.DroppableGemKinds.indices.toList) { idx =>
          for {
            // delta, gemRoll(≤20 → дроп), kindIdx=idx, dustRoll(99 → пыли нет)
            _      <- TestRandom.feedInts(0, 0, idx, 99)
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
