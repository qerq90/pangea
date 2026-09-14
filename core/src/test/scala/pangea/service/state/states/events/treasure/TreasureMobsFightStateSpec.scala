package pangea.service.state.states.events.treasure

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.monster.{Race, Rarity}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.battle.BattleState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test._

object TreasureMobsFightStateSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def setup(chain: TreasureMobsChain) =
    for {
      renderer <- TestRenderer.make
      heroDao  <- TestHeroDao.withHero(userId, TestFixtures.hero(userId))
      _        <- heroDao.writeSceneData(userId, chain.asJson)
      content  <- ZIO.attempt(SceneContent.load())
      state     = TreasureMobsFightState(heroDao, content)
    } yield (state, renderer, heroDao)

  // герой, который гарантированно бьёт и не получает урона
  private def strongHero = TestFixtures.hero(userId).copy(
    fightStats = FightStats(atk = 50, hp = 200, armor = 0, defence = 0,
                            evasion = 9999, accuracy = 9999, energy = 0))

  private val weakBattle = SoloPveBattle(
    monsterLvl = 1L, monsterRace = Race.Human.entryName, monsterRarity = Rarity.Common.entryName,
    monsterStats = FightStats(atk = 1, hp = 1, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0),
    monsterCurrentHp = 1L, monsterCurrentArmor = 0L)

  override def spec = suite("TreasureMobsFightState")(

    test("autoAdvance ведёт в Battle") {
      for {
        t <- setup(TreasureMobsChain("Orc", remaining = 3, 2, 3))
        (state, _, _) = t
      } yield assertTrue(state.autoAdvance.contains(StateType.Battle))
    },

    test("enter → один групповой бой на всех мобов той же расы, routing сразу в схрон") {
      for {
        t <- setup(TreasureMobsChain(Race.Orc.entryName, remaining = 3, 2, 3))
        (state, renderer, heroDao) = t
        _       <- state.enter(testUser, renderer)
        battle  <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        routing <- heroDao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        nextChain = routing.flatMap(_.eventData).flatMap(_.as[TreasureMobsChain].toOption)
      } yield assertTrue(battle.exists(_.monsterRace == Race.Orc.entryName)) &&
              assertTrue(battle.exists(_.group.others.size == 2)) &&
              assertTrue(battle.exists(_.group.others.forall(_.race == Race.Orc.entryName))) &&
              assertTrue(battle.exists(_.group.originRace.contains(Race.Orc.entryName))) &&
              assertTrue(routing.exists(_.returnState.contains(StateType.TreasureSchron))) &&
              assertTrue(nextChain.exists(_.remaining == 0)) &&
              assertTrue(nextChain.exists(_.race == Race.Orc.entryName))
    },

    test("enter с двумя мобами → в строю ровно двое") {
      for {
        t <- setup(TreasureMobsChain(Race.Elf.entryName, remaining = 2, 2, 3))
        (state, renderer, heroDao) = t
        _       <- state.enter(testUser, renderer)
        battle  <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      } yield assertTrue(battle.exists(_.group.others.size == 1)) &&
              assertTrue(battle.exists(_.isGroup))
    },

    test("интеграция: победа в бою цепочки переносит routing из scene_data в добычу") {
      val chain   = TreasureMobsChain(Race.Orc.entryName, remaining = 0, 2, 3)
      val routing = LootData(Nil, Nil, returnState = Some(StateType.TreasureSchron), eventData = Some(chain.asJson))
      for {
        heroDao  <- TestHeroDao.withHero(userId, strongHero)
        _        <- heroDao.writeActiveBattle(userId, weakBattle.asJson)
        _        <- heroDao.writeSceneData(userId, routing.asJson)
        renderer <- TestRenderer.make
        content  <- ZIO.attempt(SceneContent.load())
        battle    = BattleState(heroDao, TestInventoryRepository.accepting, TestItemRepository.make, content)
        result   <- battle.action(testUser, tap("Attack"), renderer)
        loot     <- heroDao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
      } yield assertTrue(result == StateType.Loot) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.TreasureSchron))) &&
              assertTrue(loot.flatMap(_.eventData).flatMap(_.as[TreasureMobsChain].toOption).exists(_.remaining == 0))
    }
  )
}
