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
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
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

    test("enter (есть ещё бои) → спавнит моба той же расы, routing возвращает снова в Fight, remaining-1") {
      for {
        t <- setup(TreasureMobsChain(Race.Orc.entryName, remaining = 3, 2, 3))
        (state, renderer, heroDao) = t
        _       <- TestRandom.feedLongs(1L) // seed моба
        _       <- state.enter(testUser, renderer)
        battle  <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
        routing <- heroDao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        nextChain = routing.flatMap(_.eventData).flatMap(_.as[TreasureMobsChain].toOption)
      } yield assertTrue(battle.exists(_.monsterRace == Race.Orc.entryName)) &&
              assertTrue(routing.exists(_.returnState.contains(StateType.TreasureMobsFight))) &&
              assertTrue(nextChain.exists(_.remaining == 2)) &&
              assertTrue(nextChain.exists(_.race == Race.Orc.entryName))
    },

    test("enter (последний бой) → routing ведёт в TreasureSchron, remaining=0") {
      for {
        t <- setup(TreasureMobsChain(Race.Elf.entryName, remaining = 1, 2, 3))
        (state, renderer, heroDao) = t
        _       <- TestRandom.feedLongs(7L)
        _       <- state.enter(testUser, renderer)
        routing <- heroDao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
        nextChain = routing.flatMap(_.eventData).flatMap(_.as[TreasureMobsChain].toOption)
      } yield assertTrue(routing.exists(_.returnState.contains(StateType.TreasureSchron))) &&
              assertTrue(nextChain.exists(_.remaining == 0))
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
        battle    = BattleState(heroDao, content)
        result   <- battle.action(testUser, tap("Attack"), renderer)
        loot     <- heroDao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
      } yield assertTrue(result == StateType.Loot) &&
              assertTrue(loot.exists(_.returnState.contains(StateType.TreasureSchron))) &&
              assertTrue(loot.flatMap(_.eventData).flatMap(_.as[TreasureMobsChain].toOption).exists(_.remaining == 0))
    }
  )
}
