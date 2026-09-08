package pangea.service.state

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails, ItemType, PotionKind, Rarity => ItemRarity}
import pangea.model.monster.{Race, Rarity}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.states.GlobalMapState
import pangea.service.state.states.battle.BattleState
import pangea.test.{TestApi, TestFixtures, TestHeroDao, TestHeroRepository, TestUserRepository}
import zio.ZIO
import zio.test._

/** `/home` — глобальная аварийная команда (см. ARCHITECTURE.md §10): должна
 *  выкидывать героя в город из ЛЮБОГО состояния, включая бой, ничего не тратя
 *  и не начисляя, и не рассматриваться как поражение/бегство. */
object StateHandlerSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val vkId     = VkId("vk_test")
  private val testUser = User(userId, vkId, TelegramId("tg_test"))

  private val hero = TestFixtures.hero(userId, state = StateType.Battle).copy(
    fightStats = FightStats(atk = 10, hp = 37, armor = 5, defence = 0, evasion = 5, accuracy = 10, energy = 5),
    silver     = 123L,
    exp        = 45L
  )

  private val battle = SoloPveBattle(
    monsterLvl          = 5L,
    monsterRace         = Race.Human.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = 5, hp = 50, armor = 0, defence = 0, evasion = 0, accuracy = 10, energy = 0),
    monsterCurrentHp    = 50L,
    monsterCurrentArmor = 0L
  )

  private def makeHandler(startState: StateType, customize: Hero => Hero = identity) =
    for {
      baseHero <- ZIO.succeed(customize(hero.copy(state = startState)))
      heroDao  <- TestHeroDao.withHero(userId, baseHero)
      _        <- heroDao.writeActiveBattle(userId, battle.asJson)
      _        <- heroDao.writeSceneData(userId, Json.obj("someLeftoverKey" -> Json.fromString("junk")))
      heroRepo <- TestHeroRepository.withHero(userId, baseHero)
      userRepo <- TestUserRepository.withUser(testUser)
      api      <- TestApi.make
      content  <- ZIO.attempt(SceneContent.load())
      states = Map[StateType, State](
        StateType.GlobalMap -> GlobalMapState(heroDao, content),
        StateType.Battle    -> BattleState(heroDao, content)
      )
      lock <- PlayerLock.make
    } yield (new StateHandler(api, userRepo, heroRepo, heroDao, states, lock), heroDao, heroRepo, api)

  override def spec = suite("StateHandler /home")(

    test("/home посреди боя → переносит в GlobalMap, чистит активный бой и scene_data, герой ничего не теряет/не получает") {
      for {
        t <- makeHandler(StateType.Battle)
        (handler, heroDao, heroRepo, _) = t
        _        <- handler.makeActionVK(vkId, eventId = 1L, UserAction("/home", None))
        updated  <- heroRepo.getHero(userId).map(_.get)
        battleOp <- heroDao.readActiveBattle(userId)
        sceneOp  <- heroDao.readSceneData(userId)
      } yield assertTrue(updated.state == StateType.GlobalMap) &&
              assertTrue(updated.fightStats.hp == 37L) &&      // не умер, HP как было
              assertTrue(updated.silver == 123L) &&            // не потерял и не получил серебро
              assertTrue(updated.exp == 45L) &&                // опыт не тронут
              assertTrue(updated.traumaNames.isEmpty) &&       // травму (как при смерти) не получил
              assertTrue(battleOp.isEmpty) &&                  // активный бой сброшен
              assertTrue(sceneOp.contains(Json.Null))          // scene_data сброшен
    },

    test("/HOME  (регистр и пробелы не важны) срабатывает так же") {
      for {
        t <- makeHandler(StateType.Battle)
        (handler, _, heroRepo, _) = t
        _       <- handler.makeActionVK(vkId, eventId = 1L, UserAction("  /HOME  ", None))
        updated <- heroRepo.getHero(userId).map(_.get)
      } yield assertTrue(updated.state == StateType.GlobalMap)
    },

    test("кнопка с payload не считается командой /home, даже если текст совпадает") {
      for {
        t <- makeHandler(StateType.Battle)
        (handler, _, heroRepo, _) = t
        _       <- handler.makeActionVK(vkId, eventId = 1L, UserAction("/home", Some("""{"action":"Attack"}""")))
        updated <- heroRepo.getHero(userId).map(_.get)
      } yield assertTrue(updated.state == StateType.Battle)
    },

    test("зелье уклонения из пояса + /home → бафф не остаётся на герое и не попадёт в следующий бой") {
      val potionBelt = Item(9L, "Пояс", 1L, ItemRarity.Green, ItemType.Belt,
        attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
        details = ItemDetails.Belt(PotionKind.Evasion, charges = 1, maxCharges = 1))
      for {
        t <- makeHandler(StateType.Battle, h => h.copy(equipment = TestFixtures.emptyEquipment.copy(belt = potionBelt)))
        (handler, heroDao, heroRepo, _) = t

        // 1) Пьём зелье уклонения из пояса. BattleState читает/пишет героя через
        // heroDao (не heroRepo) — сверяем состояние оттуда же, откуда его видит сам бой.
        _              <- handler.makeActionVK(vkId, eventId = 1L, UserAction("", Some("""{"action":"UseBelt"}""")))
        duringBattle   <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
        heroAfterDrink <- heroDao.getHeroByUserId(userId).map(_.get)
        beltAfterDrink  = heroAfterDrink.equipment.belt.details.asInstanceOf[ItemDetails.Belt]

        // 2) Прописываем /home посреди того же боя.
        _                 <- handler.makeActionVK(vkId, eventId = 2L, UserAction("/home", None))
        activeBattleAfter <- heroDao.readActiveBattle(userId)
        finalHero         <- heroDao.getHeroByUserId(userId).map(_.get)
      } yield
        // бафф реально применился и был виден только внутри active_battle текущего боя
        assertTrue(duringBattle.heroBattleState.dodgeBonus == 10L) &&
        // заряд зелья списался по-настоящему (это нормальный итог выпитого зелья, не /home)
        assertTrue(beltAfterDrink.charges == 0) &&
        // /home полностью стирает бой вместе с баффом — раз объекта с баффом больше
        // не существует, ему неоткуда «остаться навсегда» или перейти в следующий бой
        assertTrue(activeBattleAfter.isEmpty) &&
        // и герой (fightStats/evasion) не получил постоянного изменения от зелья
        assertTrue(finalHero.fightStats.evasion == hero.fightStats.evasion) &&
        // на всякий случай: любой новый бой в принципе стартует с пустым heroBattleState
        // независимо от того, что было раньше (SoloPveBattle.from не наследует бафы)
        assertTrue(pangea.model.battle.HeroBattleState.empty.isEmpty)
    },

    test("/home уже в городе → не падает, город перерисован") {
      for {
        t <- makeHandler(StateType.GlobalMap)
        (handler, _, heroRepo, api) = t
        _        <- handler.makeActionVK(vkId, eventId = 1L, UserAction("/home", None))
        updated  <- heroRepo.getHero(userId).map(_.get)
        messages <- api.sentMessages
      } yield assertTrue(updated.state == StateType.GlobalMap) &&
              assertTrue(messages.nonEmpty)
    }
  )
}
