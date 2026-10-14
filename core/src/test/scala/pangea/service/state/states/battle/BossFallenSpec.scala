package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.generator.item.MaterialGenerator
import pangea.model.battle.{BattleEffects, GroupState, MonsterSlot, SlainMonster, SoloPveBattle}
import pangea.model.cave.{CaveRoom, CaveScene, RoomKind}
import pangea.model.hero.{Hero, LoreData}
import pangea.model.item.{ItemType, MaterialKind}
import pangea.model.monster.{MiniBoss, Race, Rarity}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.LootState.LootData
import pangea.service.state.states.events.cave.MonsterCaveState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer, TestScheduler}
import zio.{Task, ZIO}
import zio.test._

/** Минибосс, павший не последним, — всё равно минибосс: Некроманта со свитой бьют
  * первым, а Крысиный король падает раньше своих крыс. И первая встреча с ними —
  * в логове или из разлома алтаря — звучит репликой и открывает Трактирщика. */
object BossFallenSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def content = ZIO.attempt(SceneContent.load())

  /** Герой высокого уровня: опыт за босса не перевалит его на новый уровень. */
  private def hero: Hero = TestFixtures.hero(userId, dungeonLevel = 30).copy(
    lvl = 40L, exp = 0L,
    fightStats = FightStats(atk = 1000, hp = 100000, armor = 0, defence = 0,
                            evasion = 9999, accuracy = 9999, energy = 0))

  private val necroLvl = 3L

  /** Некромант уже пал, в паре — последний мертвец его свиты с одной единицей HP. */
  private def lastGuard: SoloPveBattle = {
    val guard = SoloPveBattle(
      monsterLvl          = 10L,
      monsterRace         = Race.Goblin.entryName,
      monsterRarity       = Rarity.Common.entryName,
      monsterStats        = FightStats(atk = 1, hp = 1, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0),
      monsterCurrentHp    = 1L,
      monsterCurrentArmor = 0L,
      monsterUndead       = true)
    guard.copy(group = GroupState.empty.copy(slain = List(
      SlainMonster(necroLvl, Race.Undead.entryName, Rarity.Legendary.entryName, marked = false,
        name = "Некромант", boss = Some(MiniBoss.Necromancer.entryName)))))
  }

  /** Добить и дождаться конца боя: уклонение не бывает ниже 5%. */
  private def finish(state: BattleState, r: TestRenderer): Task[StateType] = {
    def attack(left: Int): Task[StateType] =
      state.action(testUser, tap("Attack"), r).flatMap { st =>
        if (st == StateType.Battle && left > 1) attack(left - 1) else ZIO.succeed(st)
      }
    attack(10)
  }

  private def cave(items: List[pangea.model.item.Item] = Nil) =
    for {
      dao   <- TestHeroDao.withHero(userId, hero)
      inv    = TestInventoryRepository.withItems(items)
      sched <- TestScheduler.make
      r     <- TestRenderer.make
      c     <- content
    } yield (MonsterCaveState(dao, inv, TestItemRepository.make, sched, c, None), dao, r)

  private def loreOf(dao: TestHeroDao): Task[LoreData] =
    dao.readLoreData(userId).map(_.flatMap(_.as[LoreData].toOption).getOrElse(LoreData.empty))

  private def texts(r: TestRenderer): Task[String] = r.sentScreens.map(_.map(_.text).mkString("\n"))

  override def spec = suite("Минибосс среди павших")(

    test("павший помнит, что был минибоссом, — и в паре, и в строю, и в записи") {
      val slot = MonsterSlot(5L, Race.Animal.entryName, Rarity.Legendary.entryName,
        FightStats(atk = 1, hp = 1, armor = 0, defence = 0, evasion = 0, accuracy = 1, energy = 0),
        0L, 0L, marked = false, 0L, BattleEffects.empty, boss = Some(MiniBoss.RatKing.entryName))
      val record = SlainMonster(3L, Race.Undead.entryName, Rarity.Legendary.entryName, marked = false,
        name = "Некромант", boss = Some(MiniBoss.Necromancer.entryName))
      // старая запись без метки читается как рядовой павший
      val old = io.circe.jawn.parse("""{"lvl":3,"race":"Orc","rarity":"Common","marked":false,"name":"Орк"}""")
        .flatMap(_.as[SlainMonster])
      assertTrue(slot.slain.boss.contains(MiniBoss.RatKing.entryName)) &&
      assertTrue(lastGuard.copy(bossKind = Some("Necromancer")).slainActive.boss.contains("Necromancer")) &&
      assertTrue(record.asJson.as[SlainMonster] == Right(record)) &&
      assertTrue(old.exists(_.boss.isEmpty))
    },

    test("Некромант, павший первым, платит как минибосс, когда добита его свита") {
      for {
        dao <- TestHeroDao.withHero(userId, hero)
        _   <- dao.writeActiveBattle(userId, lastGuard.asJson)
        r   <- TestRenderer.make
        c   <- content
        state = BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, c)
        out  <- finish(state, r)
        h    <- dao.getHeroByUserId(userId).map(_.get)
        loot <- dao.readSceneData(userId).map(_.flatMap(_.as[LootData].toOption))
      } yield assertTrue(out == StateType.Loot) &&
              // опыт — как за Некроманта его уровня, а не как за двух мертвецов
              assertTrue(h.exp == MiniBoss.Necromancer.expReward(necroLvl)) &&
              // первым в добыче — он, и роняет он своё: кости, вещь набора или
              // череп с дублонами — не трофей рядового мертвеца
              assertTrue(loot.exists(_.monsterName.contains("Некромант"))) &&
              assertTrue(loot.exists(l => l.items.forall(_.itemType != ItemType.Trophy))) &&
              assertTrue(loot.exists(l => l.items.nonEmpty || l.doubloons > 0L)) &&
              // мертвец свиты роняет своё следом
              assertTrue(loot.exists(_.queue.size == 1))
    },

    test("вызванный на алтаре Некромант при первой встрече звучит репликой и открывает Трактирщика") {
      val scene = CaveScene(Race.Goblin.entryName, List(
        CaveRoom(0, 0, 0, RoomKind.Empty, done = true),
        CaveRoom(1, 0, 0, RoomKind.Altar)), at = 1, inside = true)
      def summon(lore: LoreData) =
        for {
          t <- cave(List(MaterialGenerator.item(MaterialKind.CursedBones).copy(id = 51L)))
          (state, dao, r) = t
          _    <- dao.writeLoreData(userId, lore.asJson)
          _    <- dao.writeSceneData(userId, scene.asJson)
          _    <- state.action(testUser, tap("CaveAltar"), r)
          out  <- state.action(testUser, tap("CaveUse_51"), r)
          said <- texts(r)
          met  <- loreOf(dao)
        } yield (out, said, met)
      for {
        first <- summon(LoreData.empty)
        again <- summon(LoreData(metNecro = true))
      } yield assertTrue(first._1 == StateType.Battle) &&
              assertTrue(first._2.contains("Надо будет спросить о нём Трактирщика") && first._3.metNecro) &&
              assertTrue(again._1 == StateType.Battle && !again._2.contains("Надо будет спросить"))
    },

    test("Крысиный король в логове при первой встрече тоже отсылает к Трактирщику") {
      val sewer = CaveScene(Race.Animal.entryName, List(
        CaveRoom(0, 0, 0, RoomKind.Empty, done = true),
        CaveRoom(0, 1, 1, RoomKind.Lair)), inside = true, sewer = true, questLvl = 5L, floor = 2)
      for {
        t <- cave()
        (state, dao, r) = t
        _    <- dao.writeSceneData(userId, sewer.asJson)
        out  <- state.action(testUser, tap("CaveForward"), r)
        said <- texts(r)
        met  <- loreOf(dao)
      } yield assertTrue(out == StateType.Battle) &&
              assertTrue(said.contains("Надо будет спросить о таком Трактирщика") && met.metRatKing)
    }
  )
}
