package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Gem, GemKind, Item, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Race, Rarity}
import pangea.model.skill.{MonsterEnergy, MonsterSkill}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Энергия мобов: формулы, расовые умения и то, как её выжигает Молния. */
object MobEnergySpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def weapon(gem: Option[GemKind]): Item =
    Item(50L, "Меч", 1L, ItemRarity.Blue, ItemType.Weapon,
      attack = 10, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      sockets = List(gem.map(Gem(_, 1))))

  private def hero(gem: Option[GemKind] = None): Hero =
    TestFixtures.hero(userId).copy(
      lvl        = 10L,
      fightStats = FightStats(atk = 20, hp = 500000L, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = 5000),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = weapon(gem))
    )

  private val lvl = 10L

  private def mobBattle(
      race: Race = Race.Human,
      rarity: Rarity = Rarity.Common,
      energy: Long
  ): SoloPveBattle = SoloPveBattle(
    monsterLvl           = lvl,
    monsterRace          = race.entryName,
    monsterRarity        = rarity.entryName,
    monsterStats         = FightStats(atk = 20, hp = 100000L, armor = 0, defence = 0,
                                      evasion = 0, accuracy = 9999,
                                      energy = MonsterEnergy.maxEnergy(lvl)),
    monsterCurrentHp     = 100000L,
    monsterCurrentArmor  = 0L,
    monsterCurrentEnergy = energy
  )

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, content), dao, renderer)

  private def strike(h: Hero, b: SoloPveBattle, seed: ZIO[Any, Nothing, Unit]) =
    for {
      t <- makeState(h, b)
      (state, dao, r) = t
      _       <- seed
      _       <- state.action(testUser, tap("Attack"), r)
      after   <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      updated <- dao.getHeroByUserId(userId).map(_.get)
      screens <- r.sentScreens
    } yield (after, updated, screens.map(_.text).mkString("\n"))

  /** Броски раунда: удар героя, удар моба, затем прочее (проки, выбор умения). */
  private def seedTurn(extra: Int*) =
    TestRandom.feedInts(60 +: 90 +: extra: _*) *> TestRandom.feedLongs(100L, 100L)

  override def spec = suite("Энергия мобов")(

    // ── Формулы ───────────────────────────────────────────────────────────────
    test("цена, реген и потолок линейны по уровню, а вглубь умения идут чуть чаще") {
      // Реген растёт быстрее цены, поэтому «сколько раундов копить» с уровнем
      // немного падает — но остаётся в тех же полутора раундах, а не улетает.
      def rounds(l: Long) =
        MonsterSkill.QuickStrike.cost(l).toDouble / MonsterEnergy.regen(l, Rarity.Common).toDouble
      assertTrue(MonsterEnergy.baseCost(10L) == 50L) &&   // 30 + 2×10
      assertTrue(MonsterEnergy.maxEnergy(10L) == 150L) &&  // три обычных умения
      assertTrue(MonsterEnergy.regen(10L, Rarity.Common) == 28L) && // (15+20)×0.8
      assertTrue(rounds(150L) < rounds(1L)) &&
      // обычный моб копит ровно два раунда на первом уровне и около раунда на 150-м
      assertTrue(rounds(1L) <= 2.0) && assertTrue(rounds(150L) > 0.9)
    },

    test("редкость — единственная ручка частоты: легендарный копит вчетверо быстрее обычного") {
      val common    = MonsterEnergy.regen(lvl, Rarity.Common)
      val legendary = MonsterEnergy.regen(lvl, Rarity.Legendary)
      assertTrue(legendary >= common * 3L) &&
      assertTrue(MonsterEnergy.regen(lvl, Rarity.Rare) > MonsterEnergy.regen(lvl, Rarity.Uncommon))
    },

    test("стартовый запас: 5–30% потолка, умноженные на редкость, и не выше потолка") {
      val max = MonsterEnergy.maxEnergy(lvl)
      assertTrue(MonsterEnergy.startEnergy(lvl, Rarity.Common, 5L) == max * 5L * 8L / 1000L) &&
      assertTrue(MonsterEnergy.startEnergy(lvl, Rarity.Legendary, 30L) <= max) &&
      // редкий стартует богаче обычного при том же ролле
      assertTrue(MonsterEnergy.startEnergy(lvl, Rarity.Rare, 20L) >
                 MonsterEnergy.startEnergy(lvl, Rarity.Common, 20L))
    },

    test("базовые умения стоят 0.8 базовой цены, расовые — 1.5, округление вверх") {
      assertTrue(MonsterSkill.QuickStrike.cost(lvl) == 40L) &&      // ceil(50 × 0.8)
      assertTrue(MonsterSkill.MurlocPowder.cost(lvl) == 75L) &&     // 50 × 1.5
      assertTrue(MonsterSkill.DirtyStrike.cost(lvl) == 75L) &&
      // округление идёт вверх, а не вниз
      assertTrue(MonsterSkill.QuickStrike.cost(1L) == 26L)          // ceil(32 × 0.8) = 26
    },

    // ── Расовые пулы ──────────────────────────────────────────────────────────
    test("базовые умения есть у всех рас, расовые — только у своей") {
      assertTrue(Race.mortals.forall(MonsterSkill.QuickStrike.availableTo)) &&
      assertTrue(MonsterSkill.MurlocPowder.availableTo(Race.Murloc)) &&
      assertTrue(!MonsterSkill.MurlocPowder.availableTo(Race.Human)) &&
      assertTrue(MonsterSkill.DemonPowder.availableTo(Race.Demon)) &&
      assertTrue(MonsterSkill.GnomePowder.availableTo(Race.Gnome)) &&
      assertTrue(MonsterSkill.KhajiitPowder.availableTo(Race.Khajiit)) &&
      assertTrue(MonsterSkill.ElfPowder.availableTo(Race.Elf)) &&
      assertTrue(!MonsterSkill.DirtyStrike.availableTo(Race.Elf))
    },

    test("моб берёт самое дорогое по карману: с полным запасом мурлок сыплет порошок") {
      val h = hero()
      for {
        r <- strike(h, mobBattle(Race.Murloc, energy = MonsterEnergy.maxEnergy(lvl)), seedTurn(0))
        (after, _, log) = r
      } yield assertTrue(log.contains("достал странную пыль")) &&
              assertTrue(after.effects.monsterPowderUsed) &&
              assertTrue(after.effects.monsterPoisonsOnHit)
    },

    test("порошок высыпается один раз за бой") {
      val h = hero()
      val used = mobBattle(Race.Murloc, energy = MonsterEnergy.maxEnergy(lvl))
      val already = used.copy(effects = used.effects.copy(monsterPowderUsed = true))
      for {
        r <- strike(h, already, seedTurn(0))
        (_, _, log) = r
      } yield assertTrue(!log.contains("достал странную пыль")) &&
              // вместо него ушёл «Грязный удар» — следующее по цене умение мурлока
              assertTrue(log.contains("грязную атаку"))
    },

    test("после порошка мурлока удары травят героя") {
      val h = hero()
      val b = mobBattle(Race.Murloc, energy = 0L)
      val powdered = b.copy(effects = b.effects.copy(monsterPowderUsed = true, monsterPoisonsOnHit = true))
      for {
        r <- strike(h, powdered, seedTurn())
        (after, _, log) = r
      } yield assertTrue(after.effects.heroPoison.isDefined) &&
              assertTrue(log.contains("Отравленное оружие"))
    },

    test("грязный удар мурлока бьёт слабее обычного, но всегда травит") {
      val h = hero()
      val b = mobBattle(Race.Murloc, energy = MonsterEnergy.maxEnergy(lvl))
      val already = b.copy(effects = b.effects.copy(monsterPowderUsed = true))
      for {
        r <- strike(h, already, seedTurn(0))
        (after, _, log) = r
      } yield assertTrue(log.contains("медленно растекается яд")) &&
              assertTrue(after.effects.heroPoison.isDefined)
    },

    test("порошок демона делает удары огненными, гнома — морозными, каджита — воздушными") {
      val h = hero()
      def elementAfter(race: Race) =
        strike(h, mobBattle(race, energy = MonsterEnergy.maxEnergy(lvl)), seedTurn(0))
          .map(_._1.effects.monsterAttackElement)
      for {
        demon   <- elementAfter(Race.Demon)
        gnome   <- elementAfter(Race.Gnome)
        khajiit <- elementAfter(Race.Khajiit)
      } yield assertTrue(demon.contains("Fire")) &&
              assertTrue(gnome.contains("Cold")) &&
              assertTrue(khajiit.contains("Air"))
    },

    // ── Проки стихии, которую дал порошок ─────────────────────────────────────
    test("морозные удары моба грызут защиту героя — и срез копится") {
      val h = hero()
      val b = mobBattle(Race.Gnome, energy = 0L)
      val frosty = b.copy(effects = b.effects.copy(
        monsterPowderUsed = true, monsterAttackElement = Some("Cold")))
      for {
        // 60 — удар героя, 90 — удар моба, 1 — прок стихии прошёл
        r <- strike(h, frosty, seedTurn(1))
        (after, _, log) = r
      } yield assertTrue(after.effects.heroColdDefenceCut == pangea.model.battle.Element.Cold.DefenceReductionCut) &&
              assertTrue(log.contains("защита держит хуже")) &&
              // срез не затухает: следующий прок добавит ещё столько же
              assertTrue(after.effects.heroColdDefenceCut > 0)
    },

    test("срез защиты реально уменьшает снижение урона героя") {
      val tank = hero().copy(fightStats = hero().fightStats.copy(defence = 500))
      val b = mobBattle(energy = 0L)
      def hpLost(cut: Int) =
        strike(tank, b.copy(effects = b.effects.copy(heroColdDefenceCut = cut)), seedTurn())
          .map { case (_, u, _) => 500000L - u.fightStats.hp }
      for {
        plain  <- hpLost(0)
        bitten <- hpLost(20)
      } yield assertTrue(bitten > plain)
    },

    test("воздушный удар бодрит самого моба: точность и уклонение выше на 3 хода") {
      val h = hero()
      val b = mobBattle(Race.Khajiit, energy = 0L)
      val windy = b.copy(effects = b.effects.copy(
        monsterPowderUsed = true, monsterAttackElement = Some("Air")))
      for {
        r <- strike(h, windy, seedTurn(1))
        (after, _, log) = r
      } yield assertTrue(log.contains("Ветер подхватывает врага")) &&
              // буст вешается уже ПОСЛЕ тика начала раунда, поэтому все ходы целы
              assertTrue(after.effects.mobAirBoostTurns == pangea.model.battle.Element.Air.ProcTurns)
    },

    test("огненный порошок отыгрывается поджогом, а не этим проком") {
      val h = hero()
      val b = mobBattle(Race.Demon, energy = 0L)
      val fiery = b.copy(effects = b.effects.copy(
        monsterPowderUsed = true, monsterAttackElement = Some("Fire")))
      for {
        r <- strike(h, fiery, seedTurn(1))
        (after, _, log) = r
      } yield assertTrue(after.effects.heroBurn.isDefined) &&
              assertTrue(after.effects.heroColdDefenceCut == 0) &&
              assertTrue(after.effects.mobAirBoostTurns == 0) &&
              assertTrue(log.contains("поджёг вас"))
    },

    // ── Молния ────────────────────────────────────────────────────────────────
    test("прок Молнии выжигает энергию моба и откладывает его умение") {
      val h = hero(gem = Some(GemKind.Topaz)) // топаз — стихия молнии
      val full = MonsterEnergy.maxEnergy(lvl)
      for {
        // 60 — удар героя, 1 — прок молнии прошёл, 90 — удар моба, 0 — выбор умения
        r <- strike(h, mobBattle(energy = full),
               TestRandom.feedInts(60, 1, 90, 0) *> TestRandom.feedLongs(100L, 100L))
        (after, _, log) = r
        burned = full * pangea.model.battle.Element.LightningEnergyBurnPct / 100L
      } yield assertTrue(log.contains("выжигает")) &&
              // сожгли 30% потолка, потом моб потратил на умение и добрал реген
              assertTrue(after.monsterCurrentEnergy < full) &&
              assertTrue(burned > 0L)
    }
  )
}
