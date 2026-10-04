package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.loot.LootGenerator
import pangea.generator.loot.LootGenerator.LootDrop
import pangea.generator.monster.MonsterGenerator
import pangea.model.battle.{BattleEffects, MonsterSlot, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemType, MaterialKind, Rarity}
import pangea.model.monster.{MiniBoss, Monster, Race, Rarity => MobRarity}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.service.state.states.LootState
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Крысиный король: ком из крыс со дна канализации. Зовёт себе подмогу,
  * поглощает её и разваливается по одной крысе за раунд. */
object RatKingBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  /** Удар по месту `pos`: в строю с крысами бой сперва спрашивает, в кого бить. */
  private def tapAt(pos: Int): UserAction =
    UserAction("", Some(s"""{"action":"Attack","target":"$pos"}"""))

  private val king    = MiniBoss.RatKing
  private val bossLvl = 3L  // герой 10 уровня → BossLvL = (10−1)/3 = 3
  private val questLvl = 7L // уровень канализации, по нему считаются призванные

  private def weapon: Item =
    Item(50L, "Меч", 1L, Rarity.Blue, ItemType.Weapon,
      attack = 10, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  /** Герой бьёт без промаха и переживает всё, что король успеет ответить. */
  private def hero(atk: Long = 100L): Hero =
    TestFixtures.hero(userId).copy(
      lvl          = 10L,
      dungeonLevel = 20,
      fightStats   = FightStats(atk = atk, hp = 500000L, armor = 0, defence = 0,
                                evasion = 0, accuracy = 9999, energy = 0),
      baseStats    = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = 5000),
      equipment    = TestFixtures.emptyEquipment.copy(weapon = weapon)
    )

  private def rat(rarity: MobRarity, lvl: Long = questLvl, hp: Option[Long] = None): MonsterSlot = {
    val m = MonsterGenerator.generateOfRaceAndRarity(lvl.toInt, Race.Animal, rarity)
    MonsterSlot(m.lvl, m.race.entryName, m.rarity.entryName, m.fightStats,
      hp.getOrElse(m.fightStats.hp), m.fightStats.armor, m.marked, 0L, BattleEffects.empty)
  }

  private def kingBattle(
      h: Hero,
      turn: Int,
      energy: Long = 300L,   // максимум короля: 100 × BossLvL
      minions: List[MonsterSlot] = Nil,
      taken: Long = 0L
  ): SoloPveBattle = {
    val stats   = king.stats(bossLvl)
    val monster = Monster(0L, bossLvl, king.race, MobRarity.Legendary, stats)
    val base = SoloPveBattle.from(monster, h).copy(
      bossKind             = Some(king.entryName),
      bossTurn             = turn,
      monsterCurrentEnergy = energy,
      minionLvl            = questLvl,
      bossTaken            = taken,
      noKin                = true,
      effects              = SoloPveBattle.from(monster, h).effects.copy(monsterPoisonsOnHit = true))
    minions.foldLeft(base)((b, s) => b.admit(s))
  }

  private def makeState(h: Hero, battle: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, battle.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  /** Броски хода: удар героя, попадание короля, прок стихии, затем `extra`. */
  private def seedTurn(extra: Int*) =
    TestRandom.feedInts(60 +: 90 +: extra: _*) *> TestRandom.feedLongs(100L, 100L, 100L, 100L)

  private def strike(h: Hero, battle: SoloPveBattle, extra: Int*) = strikeAt(h, battle, None, extra: _*)

  private def strikeAt(h: Hero, battle: SoloPveBattle, at: Option[Int], extra: Int*) =
    for {
      t <- makeState(h, battle)
      (state, dao, r) = t
      _       <- seedTurn(extra: _*)
      _       <- state.action(testUser, at.fold(tap("Attack"))(tapAt), r)
      after   <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      screens <- r.sentScreens
    } yield (after, screens.map(_.text).mkString("\n"))

  override def spec = suite("Крысиный король")(

    // ── Статы ────────────────────────────────────────────────────────────────

    test("статы от BossLvL, а BossLvL — раз в три уровня героя") {
      val s = king.stats(bossLvl)
      assertTrue(king.bossLvl(10L) == 3L && king.bossLvl(1L) == 1L && king.bossLvl(3L) == 1L) &&
      assertTrue(king.bossLvl(4L) == 1L && king.bossLvl(7L) == 2L && king.levelDivisor == 3L) &&
      assertTrue(s.hp == 450L * bossLvl && s.armor == 400L * bossLvl && s.atk == 50L * bossLvl) &&
      assertTrue(s.energy == 100L * bossLvl && s.accuracy == 200L * bossLvl) &&
      assertTrue(s.defence == 100L * bossLvl && s.evasion == 50L * bossLvl) &&
      assertTrue(king.energyRegen(bossLvl) == 7L * bossLvl) &&
      assertTrue(king.expReward(bossLvl) == 200L * bossLvl) &&
      assertTrue(king.monsterName == "Крысиный король" && king.race == Race.Animal) &&
      // три шага в круге, набора под него ещё нет, кровь — его ингредиент
      assertTrue(king.abilities == 3 && king.set.isEmpty) &&
      assertTrue(king.ingredient == MaterialKind.RatKingBlood) &&
      assertTrue(king.poisonsOnHit && king.fightsInGroup && king.roundDamageCapPct == 20L)
    },

    // ── Способности ──────────────────────────────────────────────────────────

    test("призыв: из дыр лезут две-три крысы уровнем в канализацию") {
      for {
        t <- strike(hero(), kingBattle(hero(), turn = 0), 3)
        (after, log) = t
      } yield assertTrue(log.contains("Крысиный писк разносится по окрестностям")) &&
              assertTrue(after.exists(_.group.others.size == 3)) &&
              assertTrue(king.SummonMin == 2 && king.SummonMax == 3) &&
              assertTrue(after.exists(_.group.others.forall(_.lvl == questLvl))) &&
              assertTrue(after.exists(_.group.others.forall(s =>
                s.race == Race.Animal.entryName &&
                  Set[MobRarity](MobRarity.Uncommon, MobRarity.Rare).contains(MobRarity.withName(s.rarity))))) &&
              // призыв не бесплатный: со сдачей за конец раунда
              assertTrue(after.exists(_.monsterCurrentEnergy ==
                300L - king.SummonCostPerLvl * bossLvl + king.energyRegen(bossLvl))) &&
              // и очередь сдвинулась на пропуск
              assertTrue(after.exists(_.bossTurn == 1)) &&
              // звать со стороны ему некого: подкрепления в его бою нет
              assertTrue(after.exists(_.noKin))
    },

    test("без энергии он не призывает, а просто стоит") {
      for {
        t <- strike(hero(), kingBattle(hero(), turn = 0, energy = 1L))
        (after, log) = t
      } yield assertTrue(!log.contains("из дыр вылезают")) &&
              assertTrue(after.exists(_.group.others.isEmpty)) &&
              assertTrue(after.exists(_.monsterCurrentEnergy >= 1L)) &&
              assertTrue(after.exists(_.bossTurn == 1))
    },

    test("объединение: съедает слабейшую, растёт её статами и бьёт сильнее") {
      val strong = rat(MobRarity.Rare)
      val weak   = rat(MobRarity.Uncommon, hp = Some(1L))
      val before = kingBattle(hero(), turn = 2, minions = List(strong, weak))
      for {
        t <- strikeAt(hero(), before, Some(before.group.heroPos), 1, 1)
        (after, log) = t
      } yield assertTrue(log.contains("Одна из крыс стала частью Крысиного короля")) &&
              // съедена именно слабейшая, и в строю осталась сильная
              assertTrue(after.exists(b => b.group.others.size == 1 &&
                b.group.others.head.rarity == MobRarity.Rare.entryName)) &&
              // потолки выросли на её статы, атака — на 25 за уровень босса
              assertTrue(after.exists(_.monsterStats.hp == before.monsterStats.hp + weak.stats.hp)) &&
              assertTrue(after.exists(_.monsterStats.armor == before.monsterStats.armor + weak.stats.armor)) &&
              assertTrue(after.exists(_.monsterStats.atk ==
                before.monsterStats.atk + king.MergeAtkPerLvl * bossLvl)) &&
              // съеденная в павшие не идёт: её не убивали
              assertTrue(after.exists(_.group.slain.isEmpty)) &&
              assertTrue(after.exists(_.monsterCurrentEnergy ==
                300L - king.MergeCostPerLvl * bossLvl + king.energyRegen(bossLvl)))
    },

    test("есть некого — объединение пропускается") {
      for {
        t <- strike(hero(), kingBattle(hero(), turn = 2))
        (after, log) = t
      } yield assertTrue(!log.contains("стала частью")) &&
              assertTrue(after.exists(_.monsterCurrentEnergy == 300L)) &&
              assertTrue(after.exists(_.bossTurn == 0))
    },

    // ── Предел урона за раунд ────────────────────────────────────────────────

    test("за раунд от него отваливается не больше пятой части запаса") {
      val huge = hero(atk = 1000000L)
      val cap  = king.stats(bossLvl).hp * king.RoundDamageCapPct / 100L
      val pool = king.stats(bossLvl).hp + king.stats(bossLvl).armor
      for {
        t <- strike(huge, kingBattle(huge, turn = 1), 1, 1)
        (after, log) = t
      } yield assertTrue(log.contains("Одна из крыс короля пала и перестала быть частью короля")) &&
              // с одного удара он теряет ровно предел, не больше
              assertTrue(after.exists(b => pool - (b.monsterCurrentHp + b.monsterCurrentArmor) == cap)) &&
              assertTrue(after.exists(_.monsterCurrentHp > 0L)) &&
              // счётчик раунда обнулился его же ходом
              assertTrue(after.exists(_.bossTaken == 0L))
    },

    test("предел не трогает ни обычных мобов, ни прочих боссов") {
      val huge  = hero(atk = 1000000L)
      val wolf  = MiniBoss.WhiteWolf
      val stats = wolf.stats(2L)
      val boss  = SoloPveBattle.from(Monster(0L, 2L, wolf.race, MobRarity.Legendary, stats), huge)
                    .copy(bossKind = Some(wolf.entryName), monsterCurrentEnergy = 400L)
      for {
        t <- strike(huge, boss, 1, 1)
        (after, _) = t
      } yield assertTrue(wolf.roundDamageCapPct == 0L) &&
              // волка такой удар сносит целиком — предела у него нет
              assertTrue(after.isEmpty || after.exists(_.monsterCurrentHp == 0L))
    },

    // ── Добыча ───────────────────────────────────────────────────────────────

    test("дроп с короля: кровь и большая руна по два билета, черви — один") {
      val drops = (1L to 2000L).toList.flatMap(s =>
        LootGenerator.rollMiniBoss(king, bossLvl, 10L, Rng(s))._1)
      val mats  = drops.collect { case LootDrop.Gear(i) => i.material }.flatten
      val runes = drops.collect { case LootDrop.Rune(i) => i.name }
      val blood = mats.count(_ == MaterialKind.RatKingBlood)
      val worms = mats.count(_ == MaterialKind.PlagueWorms)
      assertTrue(mats.toSet == Set[MaterialKind](MaterialKind.RatKingBlood, MaterialKind.PlagueWorms)) &&
      assertTrue(runes.forall(_.contains("Большая"))) &&
      // 50 : 50 : 25 — крови и рун поровну, червей вдвое меньше
      assertTrue(blood > worms * 3 / 2 && blood < worms * 5 / 2) &&
      assertTrue(runes.size > worms * 3 / 2 && runes.size < worms * 5 / 2) &&
      // ни трофеев, ни серебра, ни вещей набора — набора у него нет
      assertTrue(!drops.exists {
        case _: LootDrop.Trophy | _: LootDrop.Silver | _: LootDrop.Doubloons => true
        case LootDrop.Gear(i) => i.set.isDefined
        case _                => false
      })
    },

    test("добитые героем крысы роняют своё, съеденные королём — нет") {
      val slainRat = rat(MobRarity.Rare).slain
      // Король на последнем издыхании: удар добивает его и бой кончается.
      val dying = kingBattle(hero(atk = 100000L), turn = 1)
        .copy(monsterCurrentHp = 1L, monsterCurrentArmor = 0L)
      val withSlain = dying.copy(group = dying.group.copy(slain = List(slainRat)))
      for {
        t <- makeState(hero(atk = 100000L), withSlain)
        (state, dao, r) = t
        _     <- seedTurn(1, 1, 1, 1)
        _     <- state.action(testUser, tap("Attack"), r)
        loot  <- dao.readSceneData(userId).map(_.flatMap(_.as[LootState.LootData].toOption))
        names  = loot.map(l => l.monsterName.toList ++ l.queue.map(_.monsterName)).getOrElse(Nil)
      } yield assertTrue(loot.exists(_.won)) &&
              // в добыче две записи: сам король и добитая по дороге крыса
              assertTrue(names.contains(king.monsterName) && names.contains(slainRat.name)) &&
              assertTrue(slainRat.name == "Чумная крыса")
    },

    test("предметов с него (0..1 + BossLvL) / 2, но всегда хотя бы один") {
      def counts(lvl: Long) = (1L to 400L).toList.map(s => LootGenerator.rollMiniBoss(king, lvl, 10L, Rng(s))._1.size)
      val one  = counts(1L)   // (0..1 + 1) / 2 = 0 или 1 → не меньше одного
      val five = counts(5L)   // (0..1 + 5) / 2 = 2 или 3
      val six  = counts(6L)   // (0..1 + 6) / 2 = 3 всегда
      assertTrue(one.forall(_ == 1)) &&
      assertTrue(five.forall(n => n == 2 || n == 3) && five.contains(2) && five.contains(3)) &&
      assertTrue(six.forall(_ == 3)) &&
      // у прочих боссов счёт прежний, без деления пополам
      assertTrue((1L to 50L).toList.map(s =>
        LootGenerator.rollMiniBoss(MiniBoss.WhiteWolf, 6L, 10L, Rng(s))._1.size).forall(n => n == 6 || n == 7))
    }
  )
}
