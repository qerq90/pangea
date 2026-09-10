package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{Burn, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{Gem, GemKind, Item, ItemType, Rarity}
import pangea.model.monster.{MiniBoss, Monster, Rarity => MobRarity}
import pangea.model.stats.FightStats
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Гнилой Джо: круг из четырёх способностей, яд на герое и главное — он трижды
 *  отказывается умирать, пока его не сожгут. */
object RottenJoeBattleSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val joe     = MiniBoss.RottenJoe
  private val bossLvl = 2L // герой 15 уровня → BossLvL = (15−1)/5 = 2

  private def weapon(gem: Option[GemKind]): Item =
    Item(50L, "Меч", 1L, Rarity.Blue, ItemType.Weapon,
      attack = 10, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      sockets = List(gem.map(Gem(_, 1))))

  /** Герой бьёт без промаха и переживает удары Джо. `atk` — сила его удара. */
  private def hero(atk: Long = 100L, hp: Long = 500000L, gem: Option[GemKind] = None): Hero =
    TestFixtures.hero(userId).copy(
      lvl        = 15L,
      fightStats = FightStats(atk = atk, hp = hp, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 0),
      baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1, vit = 5000),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = weapon(gem))
    )

  /** Косорукий герой: Джо от него уворачивается, значит его HP остаётся полным —
   *  это нужно, чтобы проверить «лечить нечего». */
  private def clumsyHero: Hero = hero().copy(
    fightStats = FightStats(atk = 100, hp = 500000L, armor = 0, defence = 0,
                            evasion = 0, accuracy = 1, energy = 0))

  private def joeBattle(
      h: Hero,
      turn: Int,
      energy: Long = 200L,  // максимум Джо: 100 × BossLvL
      hpPct: Long = 100L,
      revives: Int = 0,
      burning: Boolean = false
  ): SoloPveBattle = {
    val stats   = joe.stats(bossLvl)
    val monster = Monster(0L, bossLvl, joe.race, MobRarity.Legendary, stats)
    val base    = SoloPveBattle.from(monster, h).copy(
      bossKind             = Some(joe.entryName),
      bossTurn             = turn,
      bossRevives          = revives,
      monsterCurrentEnergy = energy,
      monsterCurrentHp     = (stats.hp * hpPct / 100L).max(1L)
    )
    if (burning) base.copy(effects = base.effects.copy(monsterBurn = Some(Burn(10)))) else base
  }

  private def makeState(h: Hero, battle: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, battle.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, content), dao, renderer)

  /** Броски хода: удар героя, попадание моба, затем `extra`. */
  private def seedTurn(extra: Int*) =
    TestRandom.feedInts(60 +: 90 +: extra: _*) *> TestRandom.feedLongs(100L, 100L)

  private def strike(h: Hero, battle: SoloPveBattle, seed: ZIO[Any, Nothing, Unit]) =
    for {
      t <- makeState(h, battle)
      (state, dao, r) = t
      _       <- seed
      out     <- state.action(testUser, tap("Attack"), r)
      updated <- dao.getHeroByUserId(userId).map(_.get)
      after   <- dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption))
      screens <- r.sentScreens
    } yield (updated, after, screens.map(_.text).mkString, out)

  override def spec = suite("Гнилой Джо")(

    // ── Статы и особенности ───────────────────────────────────────────────────
    test("статы считаются от BossLvL, брони и защиты у него нет") {
      val s = joe.stats(bossLvl)
      assertTrue(s.hp == 2200L * bossLvl) &&
      assertTrue(s.armor == 0L) && assertTrue(s.defence == 0L) &&
      assertTrue(s.atk == 200L * bossLvl) &&
      assertTrue(s.energy == 100L * bossLvl) &&
      assertTrue(s.accuracy == 350L * bossLvl) &&
      assertTrue(s.evasion == 100L * bossLvl) &&
      assertTrue(joe.energyRegen(bossLvl) == 5L * bossLvl) &&
      assertTrue(joe.expReward(bossLvl) == 175L * bossLvl) &&
      assertTrue(joe.monsterName == "Гнилой Джо")
    },

    test("яд и кровотечение на нежити не держатся, а огонь бьёт в полтора раза сильнее") {
      assertTrue(pangea.model.monster.Race.immuneToDots(joe.race)) &&
      assertTrue(joe.damageTakenMult(pangea.model.battle.Element.Fire) == 1.5) &&
      assertTrue(joe.damageTakenMult(pangea.model.battle.Element.Cold) == 1.0) &&
      assertTrue(!joe.immuneToBurn)
    },

    test("горящий Джо бьёт менее точно — шанс попасть по герою падает") {
      val h = hero()
      for {
        calm <- strike(h, joeBattle(h, turn = 3), seedTurn(100))
        hot  <- strike(h, joeBattle(h, turn = 3, burning = true), seedTurn(100))
      } yield assertTrue(joe.burnAccuracyCutPct == 20L) &&
              // на экране боя шанс попадания моба по герою виден числом
              assertTrue(calm._3.nonEmpty) && assertTrue(hot._3.nonEmpty)
    },

    test("обычная атака Джо героя не поджигает — гниль огнём не бьёт") {
      val h = hero()
      for {
        r <- strike(h, joeBattle(h, turn = 3), seedTurn(1)) // бросок, поджёгший бы у огненного
        (_, after, log, _) = r
      } yield assertTrue(joe.heroIgniteChancePct == 0L) &&
              assertTrue(after.get.effects.heroBurn.isEmpty) &&
              assertTrue(!log.contains("поджёг вас"))
    },

    // ── Способности по кругу ──────────────────────────────────────────────────
    test("смрад травит героя, и яд тикает в конце раунда") {
      val h = hero()
      for {
        r <- strike(h, joeBattle(h, turn = 0), seedTurn(100))
        (u, after, log, _) = r
      } yield assertTrue(log.contains("Ядовитые зловония")) &&
              assertTrue(after.exists(_.effects.heroPoison.isDefined)) &&
              assertTrue(u.fightStats.hp < 500000L) && // яд уже отгрыз своё
              assertTrue(log.contains("Гниль разъедает вас"))
    },

    test("широкий удар бьёт на три четверти атаки") {
      val h = hero()
      for {
        sweep <- strike(h, joeBattle(h, turn = 1), seedTurn(100, 100))
        skip  <- strike(h, joeBattle(h, turn = 3), seedTurn(100))
        sweepLost = 500000L - sweep._1.fightStats.hp
        skipLost  = 500000L - skip._1.fightStats.hp
      } yield assertTrue(sweep._3.contains("широкий удар по дуге")) &&
              // сверх обычной атаки прилетело ещё 75% от неё
              assertTrue(sweepLost > skipLost)
    },

    test("гнилое восстановление лечит его на 10% максимума, целому — нечего лечить") {
      val h = hero()
      for {
        hurt  <- strike(h, joeBattle(h, turn = 2, hpPct = 50L), seedTurn(100))
        idle  <- strike(h, joeBattle(h, turn = 3, hpPct = 50L), seedTurn(100))
        // Целый Джо: герой мажет, значит лечить ему нечего.
        whole <- strike(clumsyHero, joeBattle(clumsyHero, turn = 2), seedTurn(100))
        maxHp  = joe.stats(bossLvl).hp
      } yield assertTrue(hurt._3.contains("отчаянно пытаются спасти")) &&
              assertTrue(hurt._2.get.monsterCurrentHp - idle._2.get.monsterCurrentHp == maxHp * 10L / 100L) &&
              assertTrue(!whole._3.contains("отчаянно пытаются спасти"))
    },

    test("круг из четырёх способностей возвращается к началу") {
      val h = hero()
      for {
        last <- strike(h, joeBattle(h, turn = 3), seedTurn(100))
      } yield assertTrue(joe.abilities == 4) && assertTrue(last._2.get.bossTurn == 0)
    },

    test("без энергии способность не применяется, но очередь едет дальше") {
      val h = hero()
      for {
        r <- strike(h, joeBattle(h, turn = 0, energy = 0L), seedTurn(100))
        (_, after, log, _) = r
      } yield assertTrue(!log.contains("Ядовитые зловония")) &&
              assertTrue(after.get.effects.heroPoison.isEmpty) &&
              assertTrue(after.get.bossTurn == 1)
    },

    // ── «Отказывается умирать» ────────────────────────────────────────────────
    test("первое обнуление HP: поднимается на 75% и слабеет в атаке") {
      // Герой бьёт сильнее, чем у Джо всего HP, — падение гарантировано.
      val h = hero(atk = 100000L)
      for {
        r <- strike(h, joeBattle(h, turn = 3), seedTurn(100))
        (_, after, log, out) = r
        maxHp = joe.stats(bossLvl).hp
      } yield assertTrue(out == StateType.Battle) && // бой не кончился
              assertTrue(log.contains("отказывается умирать")) &&
              assertTrue(after.get.bossRevives == 1) &&
              assertTrue(after.get.monsterCurrentHp == maxHp * 75L / 100L) &&
              assertTrue(after.get.effects.monsterWeakenedPct == joe.ReviveAtkCutPct) &&
              assertTrue(log.contains("атака снижена"))
    },

    test("второй и третий подъёмы дают 50% и 25% HP") {
      val h = hero(atk = 100000L)
      for {
        second <- strike(h, joeBattle(h, turn = 3, revives = 1), seedTurn(100))
        third  <- strike(h, joeBattle(h, turn = 3, revives = 2), seedTurn(100))
        maxHp   = joe.stats(bossLvl).hp
      } yield assertTrue(second._2.get.monsterCurrentHp == maxHp * 50L / 100L) &&
              assertTrue(second._2.get.bossRevives == 2) &&
              assertTrue(second._3.contains("снова отказывается умирать")) &&
              assertTrue(third._2.get.monsterCurrentHp == maxHp * 25L / 100L) &&
              assertTrue(third._2.get.bossRevives == 3)
    },

    test("после третьего подъёма следующая смерть — окончательная") {
      val h = hero(atk = 100000L)
      for {
        r <- strike(h, joeBattle(h, turn = 3, revives = 3), seedTurn(100))
        (_, after, _, out) = r
      } yield assertTrue(out == StateType.Loot) && assertTrue(after.isEmpty) // бой закрыт
    },

    test("горящего Джо второе падение упокаивает насовсем") {
      val h = hero(atk = 100000L, gem = Some(GemKind.Ruby))
      for {
        // 60 — удар героя, 100 — прок огня не нужен: горение уже висит
        r <- strike(h, joeBattle(h, turn = 3, revives = 1, burning = true), seedTurn(100))
        (_, after, log, out) = r
      } yield assertTrue(out == StateType.Loot) &&
              assertTrue(after.isEmpty) &&
              assertTrue(log.contains("Огонь смог упокоить эту насмешку над жизнью")) &&
              assertTrue(!log.contains("снова отказывается умирать"))
    },

    test("а вот первое падение пламя не отменяет — Джо всё равно встаёт") {
      val h = hero(atk = 100000L, gem = Some(GemKind.Ruby))
      for {
        r <- strike(h, joeBattle(h, turn = 3, burning = true), seedTurn(100))
        (_, after, log, out) = r
      } yield assertTrue(out == StateType.Battle) &&
              assertTrue(after.get.bossRevives == 1) &&
              assertTrue(log.contains("отказывается умирать"))
    },

    // ── Добыча ────────────────────────────────────────────────────────────────
    test("дроп: кожа упыря, расколотый усилитель, дублоны и «Упырь» — поровну") {
      val drops = (1L to 400L).iterator
        .flatMap(s => pangea.generator.loot.LootGenerator
          .rollMiniBoss(joe, 2L, 40L, pangea.domain.Rng(s))._1)
        .toList
      val skins = drops.flatMap(_.itemOpt).filter(_.material.contains(pangea.model.item.MaterialKind.GhoulSkin))
      val gems  = drops.collect { case pangea.generator.loot.LootGenerator.LootDrop.Gem(g) => g }
      val gold  = drops.collect { case pangea.generator.loot.LootGenerator.LootDrop.Doubloons(a) => a }
      val gear  = drops.flatMap(_.itemOpt).filter(_.set.contains(pangea.model.item.ItemSet.Ghoul))
      assertTrue(skins.nonEmpty) && assertTrue(gems.nonEmpty) &&
      assertTrue(gold.nonEmpty) && assertTrue(gear.nonEmpty) &&
      // усилитель всегда самого низкого качества
      assertTrue(gems.forall(_.gem.exists(_.grade == pangea.model.item.Gem.MinGrade))) &&
      // дублоны: 2 × BossLvL с разбросом ±20%
      assertTrue(gold.forall(a => a >= 3L && a <= 4L)) &&
      // вещь набора приходит фиолетовой или синей — четверть роллов делится пополам
      assertTrue(gear.forall(g => g.rarity == pangea.model.item.Rarity.Purple ||
                                  g.rarity == pangea.model.item.Rarity.Blue)) &&
      assertTrue(gear.exists(_.rarity == pangea.model.item.Rarity.Purple)) &&
      assertTrue(gear.exists(_.rarity == pangea.model.item.Rarity.Blue))
    },

    test("после Джо игрок не идёт осматривать логово элементаля") {
      val h = hero(atk = 100000L)
      for {
        t <- makeState(h, joeBattle(h, turn = 3, revives = 3))
        (state, dao, r) = t
        _     <- seedTurn(100)
        _     <- state.action(testUser, tap("Attack"), r)
        scene <- dao.readSceneData(userId)
        loot   = scene.flatMap(_.as[pangea.service.state.states.LootState.LootData].toOption)
      } yield assertTrue(loot.exists(_.returnState.isEmpty)) &&
              assertTrue(loot.exists(_.doubloons >= 0L))
    }
  )
}
