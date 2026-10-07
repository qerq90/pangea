package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{BattleEffects, Bleed, Poison, SoloPveBattle}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemSet, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Race, Rarity}
import pangea.model.squad.{AllyKind, AllyRates, Squad}
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Набор «Крыса»: броня, яд с удара, своя крыса в строю, зараза вполсилы и
  * чумные крысы на последнем пороге. Падает он с Крысиного короля. */
object RatSetSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  /** Герой попадает, а дальше все броски проходят: так срабатывают и шанс яда
    * (30%), и зов крысы (5%). */
  private val hitAndAllPass = TestRandom.feedInts(99 :: List.fill(40)(1): _*)

  /** Все броски проходят, попадание не важно. */
  private val allRollsPass = TestRandom.feedInts(List.fill(40)(1): _*)

  private val slots = List(ItemType.Helmet, ItemType.ShoulderPads, ItemType.ChestPlate, ItemType.Bracelets,
    ItemType.Gloves, ItemType.Pants, ItemType.Boots, ItemType.Amulet,
    ItemType.Ring, ItemType.Ring, ItemType.Belt, ItemType.Weapon)

  /** Экипировка из `n` предметов набора «Крыса»; `armor` — броня каждого. */
  private def ratEquipment(n: Int, armor: Long): pangea.model.hero.Equipment =
    slots.take(n).zipWithIndex.foldLeft(TestFixtures.emptyEquipment) { case (eq, (t, i)) =>
      val it = Item(700L + i, "Предмет", 1L, ItemRarity.Blue, t,
        attack = 0, accuracy = 0, energy = 0, armor = armor, defence = 0, evasion = 0,
        set = Some(ItemSet.Rat))
      i match {
        case 0  => eq.copy(helmet = it)
        case 1  => eq.copy(shoulderPads = it)
        case 2  => eq.copy(chestPlate = it)
        case 3  => eq.copy(bracelets = it)
        case 4  => eq.copy(gloves = it)
        case 5  => eq.copy(pants = it)
        case 6  => eq.copy(boots = it)
        case 7  => eq.copy(amulet = it)
        case 8  => eq.copy(firstRing = it)
        case 9  => eq.copy(secondRing = it)
        case 10 => eq.copy(belt = it)
        case _  => eq.copy(weapon = it)
      }
    }

  private def hero(pieces: Int, armor: Long = 0L): Hero = TestFixtures.hero(userId).copy(
    lvl        = 10L,
    fightStats = FightStats(atk = 500, hp = 100000, armor = 0, defence = 0,
                            evasion = 9999, accuracy = 9999, energy = 0),
    baseStats  = TestFixtures.hero(userId).baseStats.copy(str = 1),
    equipment  = ratEquipment(pieces, armor))

  /** Моб, которого герой не убьёт с одного удара и который сам его не убьёт. */
  private val tough = SoloPveBattle(
    monsterLvl          = 10L,
    monsterRace         = Race.Orc.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = 1, hp = 100000, armor = 0, defence = 0,
                                     evasion = 0, accuracy = 1, energy = 0),
    monsterCurrentHp    = 100000L,
    monsterCurrentArmor = 0L)

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  override def spec = suite("Набор «Крыса»")(

    test("порог 2: +5% к потолку брони") {
      val bare = hero(1, armor = 1000L)   // один предмет — порог закрыт
      val set  = hero(2, armor = 1000L)
      assertTrue(bare.maxArmor == 1000L) &&
      assertTrue(set.maxArmor == 2000L + 2000L * ItemSet.Rat.ArmorPct / 100L) &&
      assertTrue(ItemSet.Rat.ArmorPct == ItemSet.StatBonusPct)
    },

    test("порог 4: удар по HP травит врага на 3%") {
      for {
        t <- makeState(hero(4), tough)
        (state, dao, r) = t
        _      <- hitAndAllPass
        _      <- state.action(testUser, tap("Attack"), r)
        after  <- battleOf(dao)
        log    <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        // наложился он на 3%, а к концу раунда уже тикнул и ослаб, как любой яд
      } yield assertTrue(after.effects.monsterPoison.contains(Poison(ItemSet.Rat.PoisonPct - Poison.DecayPerRound))) &&
              assertTrue(log.contains("отравлен") && log.contains("Яд снимает")) &&
              assertTrue(ItemSet.Rat.PoisonChancePct == 30L && ItemSet.Rat.PoisonPct == 3)
    },

    test("без порога 4 удар не травит, и броска на это не тратится") {
      for {
        t <- makeState(hero(2), tough)
        (state, dao, r) = t
        _      <- hitAndAllPass
        _      <- state.action(testUser, tap("Attack"), r)
        after  <- battleOf(dao)
      } yield assertTrue(after.effects.monsterPoison.isEmpty)
    },

    test("порог 6: из-под ног выскакивает обычная крыса уровнем в героя и встаёт рядом") {
      for {
        t <- makeState(hero(6), tough)
        (state, dao, r) = t
        _      <- hitAndAllPass
        _      <- state.action(testUser, tap("Attack"), r)
        after  <- battleOf(dao)
        log    <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        rat     = after.group.allies.headOption
      } yield assertTrue(after.group.hasRatAlly && after.group.allies.size == 1) &&
              assertTrue(rat.exists(a => a.kind == AllyKind.Rat && a.name == "Крыса" && a.lvl == 10L)) &&
              // встала рядом с героем, и «домом» её не записали: она не из отряда
              assertTrue(rat.exists(a => a.position == 2 && a.home == 0)) &&
              assertTrue(rat.exists(a => a.hp == a.stats.hp && a.hp > 0L)) &&
              assertTrue(log.contains("Из-под ваших ног выскакивает Крыса")) &&
              assertTrue(ItemSet.Rat.SummonChancePct == 5L)
    },

    test("без порога 6 никто не выскакивает") {
      for {
        t <- makeState(hero(4), tough)
        (state, dao, r) = t
        _      <- hitAndAllPass
        _      <- state.action(testUser, tap("Attack"), r)
        after  <- battleOf(dao)
      } yield assertTrue(after.group.allies.isEmpty)
    },

    test("порог 12: приходят чумные крысы, и одна встаёт сразу, если своих нет") {
      for {
        t <- makeState(hero(12), tough)
        (state, dao, r) = t
        _      <- state.enter(testUser, r)
        after  <- battleOf(dao)
        log    <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        rat     = after.group.allies.headOption
      } yield assertTrue(rat.exists(a => a.kind == AllyKind.Rat && a.name == "Чумная крыса")) &&
              assertTrue(log.contains("Из-под ваших ног выскакивает Чумная крыса")) &&
              // второй раз на том же раунде не приходит
              assertTrue(after.group.hasRatAlly)
    },

    test("порог 12: чумные приходят и на зов за раунд, не только на входе") {
      for {
        t <- makeState(hero(12), tough)
        (state, dao, r) = t
        _      <- hitAndAllPass
        _      <- state.action(testUser, tap("Attack"), r)   // без enter: начальной крысы нет
        after  <- battleOf(dao)
      } yield assertTrue(after.group.allies.map(_.name) == List("Чумная крыса")) &&
              assertTrue(hero(12).sets.summonsPlagueRats && !hero(6).sets.summonsPlagueRats)
    },

    test("союзная крыса умений не знает — только кусает, и энергию не тратит") {
      for {
        t <- makeState(hero(12), tough)
        (state, dao, r) = t
        _      <- state.enter(testUser, r)        // крыса встала в строй
        _      <- hitAndAllPass
        _      <- state.action(testUser, tap("Attack"), r)
        after  <- battleOf(dao)
        log    <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        rat     = after.group.allies.head
        // ход крысы виден в логе — ударом либо промахом; умение дало бы вторую такую строку
        swings  = (log.split("Чумная крыса бьёт").length - 1) +
                  (log.split("Чумная крыса промахивается").length - 1)
      } yield assertTrue(!AllyKind.Rat.usesSkills) &&
              assertTrue(AllyKind.values.filterNot(_ == AllyKind.Rat).forall(_.usesSkills)) &&
              assertTrue(swings == 1) &&
              // и энергия при ней осталась вся: платить ей не за что
              assertTrue(rat.energy == rat.stats.energy && rat.energy > 0L)
    },

    test("порог 12: вторую крысу на входе не дают — ни когда своя уже есть, ни во втором раунде") {
      val withRat  = tough.copy(group = tough.group.copy(round = 0))
      val lateRound = tough.copy(group = tough.group.copy(round = 1))
      for {
        t1 <- makeState(hero(12), withRat)
        (s1, dao1, r1) = t1
        _  <- s1.enter(testUser, r1)
        _  <- s1.enter(testUser, r1)          // вернулись из снаряжения
        one <- battleOf(dao1)
        t2 <- makeState(hero(12), lateRound)
        (s2, dao2, r2) = t2
        _  <- s2.enter(testUser, r2)
        none <- battleOf(dao2)
      } yield assertTrue(one.group.allies.size == 1) &&
              assertTrue(none.group.allies.isEmpty)
    },

    test("порог 10: яд и кровь снимают с героя вдвое меньше, а рана затягивается") {
      val maxHp = hero(10).effectiveMaxHp(0L)
      val hurt  = tough.copy(effects = BattleEffects(
        heroPoison = Some(Poison(10)), heroBleed = Some(Bleed(10))))
      for {
        t <- makeState(hero(10).copy(fightStats = hero(10).fightStats.copy(hp = maxHp)), hurt)
        (state, dao, r) = t
        _     <- allRollsPass
        _     <- state.action(testUser, tap("Attack"), r)
        after <- battleOf(dao)
        h     <- dao.getHeroByUserId(userId).map(_.get)
        tick   = maxHp * 10L / 100L / 2L            // по 10% макс.HP, вдвое слабее
      } yield assertTrue(h.fightStats.hp == maxHp - tick * 2L) &&
              // кровь затухает, как яд: оба ослабли на свои два процента
              assertTrue(after.effects.heroBleed.contains(Bleed(10 - ItemSet.Rat.BleedDecayPerRound))) &&
              assertTrue(after.effects.heroPoison.contains(Poison(10 - Poison.DecayPerRound))) &&
              assertTrue(ItemSet.Rat.BleedDecayPerRound == Poison.DecayPerRound)
    },

    test("без порога 10 кровь не затухает сама — её снимает только лечение") {
      val hurt = tough.copy(effects = BattleEffects(heroBleed = Some(Bleed(10))))
      for {
        t <- makeState(hero(8), hurt)
        (state, dao, r) = t
        _     <- allRollsPass
        _     <- state.action(testUser, tap("Attack"), r)
        after <- battleOf(dao)
      } yield assertTrue(after.effects.heroBleed.contains(Bleed(10)))
    },

    test("после боя крыса переходит в отряд — даже если отряда у героя не было") {
      // Моб с одним HP: удар его добивает, бой кончается победой.
      val weak = tough.copy(monsterStats = tough.monsterStats.copy(hp = 1L), monsterCurrentHp = 1L)
      for {
        t <- makeState(hero(12), weak)
        (state, dao, r) = t
        _     <- state.enter(testUser, r)          // крыса порога 12 встаёт в строй
        inFight <- battleOf(dao)
        _     <- hitAndAllPass
        _     <- state.action(testUser, tap("Attack"), r)
        after <- dao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(inFight.group.allies.size == 1) &&
              assertTrue(after.squad.hasRat && after.squad.allies.size == 1) &&
              assertTrue(after.squad.allies.head.name == "Чумная крыса") &&
              assertTrue(after.squad.allies.head.hiredUntil > 0L)
    },

    test("крыса остаётся при герое на сутки и в отряд попадает со своим сроком") {
      val now = 1_000_000L
      val sq  = Squad.empty.summonRat(
        pangea.model.squad.UndeadForm("Крыса", 10L,
          FightStats(atk = 10, hp = 100, armor = 50, defence = 1, evasion = 1, accuracy = 10, energy = 10)),
        10L, now)
      val rat = sq.allyAt(2)
      assertTrue(rat.exists(a => a.kind == AllyKind.Rat && a.hiredUntil == now + AllyRates.RatMs)) &&
      assertTrue(rat.exists(a => a.hp == 100L && a.armor == 50L && a.lvlAt(99L) == 10L)) &&
      assertTrue(sq.hasRat && AllyRates.RatMs == 24L * 60L * 60L * 1000L) &&
      // срок вышел — убегает
      assertTrue(rat.exists(_.leaving(now + AllyRates.RatMs))) &&
      assertTrue(sq.expire(now + AllyRates.RatMs)._2.map(_.kind) == List(AllyKind.Rat))
    }
  )
}
