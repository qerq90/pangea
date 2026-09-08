package pangea.model.hero

import pangea.domain.Rng
import pangea.generator.item.ItemNameGenerator
import pangea.model.item.{Item, ItemSet, ItemType, Rarity}
import pangea.test.TestFixtures
import zio.test._

object HeroSetsSpec extends ZIOSpecDefault {

  private val userId = pangea.model.user.UserId(1L)

  private def piece(id: Long, itemType: ItemType, set: Option[ItemSet]): Item =
    Item(id, "Предмет", 1L, Rarity.Blue, itemType,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, set = set)

  private def heroWearing(eq: Equipment) = TestFixtures.hero(userId).copy(equipment = eq)

  override def spec = suite("HeroSets")(

    // Регрессия: пока варианты читали общие ставки из СВОЕГО компаньона, его
    // инициализация начиналась изнутри их конструкторов, а он в этот момент сам
    // собирал их через findValues. На одном потоке это клало в values null, на
    // нескольких — вешало инициализацию класса намертво (тесты не завершались).
    test("values не содержит null и бонусы читаются без цикла инициализации") {
      val forced = ItemSet.StoneGuard
      assertTrue(forced.bonuses.nonEmpty) &&
      assertTrue(!ItemSet.values.contains(null)) &&
      assertTrue(ItemSet.values.forall(_.bonuses.map(_.pieces) == ItemSet.Thresholds)) &&
      assertTrue(ItemSet.values.forall(_.bonuses.forall(_.text.nonEmpty)))
    },

    test("бонусы открываются по порогам 2/4/6/8/10/12 и не раньше") {
      val counts = List(0, 1, 2, 3, 4, 8, 11, 12)
      val opened = counts.map(n => n -> HeroSets(Map(ItemSet.Ghoul -> n)).bonuses(ItemSet.Ghoul).map(_.pieces))
      assertTrue(opened == List(
        0  -> Nil,
        1  -> Nil,
        2  -> List(2),
        3  -> List(2),          // третий предмет ничего не открывает
        4  -> List(2, 4),
        8  -> List(2, 4, 6, 8),
        11 -> List(2, 4, 6, 8, 10),
        12 -> List(2, 4, 6, 8, 10, 12)
      ))
    },

    test("наборы независимы: 12 предметов из шести наборов дают шесть двойных бонусов") {
      // Шесть наборов игра пока не знает — берём все четыре по два предмета.
      val counts = ItemSet.values.map(_ -> 2).toMap
      val sets   = HeroSets(counts)
      assertTrue(sets.activeBonuses.size == ItemSet.values.size) &&
      assertTrue(sets.activeBonuses.forall { case (_, bs) => bs.map(_.pieces) == List(2) }) &&
      // каждый набор отдал свою прибавку к своему стату
      assertTrue(sets.defenceBonusPct == 5L) &&
      assertTrue(sets.attackBonusPct == 5L) &&
      assertTrue(sets.evasionBonusPct == 5L) &&
      assertTrue(sets.accuracyBonusPct == 5L)
    },

    test("снятие предмета закрывает порог обратно") {
      val four = HeroSets(Map(ItemSet.Hunter -> 4))
      val three = HeroSets(Map(ItemSet.Hunter -> 3))
      assertTrue(four.has(ItemSet.Hunter, 4)) &&
      assertTrue(four.energyBonusPct == ItemSet.Hunter.EnergyPct) &&
      assertTrue(!three.has(ItemSet.Hunter, 4)) &&
      assertTrue(three.energyBonusPct == 0L) &&
      assertTrue(three.accuracyBonusPct == 5L) // порог 2 ещё держится
    },

    test("все четыре набора реализованы целиком — «показываем, но не работает» не осталось") {
      assertTrue(ItemSet.values.forall(_.bonuses.forall(_.active)))
    },

    test("«Каменный страж» реализован целиком: все шесть порогов действуют") {
      val full = HeroSets(Map(ItemSet.StoneGuard -> 12))
      assertTrue(full.defenceBonusPct == ItemSet.StoneGuard.DefencePct) &&
      assertTrue(full.elementalDamageTakenMult == 0.8) &&
      assertTrue(full.armorDamageTakenMult == 0.8) &&
      assertTrue(full.igniteResistPct == ItemSet.StoneGuard.IgniteResistPct) &&
      assertTrue(full.rescuesOnLowHp)
    },

    test("пороги «Каменного стража» включаются по одному, а не все сразу") {
      val six = HeroSets(Map(ItemSet.StoneGuard -> 6))
      assertTrue(six.elementalDamageTakenMult == 0.8) && // порог 4 набран
      assertTrue(six.armorSpent(100L) == 80L) &&         // порог 6 набран
      assertTrue(six.igniteResistPct == 0L) &&           // порог 10 ещё нет
      assertTrue(!six.rescuesOnLowHp) &&                 // порог 12 ещё нет
      // без набора броня тратится ровно на поглощённое
      assertTrue(HeroSets(Map.empty).armorSpent(100L) == 100L)
    },

    test("«Упырь» реализован целиком: все шесть порогов действуют") {
      val full = HeroSets(Map(ItemSet.Ghoul -> 12))
      assertTrue(full.bonuses(ItemSet.Ghoul).forall(_.active)) &&
      assertTrue(full.lifestealPct == ItemSet.Ghoul.LifestealPct) &&
      assertTrue(full.bleedOnHitChancePct == ItemSet.Ghoul.BleedChancePct) &&
      assertTrue(full.healsFromBleed) &&
      assertTrue(full.skillsAlwaysBleed) &&
      assertTrue(full.feastsOnKill)
    },

    test("пороги «Упыря» включаются по одному, а не все сразу") {
      val four = HeroSets(Map(ItemSet.Ghoul -> 4))
      assertTrue(four.lifestealPct == 2L) &&        // порог 4 набран
      assertTrue(four.bleedOnHitChancePct == 0L) && // порог 6 ещё нет
      assertTrue(!four.healsFromBleed) &&
      assertTrue(!four.skillsAlwaysBleed) &&
      assertTrue(!four.feastsOnKill)
    },

    test("порог 8 у каждого набора даёт +300 HP и +10% HP, и они складываются") {
      val one = HeroSets(Map(ItemSet.Ghoul -> 8))
      val two = HeroSets(Map(ItemSet.Ghoul -> 8, ItemSet.Hunter -> 8))
      assertTrue(one.flatHp == 300L && one.maxHpBonusPct == 10L) &&
      assertTrue(two.flatHp == 600L && two.maxHpBonusPct == 20L)
    },

    // ── Подключение к статам героя ────────────────────────────────────────────
    test("считаются только 12 сетовых слотов: фляга и доп. оружие в набор не идут") {
      val eq = TestFixtures.emptyEquipment.copy(
        flask            = piece(1L, ItemType.Flask, Some(ItemSet.Ghoul)),
        additionalWeapon = piece(2L, ItemType.AdditionalWeapon, Some(ItemSet.Ghoul)),
        helmet           = piece(3L, ItemType.Helmet, Some(ItemSet.Ghoul)))
      assertTrue(eq.setCounts == Map[ItemSet, Int](ItemSet.Ghoul -> 1)) // только шлем
    },

    test("два предмета «Дикого пламени» дают +5% к атаке в итоговых статах") {
      val hero0 = heroWearing(TestFixtures.emptyEquipment)
      val hero2 = heroWearing(TestFixtures.wearingSet(ItemSet.WildFlame, 2))
      val base  = hero0.effectiveFightStats(0L).atk
      assertTrue(hero2.effectiveFightStats(0L).atk == base + base * 5L / 100L)
    },

    test("восемь предметов набора поднимают макс. HP на 300 и на 10%") {
      val hero0 = heroWearing(TestFixtures.emptyEquipment)
      val hero8 = heroWearing(TestFixtures.wearingSet(ItemSet.StoneGuard, 8))
      val base  = hero0.effectiveMaxHp(0L)
      assertTrue(hero8.effectiveMaxHp(0L) == (base + 300L) * 110L / 100L)
    },

    test("«Охотник» на 4 предметах поднимает макс. энергию на 10%") {
      val hero0 = heroWearing(TestFixtures.emptyEquipment)
      val hero4 = heroWearing(TestFixtures.wearingSet(ItemSet.Hunter, 4))
      assertTrue(hero4.maxEnergy(0L) > hero0.maxEnergy(0L)) &&
      assertTrue(hero4.sets.agiEnergyRegenMult == 2L)
    },

    // ── Название предмета ─────────────────────────────────────────────────────
    test("имя сетового предмета несёт название набора третьим словом вместо титула") {
      val (name, _) = ItemNameGenerator.setName(ItemType.Helmet, Rarity.Blue, ItemSet.StoneGuard, Rng(42L))
      val words     = name.split(" ").toList
      assertTrue(name.startsWith(Rarity.Blue.emoji)) &&
      // кружок, прилагательное, тип, затем имя набора (оно из двух слов)
      assertTrue(words.drop(3).mkString(" ") == ItemSet.StoneGuard.title) &&
      assertTrue(ItemSet.byTitle(words.drop(3).mkString(" ")).contains(ItemSet.StoneGuard))
    },

    test("у каждого набора имя-титул уникально и распознаётся обратно") {
      assertTrue(ItemSet.values.map(_.title).distinct.size == ItemSet.values.size) &&
      assertTrue(ItemSet.values.forall(s => ItemSet.byTitle(s.title).contains(s)))
    },

    test("описание предмета показывает его набор") {
      val it = piece(1L, ItemType.Helmet, Some(ItemSet.Ghoul))
      assertTrue(it.statsLines.exists(_ == "Набор: «Упырь»")) &&
      assertTrue(!piece(2L, ItemType.Helmet, None).statsLines.exists(_.startsWith("Набор")))
    }
  )
}
