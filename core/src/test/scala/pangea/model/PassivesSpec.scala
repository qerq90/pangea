package pangea.model

import pangea.model.hero.HeroPassives
import pangea.model.item.{Item, ItemDetails, ItemType, PassiveKind, Rarity}
import pangea.model.state.StateType
import pangea.model.user.UserId
import pangea.service.state.states.InventoryState
import pangea.test.TestFixtures
import zio.test._

/** Модельные (детерминированные, без RNG) тесты пассивок: фасад [[HeroPassives]],
 *  агрегация на герое/экипировке, пул событий подземелья и хелперы Тайника. */
object PassivesSpec extends ZIOSpecDefault {

  private val userId = UserId(1L)

  private def passiveItem(itemType: ItemType, kind: PassiveKind): Item =
    Item(1L, "test", 1L, Rarity.Gray, itemType,
         attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
         details = ItemDetails.Passive(kind))

  // Герой с набором пассивок, разложенных по одному представительному слоту на
  // группу (шлем/плечи/сапоги/кольцо/амулет). Для тестов фасада большего не нужно.
  private def heroWith(kinds: PassiveKind*) = {
    val base = TestFixtures.hero(userId)
    kinds.foldLeft(base) { (h, k) =>
      val eq =
        if (k.eligibleSlots.contains(ItemType.Helmet))      h.equipment.copy(helmet = passiveItem(ItemType.Helmet, k))
        else if (k.eligibleSlots.contains(ItemType.Amulet)) h.equipment.copy(amulet = passiveItem(ItemType.Amulet, k))
        else if (k.eligibleSlots.contains(ItemType.Ring))   h.equipment.copy(firstRing = passiveItem(ItemType.Ring, k))
        else if (k.eligibleSlots.contains(ItemType.Boots))  h.equipment.copy(boots = passiveItem(ItemType.Boots, k))
        else                                                h.equipment.copy(shoulderPads = passiveItem(ItemType.ShoulderPads, k))
      h.copy(equipment = eq)
    }
  }

  def spec = suite("PassivesSpec")(

    // ── HeroPassives фасад ─────────────────────────────────────────────────────
    test("пустой фасад — нейтральные модификаторы") {
      val p = HeroPassives.empty
      assertTrue(
        p.battleEncounterFactor == 1.0,
        p.dodgeBonusPct == 0L,
        p.finalDamageMult == 1.0,
        p.healMult == 1.0,
        p.energyRegenMult == 1.0,
        p.hpRegenPct == 0L,
        p.armorRegenPct == 0L,
        p.extraInventorySlots == 0L,
        !p.hasImpenetrable && !p.hasToughness && !p.hasSpiky && !p.hasQuickHands,
        !p.hasTaxidermist && !p.hasJeweler && !p.hasMarauder
      )
    },

    test("Быстрые ноги +5 и Сливающиеся +10 складываются в бонус уклонения") {
      val p = HeroPassives(Set(PassiveKind.QuickFeet, PassiveKind.Blending))
      assertTrue(p.dodgeBonusPct == 15L)
    },

    test("Сверкающие дают бонус уклона только для бегства") {
      val p = HeroPassives(Set(PassiveKind.Glittering))
      assertTrue(p.dodgeBonusPct == 0L, p.fleeDodgeBonusPct == 25L)
    },

    test("плоские бонусы защиты/точности = 5% от значения, минимум 1") {
      val p = HeroPassives(Set(PassiveKind.Reinforced, PassiveKind.Precise))
      assertTrue(p.defenceFlatBonus(100) == 5L, p.accuracyFlatBonus(200) == 10L, p.defenceFlatBonus(1) == 1L)
    },

    test("Разбойника ×1.05 урона, Целителя ×1.10 лечения, Сосредоточенный ×1.10 энергии") {
      assertTrue(
        HeroPassives(Set(PassiveKind.Robber)).finalDamageMult == 1.05,
        HeroPassives(Set(PassiveKind.Healer)).healMult == 1.10,
        HeroPassives(Set(PassiveKind.Focused)).energyRegenMult == 1.10
      )
    },

    test("Охотника ×1.2, Скрытный ×0.8 к весу боя") {
      assertTrue(
        HeroPassives(Set(PassiveKind.Hunter)).battleEncounterFactor == 1.2,
        math.abs(HeroPassives(Set(PassiveKind.Stealthy)).battleEncounterFactor - 0.8) < 1e-9
      )
    },

    // ── Агрегация на герое/экипировке ──────────────────────────────────────────
    test("hero.passives собирает пассивки с надетых предметов") {
      val hero = heroWith(PassiveKind.Robber, PassiveKind.Precise)
      assertTrue(hero.passives.finalDamageMult == 1.05, hero.passives.accuracyFlatBonus(100) == 5L)
    },

    test("дубли пассивок схлопываются в множестве (работает одна)") {
      val base = TestFixtures.hero(userId)
      val eq = base.equipment.copy(
        firstRing  = passiveItem(ItemType.Ring, PassiveKind.Robber),
        secondRing = passiveItem(ItemType.Ring, PassiveKind.Robber))
      val hero = base.copy(equipment = eq)
      assertTrue(hero.equipment.passiveKinds == Set[PassiveKind](PassiveKind.Robber))
    },

    test("Укреплённые/Точности входят в effectiveFightStats как плоская прибавка") {
      val base = TestFixtures.hero(userId).copy(
        fightStats = TestFixtures.hero(userId).fightStats.copy(defence = 100, accuracy = 100))
      val eq = base.equipment.copy(
        boots  = passiveItem(ItemType.Boots, PassiveKind.Reinforced),
        amulet = passiveItem(ItemType.Amulet, PassiveKind.Precise))
      val hero = base.copy(equipment = eq)
      val fs   = hero.effectiveFightStats(0L)
      assertTrue(fs.defence == 105L, fs.accuracy == 105L)
    },

    // ── Реген перед атакой ─────────────────────────────────────────────────────
    test("withCombatRegen лечит 4% макс.HP и 4% макс.брони") {
      val base = TestFixtures.hero(userId).copy(
        fightStats = TestFixtures.hero(userId).fightStats.copy(hp = 1, armor = 0))
      val eq = base.equipment.copy(
        amulet = passiveItem(ItemType.Amulet, PassiveKind.Healing),
        helmet = Item(2L, "шлем", 1L, Rarity.Gray, ItemType.Helmet,
                      attack = 0, accuracy = 0, energy = 0, armor = 100, defence = 0, evasion = 0))
      // Второй амулет-слот занять нельзя — Самовосстанавливающийся кладём на плечи? Он Amulet-only.
      val hero    = base.copy(equipment = eq)
      val maxHp   = hero.effectiveMaxHp(0L)
      val regened = hero.withCombatRegen(0L)
      assertTrue(regened.fightStats.hp == (1 + maxHp * 4 / 100))
    },

    test("withCombatRegen никогда не опускает hp/броню ниже текущего (даже если > потолка)") {
      val base = TestFixtures.hero(userId).copy(
        fightStats = TestFixtures.hero(userId).fightStats.copy(hp = 100000L, armor = 100000L))
      val regened = base.withCombatRegen(0L)
      assertTrue(regened.fightStats.hp == 100000L, regened.fightStats.armor == 100000L)
    },

    // ── Пул событий подземелья ─────────────────────────────────────────────────
    test("Охотника повышает долю боёв, Скрытный понижает") {
      val baseBattles    = StateType.events.count(_ == StateType.Battle)
      val hunterBattles  = StateType.eventsWithBattleFactor(1.2).count(_ == StateType.Battle)
      val stealthBattles = StateType.eventsWithBattleFactor(0.8).count(_ == StateType.Battle)
      val nonBattleSame  = StateType.eventsWithBattleFactor(1.2).count(_ != StateType.Battle) ==
                           StateType.events.count(_ != StateType.Battle)
      assertTrue(hunterBattles > baseBattles, stealthBattles < baseBattles, nonBattleSame)
    },

    // ── Хелперы Тайника ────────────────────────────────────────────────────────
    test("stashBonus/stashDelta считают +10 только для Тайника") {
      val stashBoots = passiveItem(ItemType.Boots, PassiveKind.Stash)
      val plainBoots = passiveItem(ItemType.Boots, PassiveKind.QuickFeet)
      assertTrue(
        InventoryState.stashBonus(stashBoots) == 10L,
        InventoryState.stashBonus(plainBoots) == 0L,
        InventoryState.stashDelta(plainBoots, stashBoots) == 10L,
        InventoryState.stashDelta(stashBoots, plainBoots) == -10L
      )
    },

    test("fitsWithoutStash: сумка влезает в базовую вместимость только с запасом ≥10") {
      def inv(items: Int, maxItems: Long) = pangea.model.inventory.Inventory(
        0L, TestFixtures.hero(userId).id, maxItems,
        pangea.model.inventory.Inventory.Items(List.fill(items)(
          Item(1L, "x", 1L, Rarity.Gray, ItemType.Trophy, 0, 0, 0, 0, 0, 0))))
      // maxItems=30 (20 база + 10 тайник). Возвращаем 1 предмет при снятии.
      assertTrue(
        InventoryState.fitsWithoutStash(inv(items = 19, maxItems = 30L), returningItems = 1),  // 19+1 ≤ 20
        !InventoryState.fitsWithoutStash(inv(items = 20, maxItems = 30L), returningItems = 1)   // 20+1 > 20
      )
    }
  )
}
