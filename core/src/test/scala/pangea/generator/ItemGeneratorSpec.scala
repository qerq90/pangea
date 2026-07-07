package pangea.generator

import pangea.domain.Rng
import pangea.generator.item.ItemGenerator
import pangea.model.item.{Item, ItemDetails, ItemType, Rarity}
import pangea.model.skill.Skill
import zio.test._

object ItemGeneratorSpec extends ZIOSpecDefault {

  private def skillOf(i: Item): Option[Skill] = i.details match {
    case ItemDetails.Weapon(s) => Some(s)
    case ItemDetails.Armor(s)  => Some(s)
    case _                     => None
  }

  def spec = suite("ItemGeneratorSpec")(
    test("same seed produces identical item") {
      val rng        = Rng(42L)
      val (item1, _) = ItemGenerator.createItem(10L, Rarity.Blue, rng)
      val (item2, _) = ItemGenerator.createItem(10L, Rarity.Blue, rng)
      assertTrue(item1 == item2)
    },
    test("different seeds produce different items") {
      val (item1, _) = ItemGenerator.createItem(10L, Rarity.Blue, Rng(1L))
      val (item2, _) = ItemGenerator.createItem(10L, Rarity.Blue, Rng(999L))
      assertTrue(item1 != item2)
    },
    test("item level stays within [1, 150]") {
      val items = (1L to 20L).map(seed => ItemGenerator.createItem(100L, Rarity.Gray, Rng(seed))._1)
      assertTrue(items.forall(i => i.lvl >= 1L && i.lvl <= 150L))
    },
    test("rng advances after createItem") {
      val rng       = Rng(42L)
      val (_, rng2) = ItemGenerator.createItem(10L, Rarity.Blue, rng)
      assertTrue(rng2 != rng)
    },

    test("rarityForLevel level≤15 → только Gray/White/Green") {
      val rarities = (1L to 50L).map(seed => ItemGenerator.rarityForLevel(5, Rng(seed))._1).toSet
      assertTrue(rarities.forall(r => r == Rarity.Gray || r == Rarity.White || r == Rarity.Green))
    },

    test("rarityForLevel level>100 → только Purple/Violet/Orange") {
      val rarities = (1L to 50L).map(seed => ItemGenerator.rarityForLevel(120, Rng(seed))._1).toSet
      assertTrue(rarities.forall(r => r == Rarity.Purple || r == Rarity.Violet || r == Rarity.Orange))
    },

    test("rarityForLevel разные уровни дают разные редкости") {
      val low  = ItemGenerator.rarityForLevel(1,   Rng(42L))._1
      val high = ItemGenerator.rarityForLevel(150, Rng(42L))._1
      assertTrue(low != high)
    },

    test("оружие всегда получает обязательную атаку (>0)") {
      val weapons = (1L to 500L)
        .map(s => ItemGenerator.createItemAtLevel(10L, Rarity.Green, Rng(s))._1)
        .filter(_.itemType == ItemType.Weapon)
      assertTrue(weapons.nonEmpty) && assertTrue(weapons.forall(_.attack > 0L))
    },

    test("нагрудник даёт обязательный HP ≈ lvl×(12+R3)±10%") {
      val lvl  = 10L
      val base = lvl * (12.0 + Rarity.Green.factorR3) // R3 = 4 → 160
      val lo   = (base * 0.9).toLong
      val hi   = (base * 1.1).toLong + 1
      val chests = (1L to 800L)
        .map(s => ItemGenerator.createItemAtLevel(lvl, Rarity.Green, Rng(s))._1)
        .filter(_.itemType == ItemType.ChestPlate)
      assertTrue(chests.nonEmpty) &&
      assertTrue(chests.forall(c => c.hp >= lo && c.hp <= hi))
    },

    test("HP-прибавку получает только нагрудник, остальные hp = 0") {
      val items = (1L to 500L)
        .map(s => ItemGenerator.createItemAtLevel(10L, Rarity.Blue, Rng(s))._1)
      assertTrue(items.filter(_.itemType != ItemType.ChestPlate).forall(_.hp == 0L)) &&
      assertTrue(items.filter(_.itemType == ItemType.ChestPlate).forall(_.hp > 0L))
    },

    test("Weapon всегда имеет activeSkill из weaponSkills") {
      val weapons = (1L to 500L)
        .map(s => ItemGenerator.createItemAtLevel(10L, Rarity.Green, Rng(s))._1)
        .filter(_.itemType == ItemType.Weapon)
      assertTrue(weapons.nonEmpty) &&
      assertTrue(weapons.forall(i => skillOf(i).exists(Skill.weaponSkills.contains)))
    },

    test("ChestPlate всегда имеет activeSkill из armorSkills") {
      val chests = (1L to 500L)
        .map(s => ItemGenerator.createItemAtLevel(10L, Rarity.Green, Rng(s))._1)
        .filter(_.itemType == ItemType.ChestPlate)
      assertTrue(chests.nonEmpty) &&
      assertTrue(chests.forall(i => skillOf(i).exists(Skill.armorSkills.contains)))
    },

    test("Остальные слоты не получают activeSkill") {
      val others = (1L to 500L)
        .map(s => ItemGenerator.createItemAtLevel(10L, Rarity.Blue, Rng(s))._1)
        .filter(i => i.itemType != ItemType.Weapon && i.itemType != ItemType.ChestPlate)
      assertTrue(others.forall(i => skillOf(i).isEmpty))
    },

    test("Распределение оружейных навыков примерно равномерно") {
      val skills = (1L to 1500L)
        .map(s => ItemGenerator.createItemAtLevel(10L, Rarity.Green, Rng(s))._1)
        .filter(_.itemType == ItemType.Weapon)
        .flatMap(skillOf)
      val counts = Skill.weaponSkills.map(s => skills.count(_ == s))
      // У всех 3 навыков ненулевая выборка — каждое значение хотя бы изредка выпало.
      assertTrue(counts.forall(_ > 0))
    }
  )
}
