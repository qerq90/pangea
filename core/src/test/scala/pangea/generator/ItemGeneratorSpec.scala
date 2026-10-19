package pangea.generator

import pangea.domain.Rng
import pangea.generator.item.ItemGenerator
import pangea.model.item.{Grip, Item, ItemDetails, ItemType, PassiveKind, Rarity}
import pangea.model.skill.Skill.Counterstrike
import pangea.model.skill.Skill
import zio.test._

object ItemGeneratorSpec extends ZIOSpecDefault {

  private def skillOf(i: Item): Option[Skill] = i.details match {
    case ItemDetails.Weapon(s) => Some(s)
    case ItemDetails.Armor(s)  => Some(s)
    case _                     => None
  }

  // Доля предметов заданного типа с пассивкой при данной редкости (по выборке).
  // Кольцо — одна вещь из двадцати, поэтому выборка большая: иначе шум.
  private def passiveRate(itemType: ItemType, rarity: Rarity): Double = {
    val items = (1L to 8000L)
      .map(s => ItemGenerator.createItemAtLevel(10L, rarity, Rng(s))._1)
      .filter(_.itemType == itemType)
    items.count(_.passive.isDefined).toDouble / items.size.max(1)
  }

  // Распределение числа гнёзд (число гнёзд -> доля выборки) для конкретного
  // слота и редкости. Слот форсируем, чтобы набрать выборку по оружию/броне
  // отдельно — таблицы гнёзд у них разные. Хват оружия тоже можно задать:
  // двуручному гнездо сверх таблицы.
  private def socketDist(itemType: ItemType, rarity: Rarity, samples: Long = 3000L,
                         grip: Option[Grip] = None): Map[Int, Double] =
    (1L to samples)
      .map(s => ItemGenerator.createItemOfType(itemType, 10L, rarity, Rng(s), grip)._1.sockets.size)
      .groupBy(identity)
      .map { case (n, xs) => n -> xs.size.toDouble / samples }

  private def near(actual: Double, expected: Double, tol: Double = 0.05): Boolean =
    math.abs(actual - expected) <= tol

  // Слоты «не-оружие», по которым проверяем броневую таблицу гнёзд.
  private val gearSlots =
    List(ItemType.Helmet, ItemType.ChestPlate, ItemType.Ring, ItemType.Boots, ItemType.Amulet)

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

    test("находка на полу — только серый, белый или зелёный, на любом этаже") {
      val rarities = (1L to 300L).map(seed => ItemGenerator.foundItemRarity(Rng(seed))._1).toSet
      assertTrue(rarities.subsetOf(Set[Rarity](Rarity.Gray, Rarity.White, Rarity.Green))) &&
      assertTrue(rarities.size == 3)
    },

    test("доли находки: 60% серый, 30% белый, 10% зелёный") {
      val n     = 3000
      val all   = (1L to n.toLong).map(seed => ItemGenerator.foundItemRarity(Rng(seed))._1)
      def share(r: Rarity) = all.count(_ == r).toDouble / n
      assertTrue(ItemGenerator.FoundGrayPct == 60L) &&
      assertTrue(ItemGenerator.FoundWhitePct == 30L) &&
      assertTrue(math.abs(share(Rarity.Gray)  - 0.60) < 0.05) &&
      assertTrue(math.abs(share(Rarity.White) - 0.30) < 0.05) &&
      assertTrue(math.abs(share(Rarity.Green) - 0.10) < 0.05)
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

    test("Weapon всегда имеет activeSkill из weaponSkills, лук и арбалет — из rangedSkills, а парирующий кинжал — Контрудар") {
      val weapons = (1L to 2000L)
        .map(s => ItemGenerator.createItemAtLevel(10L, Rarity.Green, Rng(s))._1)
        .filter(_.itemType == ItemType.Weapon)
      val (parrying, others) = weapons.partition(_.grip.contains(Grip.Parrying))
      val (ranged, rest)     = others.partition(_.ranged)
      assertTrue(rest.nonEmpty && parrying.nonEmpty && ranged.nonEmpty) &&
      assertTrue(rest.forall(i => skillOf(i).exists(Skill.weaponSkills.contains))) &&
      assertTrue(ranged.forall(i => skillOf(i).exists(Skill.rangedSkills.contains))) &&
      assertTrue(parrying.forall(_.details == ItemDetails.HandSkill(Counterstrike)))
    },

    test("ChestPlate всегда имеет activeSkill из armorSkills") {
      val chests = (1L to 500L)
        .map(s => ItemGenerator.createItemAtLevel(10L, Rarity.Green, Rng(s))._1)
        .filter(_.itemType == ItemType.ChestPlate)
      assertTrue(chests.nonEmpty) &&
      assertTrue(chests.forall(i => skillOf(i).exists(Skill.armorSkills.contains)))
    },

    test("пустых вещей не бывает: даже серая первого уровня несёт хоть одну характеристику") {
      val items = (1L to 3000L).map(s => ItemGenerator.createItemAtLevel(1L, Rarity.Gray, Rng(s))._1)
      val empty = items.filter(i => i.attack + i.accuracy + i.energy + i.armor + i.defence + i.evasion + i.hp <= 0L)
      // и лёгкие (меткость), и тяжёлые (броня) — главный стат не ниже единицы
      val light = items.filter(i => i.accuracy > 0L && i.armor == 0L)
      val heavy = items.filter(i => i.armor > 0L)
      assertTrue(empty.isEmpty && light.nonEmpty && heavy.nonEmpty)
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
    },

    // ── Пассивки ──────────────────────────────────────────────────────────────
    test("оружие/нагрудник/пояс/фляга пассивок не получают") {
      val noPassive = List(ItemType.Weapon, ItemType.ChestPlate, ItemType.Belt, ItemType.Flask)
      val items = (1L to 1500L)
        .map(s => ItemGenerator.createItemAtLevel(10L, Rarity.Orange, Rng(s))._1)
        .filter(i => noPassive.contains(i.itemType))
      assertTrue(items.nonEmpty) && assertTrue(items.forall(_.passive.isEmpty))
    },

    test("фиол./пурпур/оранж всегда дают пассивку носимым слотам") {
      val rates = List(Rarity.Purple, Rarity.Violet, Rarity.Orange).map(r => passiveRate(ItemType.Amulet, r))
      assertTrue(rates.forall(_ == 1.0))
    },

    test("шанс пассивки растёт с редкостью (серый ~10% < зелёный ~40% < синий ~80%)") {
      val gray  = passiveRate(ItemType.Ring, Rarity.Gray)
      val green = passiveRate(ItemType.Ring, Rarity.Green)
      val blue  = passiveRate(ItemType.Ring, Rarity.Blue)
      assertTrue(gray > 0.03 && gray < 0.18) &&
        assertTrue(green > 0.30 && green < 0.50) &&
        assertTrue(blue > 0.70 && blue < 0.90) &&
        assertTrue(gray < green && green < blue)
    },

    test("пассивка предмета всегда из пула, допустимого для его слота") {
      val items = (1L to 2000L)
        .map(s => ItemGenerator.createItemAtLevel(10L, Rarity.Orange, Rng(s))._1)
        .filter(_.passive.isDefined)
      assertTrue(items.nonEmpty) &&
        assertTrue(items.forall(i => i.passive.exists(_.eligibleSlots.contains(i.itemType))))
    },

    test("на кольцах встречаются только кольцевые пассивки (Ювелир/Мародёр/Разбойник/Целитель)") {
      val ringPassives = (1L to 2000L)
        .map(s => ItemGenerator.createItemAtLevel(10L, Rarity.Orange, Rng(s))._1)
        .filter(_.itemType == ItemType.Ring)
        .flatMap(_.passive)
        .toSet
      val expected = Set[PassiveKind](
        PassiveKind.Jeweler, PassiveKind.Marauder, PassiveKind.Robber, PassiveKind.Healer)
      assertTrue(ringPassives.nonEmpty) && assertTrue(ringPassives.subsetOf(expected))
    },

    // ── Гнёзда под камни ──────────────────────────────────────────────────────
    test("серый/белый/зелёный — без гнёзд и у оружия, и у остального снаряжения") {
      val rarities = List(Rarity.Gray, Rarity.White, Rarity.Green)
      val slots    = ItemType.Weapon :: gearSlots
      assertTrue(rarities.forall(r => slots.forall(t => socketDist(t, r, 300L).keySet == Set(0))))
    },

    test("многогнёздность — только у оружия: остальным слотам не больше одного гнезда") {
      val rarities = List(Rarity.Blue, Rarity.Purple, Rarity.Violet, Rarity.Orange)
      assertTrue(gearSlots.forall(t => rarities.forall(r => socketDist(t, r, 300L).keys.forall(_ <= 1))))
    },

    test("синее одноручное ОРУЖИЕ: 20% ноль / 30% одно / 30% два / 20% три") {
      val d = socketDist(ItemType.Weapon, Rarity.Blue, grip = Some(Grip.OneHanded))
      assertTrue(near(d.getOrElse(0, 0.0), 0.20)) &&
      assertTrue(near(d.getOrElse(1, 0.0), 0.30)) &&
      assertTrue(near(d.getOrElse(2, 0.0), 0.30)) &&
      assertTrue(near(d.getOrElse(3, 0.0), 0.20))
    },

    test("фиолетовое одноручное ОРУЖИЕ: 10% ноль / 30% одно / 30% два / 30% три") {
      val d = socketDist(ItemType.Weapon, Rarity.Purple, grip = Some(Grip.OneHanded))
      assertTrue(near(d.getOrElse(0, 0.0), 0.10)) &&
      assertTrue(near(d.getOrElse(1, 0.0), 0.30)) &&
      assertTrue(near(d.getOrElse(2, 0.0), 0.30)) &&
      assertTrue(near(d.getOrElse(3, 0.0), 0.30))
    },

    test("пурпурное и легендарное одноручное ОРУЖИЕ: только 2 или 3 гнезда, поровну") {
      val dists = List(Rarity.Violet, Rarity.Orange).map(socketDist(ItemType.Weapon, _, grip = Some(Grip.OneHanded)))
      assertTrue(dists.forall(_.keySet == Set(2, 3))) &&
      assertTrue(dists.forall(d => near(d(2), 0.50))) &&
      assertTrue(dists.forall(d => near(d(3), 0.50)))
    },

    test("двуручному — гнездо сверх таблицы, до четырёх; серое, белое и зелёное — без гнёзд") {
      val blue   = socketDist(ItemType.Weapon, Rarity.Blue, grip = Some(Grip.TwoHanded))
      val purple = socketDist(ItemType.Weapon, Rarity.Purple, grip = Some(Grip.TwoHanded))
      val top    = List(Rarity.Violet, Rarity.Orange).map(socketDist(ItemType.Weapon, _, grip = Some(Grip.TwoHanded)))
      val low    = List(Rarity.Gray, Rarity.White, Rarity.Green).map(socketDist(ItemType.Weapon, _, 300L, Some(Grip.TwoHanded)))
      assertTrue(blue.keySet == Set(1, 2, 3, 4) && near(blue(1), 0.20) && near(blue(4), 0.20)) &&
      assertTrue(purple.keySet == Set(1, 2, 3, 4) && near(purple(1), 0.10) && near(purple(4), 0.30)) &&
      assertTrue(top.forall(_.keySet == Set(3, 4)) && top.forall(d => near(d(4), 0.50))) &&
      assertTrue(low.forall(_.keySet == Set(0)))
    },

    test("синее не-оружие: 60% ноль / 40% одно") {
      val d = socketDist(ItemType.Helmet, Rarity.Blue)
      assertTrue(d.keySet == Set(0, 1)) &&
      assertTrue(near(d(0), 0.60)) &&
      assertTrue(near(d(1), 0.40))
    },

    test("фиолетовое не-оружие: 10% ноль / 90% одно") {
      val d = socketDist(ItemType.Helmet, Rarity.Purple)
      assertTrue(d.keySet == Set(0, 1)) &&
      assertTrue(near(d(0), 0.10)) &&
      assertTrue(near(d(1), 0.90))
    },

    test("пурпурное и легендарное не-оружие: всегда ровно одно гнездо") {
      val dists = List(Rarity.Violet, Rarity.Orange).flatMap(r => gearSlots.map(socketDist(_, r, 300L)))
      assertTrue(dists.forall(_.keySet == Set(1)))
    },

    test("атака падает только на оружие — ни шлемов, ни колец с прибавкой к урону") {
      val items = (1L to 4000L).toList.flatMap { seed =>
        Rarity.values.toList.map(r => ItemGenerator.createItemAtLevel(20L, r, Rng(seed))._1)
      }
      val withAttack = items.filter(_.attack > 0)
      assertTrue(items.size > 1000) &&
      // атака есть — значит это оружие, и ничто иное
      assertTrue(withAttack.forall(_.itemType == ItemType.Weapon)) &&
      // у оружия она есть всегда: обязательная прибавка никуда не делась
      assertTrue(items.filter(_.itemType == ItemType.Weapon).forall(_.attack > 0)) &&
      // прочие слоты не остались пустыми: главный стат им достался другой
      assertTrue(items.filterNot(_.itemType == ItemType.Weapon)
        .forall(i => i.armor > 0 || i.defence > 0 || i.accuracy > 0 || i.evasion > 0 || i.hp > 0))
    },

    test("пересборка в кубе тоже не вешает атаку на броню") {
      val slots = List(ItemType.Helmet, ItemType.ChestPlate, ItemType.Ring, ItemType.Boots,
        ItemType.Amulet, ItemType.Belt, ItemType.Gloves, ItemType.Pants)
      val gear = (1L to 400L).toList.flatMap { seed =>
        slots.map(t => ItemGenerator.createItemOfType(t, 20L, Rarity.Orange, Rng(seed))._1)
      }
      val weapons = (1L to 200L).toList
        .map(seed => ItemGenerator.createItemOfType(ItemType.Weapon, 20L, Rarity.Orange, Rng(seed))._1)
      assertTrue(gear.forall(_.attack == 0)) &&
      assertTrue(weapons.forall(_.attack > 0))
    }
  )
}
