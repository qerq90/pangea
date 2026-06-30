package pangea.generator

import pangea.domain.Rng
import pangea.generator.loot.SchronGenerator
import pangea.model.item.{ItemType, TrophyKind, Rarity => ItemRarity}
import pangea.model.monster.Race
import zio.test._

object SchronGeneratorSpec extends ZIOSpecDefault {

  private val gearRarities: Set[ItemRarity] =
    Set(ItemRarity.Green, ItemRarity.Blue, ItemRarity.Purple, ItemRarity.Violet, ItemRarity.Orange)

  // прогон по множеству сидов
  private def rewards(race: Race, lvl: Long, dMin: Int, dMax: Int) =
    (1L to 500L).map(s => SchronGenerator.roll(race, lvl, dMin, dMax, Rng(s))._1)

  def spec = suite("SchronGeneratorSpec")(

    test("детерминизм: один seed → одинаковая награда") {
      val (a, _) = SchronGenerator.roll(Race.Orc, 20L, 2, 3, Rng(777L))
      val (b, _) = SchronGenerator.roll(Race.Orc, 20L, 2, 3, Rng(777L))
      assertTrue(a == b)
    },

    test("первый слот 100% → награда никогда не пустая") {
      val all = rewards(Race.Human, 30L, 2, 3)
      assertTrue(all.forall(r => r.items.nonEmpty || r.gold > 0L))
    },

    test("не больше двух категорий за раз, без повтора (2 слота, used)") {
      val all = rewards(Race.Elf, 30L, 2, 3)
      // максимум: 1 gear + 1 trophy ИЛИ предмет + золото — суммарно ≤ 2 категорий
      assertTrue(all.forall(r => r.items.size + (if (r.gold > 0L) 1 else 0) <= 2))
    },

    test("дублоны выпадают только вместе с золотом и в заданном диапазоне") {
      val all = rewards(Race.Gnome, 10L, 2, 3)
      assertTrue(all.forall(r => (r.doubloons > 0L) == (r.gold > 0L))) &&
      assertTrue(all.forall(r => r.gold <= 0L || (r.doubloons >= 2L && r.doubloons <= 3L)))
    },

    test("золото около lvl×8 ±20%") {
      val golds = rewards(Race.Goblin, 10L, 1, 2).map(_.gold).filter(_ > 0L)
      // база = 80, разброс 80..120% → [64, 96]
      assertTrue(golds.nonEmpty) &&
      assertTrue(golds.forall(g => g >= 64L && g <= 96L))
    },

    test("экипировка: редкости только Green/Blue/Purple/Violet/Orange, не трофей") {
      val gears = rewards(Race.Demon, 50L, 2, 3)
        .flatMap(_.items)
        .filter(_.itemType != ItemType.Trophy)
      assertTrue(gears.nonEmpty) &&
      assertTrue(gears.forall(i => gearRarities.contains(i.rarity))) &&
      assertTrue(gears.forall(i => i.lvl >= 1L && i.lvl <= 51L))
    },

    test("трофей: только Реликвия/Талисман, хранит расу и уровень") {
      val trophies = rewards(Race.Khajiit, 12L, 2, 3)
        .flatMap(_.items)
        .filter(_.itemType == ItemType.Trophy)
      assertTrue(trophies.nonEmpty) &&
      assertTrue(trophies.forall(t => t.trophyKind.exists(k => k == TrophyKind.Relic || k == TrophyKind.Talisman))) &&
      assertTrue(trophies.forall(_.race.contains(Race.Khajiit.entryName))) &&
      assertTrue(trophies.forall(_.name.contains("Каджит"))) &&
      assertTrue(trophies.forall(_.lvl == 12L))
    },

    test("оба вида трофея реально встречаются на множестве сидов") {
      val kinds = rewards(Race.Human, 20L, 2, 3)
        .flatMap(_.items)
        .flatMap(_.trophyKind)
        .toSet
      assertTrue(kinds.contains(TrophyKind.Relic)) &&
      assertTrue(kinds.contains(TrophyKind.Talisman))
    }
  )
}
