package pangea.generator

import pangea.domain.Rng
import pangea.generator.loot.LootGenerator
import pangea.generator.loot.LootGenerator.LootDrop
import pangea.model.item.ItemType
import pangea.model.monster.{Race, Rarity}
import zio.test._

object LootGeneratorSpec extends ZIOSpecDefault {

  private def category(d: LootDrop): String = d match {
    case LootDrop.Gear(_)       => "gear"
    case LootDrop.Trophy(_)     => "trophy"
    case LootDrop.Gold(_, true) => "goldPile"
    case LootDrop.Gold(_, _)    => "goldSmall"
  }

  def spec = suite("LootGeneratorSpec")(

    test("детерминизм: один seed → одинаковый дроп") {
      val (a, _) = LootGenerator.roll(Rarity.Legendary, Race.Orc, 20L, Rng(123L))
      val (b, _) = LootGenerator.roll(Rarity.Legendary, Race.Orc, 20L, Rng(123L))
      assertTrue(a == b)
    },

    test("нет повторных категорий за один бой (ответ 4)") {
      val cases = (1L to 200L).map { s =>
        val (drops, _) = LootGenerator.roll(Rarity.Legendary, Race.Human, 30L, Rng(s))
        val cats       = drops.map(category)
        cats.distinct.size == cats.size
      }
      assertTrue(cases.forall(identity))
    },

    test("обычный моб: не больше одного дропа за бой") {
      val cases = (1L to 200L).map { s =>
        LootGenerator.roll(Rarity.Common, Race.Goblin, 5L, Rng(s))._1.size <= 1
      }
      assertTrue(cases.forall(identity))
    },

    test("легендарный моб роняет лут хотя бы иногда (первые два слота 100%)") {
      val any = (1L to 50L).exists(s => LootGenerator.roll(Rarity.Legendary, Race.Demon, 40L, Rng(s))._1.nonEmpty)
      assertTrue(any)
    },

    test("трофей хранит расу и уровень убийства") {
      val trophy = (1L to 300L).iterator
        .flatMap(s => LootGenerator.roll(Rarity.Rare, Race.Khajiit, 12L, Rng(s))._1)
        .collectFirst { case LootDrop.Trophy(i) => i }
      assertTrue(trophy.exists(i =>
        i.itemType == ItemType.Trophy &&
        i.race.contains(Race.Khajiit.entryName) &&
        i.lvl == 12L &&
        i.name.contains("Каджит")))
    },

    test("золото всегда положительное и около lvl×4 ±20%") {
      val golds = (1L to 300L).iterator
        .flatMap(s => LootGenerator.roll(Rarity.Mythical, Race.Gnome, 10L, Rng(s))._1)
        .collect { case LootDrop.Gold(a, _) => a }
        .toList
      // база = 40, разброс 80..120% → [32, 48]
      assertTrue(golds.nonEmpty) &&
      assertTrue(golds.forall(g => g >= 32L && g <= 48L))
    },

    test("экипировка генерируется с разбросом уровня из ItemGenerator (killLevel-6 .. killLevel+1)") {
      val gears = (1L to 300L).iterator
        .flatMap(s => LootGenerator.roll(Rarity.Rare, Race.Elf, 50L, Rng(s))._1)
        .collect { case LootDrop.Gear(i) => i }
        .toList
      assertTrue(gears.nonEmpty) &&
      assertTrue(gears.forall(i => i.lvl >= 1L && i.lvl <= 51L)) &&
      assertTrue(gears.forall(_.itemType != ItemType.Trophy))
    }
  )
}
