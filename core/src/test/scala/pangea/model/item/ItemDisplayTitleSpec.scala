package pangea.model.item

import pangea.domain.Rng
import pangea.generator.item.{GemGenerator, ItemGenerator, MaterialGenerator}
import zio.test._

/** Единый заголовок предмета для всех списков и экранов:
 *  «<кружок редкости> [Ур.N] <имя>». */
object ItemDisplayTitleSpec extends ZIOSpecDefault {

  private def item(name: String, lvl: Long, rarity: Rarity, itemType: ItemType,
                   details: ItemDetails = ItemDetails.Plain): Item =
    Item(1L, name, lvl, rarity, itemType,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = details)

  override def spec = suite("Item.displayTitle")(

    test("у пыли в описании только сам текст — без «Материал:» и повтора имени") {
      val dust  = MaterialGenerator.item(MaterialKind.TopazDust)
      val lines = dust.statsLines
      assertTrue(dust.displayTitle == "Топазная пыль") &&
      assertTrue(lines == List(MaterialKind.TopazDust.description)) &&
      assertTrue(!lines.exists(_.contains("Материал"))) &&
      assertTrue(!lines.exists(_.contains("Топазная пыль")))
    },

    test("материалу без описания строка «Материал: …» остаётся — иначе экран пуст") {
      assertTrue(MaterialKind.Mithril.description.isEmpty) &&
      assertTrue(MaterialGenerator.mithril.statsLines == List("Материал: Мифрил"))
    },

    test("снаряжение: кружок редкости, затем [Ур.N], затем имя без кружка") {
      val helmet = item("🔵 Хороший Шлем Рыцаря", 12L, Rarity.Blue, ItemType.Helmet)
      assertTrue(helmet.displayTitle == "🔵 [Ур.12] Хороший Шлем Рыцаря")
    },

    test("кружок берётся из редкости предмета и не дублируется в имени") {
      val titles = List(Rarity.Gray, Rarity.White, Rarity.Green, Rarity.Blue,
                        Rarity.Purple, Rarity.Violet, Rarity.Orange)
        .map(r => item(s"${r.emoji} Шлем Рыцаря", 7L, r, ItemType.Helmet).displayTitle)
      assertTrue(titles.forall(t => t.count(_ == '[') == 1)) &&
      assertTrue(titles.forall(_.endsWith("[Ур.7] Шлем Рыцаря"))) &&
      assertTrue(titles.head.startsWith(Rarity.Gray.emoji)) &&
      assertTrue(titles.last.startsWith(Rarity.Orange.emoji))
    },

    test("имя без кружка (трофей, тестовые предметы) → просто [Ур.N] Имя") {
      val trophy = item("Голова (Человек)", 5L, Rarity.Gray, ItemType.Trophy,
        ItemDetails.Trophy("Human", TrophyKind.Head))
      assertTrue(trophy.displayTitle == "[Ур.5] Голова (Человек)")
    },

    test("камни, карты клада и материалы уровня не показывают — только имя") {
      val gem  = GemGenerator.item(GemKind.Skull, 3)
      val map  = item("Карта клада", 9L, Rarity.Gray, ItemType.TreasureMap)
      val half = item("Половинка карты", 9L, Rarity.Gray, ItemType.TreasureMapHalf)
      val mat  = item("Слиток", 9L, Rarity.Gray, ItemType.Material)
      assertTrue(gem.displayTitle == gem.name) &&
      assertTrue(map.displayTitle == "Карта клада") &&
      assertTrue(half.displayTitle == "Половинка карты") &&
      assertTrue(mat.displayTitle == "Слиток") &&
      assertTrue(List(gem, map, half, mat).forall(!_.displayTitle.contains("Ур.")))
    },

    test("строка сравнения «надето» использует тот же заголовок") {
      val helmet = item("🟠 Идеальный Шлем Вождя", 30L, Rarity.Orange, ItemType.Helmet)
      assertTrue(helmet.equippedComparison("Сейчас надет")
        .startsWith("Сейчас надет: 🟠 [Ур.30] Идеальный Шлем Вождя"))
    },

    test("сгенерированные предметы: заголовок всегда «<кружок> [Ур.N] <имя>»") {
      val items = (1L to 200L)
        .map(s => ItemGenerator.createItemAtLevel(20L, Rarity.Violet, Rng(s))._1)
      assertTrue(items.nonEmpty) &&
      assertTrue(items.forall(i => i.displayTitle.startsWith(s"${i.rarity.emoji} [Ур.${i.lvl}] "))) &&
      // кружок остался ровно один — из имени он вырезан
      assertTrue(items.forall(i => i.displayTitle.indexOf(i.rarity.emoji) == i.displayTitle.lastIndexOf(i.rarity.emoji)))
    }
  )
}
