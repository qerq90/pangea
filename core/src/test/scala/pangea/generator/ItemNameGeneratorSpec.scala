package pangea.generator

import pangea.domain.Rng
import pangea.generator.item.ItemNameGenerator
import pangea.model.item.{ItemType, Rarity}
import zio.test._

object ItemNameGeneratorSpec extends ZIOSpecDefault {

  override def spec = suite("ItemNameGenerator")(

    test("Gray Weapon → имя без суффикса") {
      val (name, _) = ItemNameGenerator.generate(ItemType.Weapon, Rarity.Gray, Rng(42L))
      assertTrue(name == "Меч" || name == "Топор" || name == "Кинжал") &&
        assertTrue(!name.contains(" "))
    },

    test("Blue Weapon → имя с суффиксом 'воина'") {
      val (name, _) = ItemNameGenerator.generate(ItemType.Weapon, Rarity.Blue, Rng(42L))
      assertTrue(name.endsWith("воина"))
    },

    test("Orange Helmet → имя с суффиксом 'легенды'") {
      val (name, _) = ItemNameGenerator.generate(ItemType.Helmet, Rarity.Orange, Rng(1L))
      assertTrue(name.endsWith("легенды"))
    },

    test("разные seeds дают разные имена для одного типа") {
      val names = (1L to 20L).map(seed => ItemNameGenerator.generate(ItemType.Weapon, Rarity.Gray, Rng(seed))._1)
      assertTrue(names.distinct.size > 1)
    },

    test("ItemGenerator создаёт предметы с реальными именами") {
      import pangea.generator.item.ItemGenerator
      val (item, _) = ItemGenerator.createItem(5L, Rarity.Blue, Rng(999L))
      assertTrue(item.name != "?" && item.name.nonEmpty && !item.name.startsWith("default"))
    }
  )
}
