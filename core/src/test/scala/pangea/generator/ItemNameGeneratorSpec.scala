package pangea.generator

import pangea.domain.Rng
import pangea.generator.item.ItemNameGenerator
import pangea.model.item.{ItemType, Rarity}
import zio.test._

object ItemNameGeneratorSpec extends ZIOSpecDefault {

  private val titles = Set(
    "рыцаря", "командира", "дворянина", "аристократа", "гвардейца",
    "кавалериста", "стражника", "охранника", "повара", "стрелка",
    "разведчика", "оруженосца", "бойца", "вождя"
  )

  override def spec = suite("ItemNameGenerator")(

    test("формат: кружок + прилагательное + тип + титул") {
      val (name, _) = ItemNameGenerator.generate(ItemType.Weapon, Rarity.Gray, Rng(42L))
      val parts = name.split(' ').toList
      assertTrue(parts.length == 4) &&
        assertTrue(parts.head == "⚫") &&
        assertTrue(titles.contains(parts(3)))
    },

    test("Blue Weapon начинается с синего кружка") {
      val (name, _) = ItemNameGenerator.generate(ItemType.Weapon, Rarity.Blue, Rng(42L))
      assertTrue(name.startsWith("🔵"))
    },

    test("Orange Helmet начинается с оранжевого кружка") {
      val (name, _) = ItemNameGenerator.generate(ItemType.Helmet, Rarity.Orange, Rng(1L))
      assertTrue(name.startsWith("🟠"))
    },

    test("прилагательное и тип — с заглавной, титул — с маленькой") {
      val (name, _) = ItemNameGenerator.generate(ItemType.ChestPlate, Rarity.Green, Rng(7L))
      val parts = name.split(' ').toList
      assertTrue(parts(1).head.isUpper) &&
        assertTrue(parts(2).head.isUpper) &&
        assertTrue(parts(3).head.isLower)
    },

    test("разные seeds дают разные имена для одного типа") {
      val names = (1L to 20L).map(seed =>
        ItemNameGenerator.generate(ItemType.Weapon, Rarity.Gray, Rng(seed))._1)
      assertTrue(names.distinct.size > 1)
    },

    test("ItemGenerator создаёт предметы с реальными именами") {
      import pangea.generator.item.ItemGenerator
      val (item, _) = ItemGenerator.createItem(5L, Rarity.Blue, Rng(999L))
      assertTrue(item.name.nonEmpty && item.name != "?" && !item.name.startsWith("default"))
    }
  )
}
