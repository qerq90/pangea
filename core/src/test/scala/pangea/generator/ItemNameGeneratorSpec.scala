package pangea.generator

import pangea.domain.Rng
import pangea.generator.item.ItemNameGenerator
import pangea.model.item.{ItemType, Rarity}
import zio.test._

object ItemNameGeneratorSpec extends ZIOSpecDefault {

  // Слова из словарей титула/расы — родительный падеж, с заглавной.
  private val titlesAndRaces = Set(
    "Рыцаря", "Командира", "Дворянина", "Аристократа", "Гвардейца",
    "Кавалериста", "Стражника", "Охранника", "Повара", "Стрелка",
    "Разведчика", "Оруженосца", "Бойца", "Вождя",
    "Эльфа", "Орка", "Гоблина", "Мурлока", "Демона", "Гнома",
    "Аргонианца", "Человека", "Каджита", "Прохвоста", "Троглодита", "Скелета"
  )

  override def spec = suite("ItemNameGenerator")(

    test("формат: ровно 4 слова — качество, тип, титул, раса") {
      val (name, _) = ItemNameGenerator.generate(ItemType.Weapon, Rarity.Gray, Rng(42L))
      val parts = name.split(' ').toList
      assertTrue(parts.length == 4) &&
        assertTrue(titlesAndRaces.contains(parts(2))) &&
        assertTrue(titlesAndRaces.contains(parts(3)))
    },

    test("Blue Weapon → качество из синего пула (Хороший/Надёжный/...) — м.р., с типом-Masc слова совпадают") {
      // Weapon содержит и м.р. (Меч/Топор/Кинжал) и ср.р. (Копьё) — поэтому проверяем,
      // что первое слово оканчивается на одно из ожидаемых окончаний для синей редкости.
      val blueRoots = Set("Хорош", "Надёжн", "Качественн", "Прочн", "Удобн",
                          "Стильн", "Добротн", "Функциональн", "Долговечн")
      val (name, _) = ItemNameGenerator.generate(ItemType.Weapon, Rarity.Blue, Rng(42L))
      val adj = name.split(' ').head
      assertTrue(blueRoots.exists(adj.startsWith))
    },

    test("Orange Helmet → качество из оранжевого пула (Лучший/Идеальный/...)") {
      val orangeRoots = Set("Лучш", "Идеальн", "Безупречн", "Эталонн", "Непревзойдённ", "Легендарн")
      val (name, _) = ItemNameGenerator.generate(ItemType.Helmet, Rarity.Orange, Rng(1L))
      val adj = name.split(' ').head
      assertTrue(orangeRoots.exists(adj.startsWith))
    },

    test("согласование рода: Кираса (ж.р.) → прилагательное на '-ая'") {
      // Зацикливаем по seeds, пока не получим базу 'Кираса', и проверяем согласование.
      val sample = (1L to 200L).iterator
        .map(seed => ItemNameGenerator.generate(ItemType.ChestPlate, Rarity.Gray, Rng(seed))._1)
        .find(_.split(' ')(1) == "Кираса")
      assertTrue(sample.exists(_.split(' ').head.endsWith("ая")))
    },

    test("согласование рода: Сапоги (мн.ч.) → прилагательное на '-ые'/'-ие'") {
      val sample = (1L to 200L).iterator
        .map(seed => ItemNameGenerator.generate(ItemType.Boots, Rarity.Gray, Rng(seed))._1)
        .find(_.split(' ')(1) == "Сапоги")
      assertTrue(sample.exists { n =>
        val adj = n.split(' ').head
        adj.endsWith("ые") || adj.endsWith("ие")
      })
    },

    test("разные seeds дают разные имена для одного типа") {
      val names = (1L to 20L).map(seed =>
        ItemNameGenerator.generate(ItemType.Weapon, Rarity.Gray, Rng(seed))._1)
      assertTrue(names.distinct.size > 1)
    },

    test("ItemGenerator создаёт предметы с реальными именами") {
      import pangea.generator.item.ItemGenerator
      val (item, _) = ItemGenerator.createItem(5L, Rarity.Blue, Rng(999L))
      assertTrue(item.name != "?" && item.name.nonEmpty && !item.name.startsWith("default"))
    }
  )
}
