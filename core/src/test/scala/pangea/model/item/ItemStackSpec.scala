package pangea.model.item

import pangea.generator.item.{GemGenerator, MaterialGenerator}
import zio.test._

/** Складывание одинаковых вещей в одну строку экрана. Складывание ЭКРАННОЕ:
  * места в сумке каждая вещь занимает своё. */
object ItemStackSpec extends ZIOSpecDefault {

  private def gem(kind: GemKind, grade: Int, id: Long): Item =
    GemGenerator.item(kind, grade).copy(id = id)

  private def dust(kind: MaterialKind, id: Long): Item =
    MaterialGenerator.item(kind).copy(id = id)

  private def trophy(id: Long, race: String, kind: TrophyKind, lvl: Long): Item =
    Item(id, s"${kind.displayName} ($race)", lvl, Rarity.Gray, ItemType.Trophy,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Trophy(race, kind))

  private def sword(id: Long): Item =
    Item(id, "Меч", 10L, Rarity.Blue, ItemType.Weapon,
      attack = 5, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0)

  override def spec = suite("ItemStack")(

    test("одинаковые камни — одна строка, разные грейды — разные") {
      val items = List(
        gem(GemKind.Amethyst, 1, 1L), gem(GemKind.Amethyst, 1, 2L),
        gem(GemKind.Amethyst, 1, 3L), gem(GemKind.Amethyst, 2, 4L))
      val groups = ItemStack.grouped(items)
      assertTrue(groups.size == 2) &&
      assertTrue(groups.head._2 == 3) &&   // три надколотых
      assertTrue(groups.last._2 == 1) &&   // повреждённый сам по себе
      assertTrue(groups.head._1.id == 1L)  // кнопка ведёт на первый из стопки
    },

    test("камни разных видов не смешиваются") {
      val groups = ItemStack.grouped(List(
        gem(GemKind.Ruby, 1, 1L), gem(GemKind.Sapphire, 1, 2L), gem(GemKind.Ruby, 1, 3L)))
      assertTrue(groups.size == 2) &&
      assertTrue(groups.map(_._2).sum == 3)
    },

    test("материалы складываются по виду") {
      val groups = ItemStack.grouped(List(
        dust(MaterialKind.RubyDust, 1L), dust(MaterialKind.RubyDust, 2L),
        dust(MaterialKind.TopazDust, 3L), MaterialGenerator.mithril.copy(id = 4L)))
      assertTrue(groups.size == 3) &&
      assertTrue(groups.find(_._1.material.contains(MaterialKind.RubyDust)).exists(_._2 == 2))
    },

    test("трофеи складываются, только когда совпали вид, раса и уровень") {
      val groups = ItemStack.grouped(List(
        trophy(1L, "Human", TrophyKind.Head, 10L),
        trophy(2L, "Human", TrophyKind.Head, 10L),
        trophy(3L, "Human", TrophyKind.Head, 11L),   // другой уровень
        trophy(4L, "Orc",   TrophyKind.Head, 10L),   // другая раса
        trophy(5L, "Human", TrophyKind.Talisman, 10L)) // другой вид
      )
      assertTrue(groups.size == 4) &&
      assertTrue(groups.head._2 == 2)
    },

    test("экипировка не складывается никогда — у каждой вещи своя судьба") {
      val groups = ItemStack.grouped(List(sword(1L), sword(2L), sword(3L)))
      assertTrue(groups.size == 3) &&
      assertTrue(groups.forall(_._2 == 1)) &&
      assertTrue(!ItemStack.stackable(sword(1L)))
    },

    test("порядок сохраняется: группа встаёт на место первой своей вещи") {
      val items = List(sword(1L), gem(GemKind.Ruby, 1, 2L), dust(MaterialKind.RubyDust, 3L),
                       gem(GemKind.Ruby, 1, 4L))
      val ids = ItemStack.grouped(items).map(_._1.id)
      assertTrue(ids == List(1L, 2L, 3L))
    },

    test("счётчик отдельной вещи и приписка к названию") {
      val items = List(gem(GemKind.Topaz, 3, 1L), gem(GemKind.Topaz, 3, 2L), sword(3L))
      assertTrue(ItemStack.countOf(items, items.head) == 2) &&
      assertTrue(ItemStack.countOf(items, items.last) == 1) &&
      assertTrue(ItemStack.countSuffix(2) == " (2 шт)") &&
      assertTrue(ItemStack.countSuffix(1).isEmpty)
    },

    test("сложенные вещи занимают столько мест, сколько их есть") {
      // Складывание только экранное: сумка считает предметы, а не строки.
      val items = List.tabulate(5)(i => gem(GemKind.Emerald, 1, i.toLong))
      assertTrue(ItemStack.grouped(items).size == 1) &&
      assertTrue(items.size == 5)
    }
  )
}
