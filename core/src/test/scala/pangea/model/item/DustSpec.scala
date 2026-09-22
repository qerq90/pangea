package pangea.model.item

import pangea.generator.item.MaterialGenerator
import pangea.model.barrel.Barrel
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory
import zio.test._

/** Пыль места не занимает — ни в сумке, ни в бочке, — но её не больше сотни
  * горстей каждого вида в каждом хранилище. В кубе Азата она обычный предмет и
  * занимает один из девяти слотов (см. CubeState). */
object DustSpec extends ZIOSpecDefault {

  private def dust(kind: MaterialKind, id: Long): Item = MaterialGenerator.item(kind).copy(id = id)

  private def gear(id: Long): Item =
    Item(id, "Меч", 1L, Rarity.Gray, ItemType.Weapon, attack = 1, accuracy = 0, energy = 0,
      armor = 0, defence = 0, evasion = 0)

  private def bag(items: List[Item], max: Long = 20L): Inventory =
    Inventory(1L, HeroId(1L), max, Inventory.Items(items))

  private def barrel(items: List[Item]): Barrel = Barrel(1L, HeroId(1L), Inventory.Items(items), 0L)

  private val ruby    = MaterialKind.dustOf(GemKind.Ruby)
  private val emerald = MaterialKind.dustOf(GemKind.Emerald)

  override def spec = suite("Пыль")(

    test("пыль узнаётся по виду: горсть — пыль, мифрил и трава — нет") {
      assertTrue(dust(ruby, 1L).isDust && dust(ruby, 1L).dustKind.contains(ruby)) &&
      assertTrue(MaterialKind.dusts.forall(k => MaterialGenerator.item(k).isDust)) &&
      assertTrue(!MaterialGenerator.mithril.isDust && MaterialGenerator.mithril.dustKind.isEmpty) &&
      assertTrue(!MaterialGenerator.item(MaterialKind.Wormwood).isDust) &&
      assertTrue(!gear(1L).isDust)
    },

    test("в сумке пыль не занимает слотов: сотня горстей — и сумка всё ещё пуста") {
      val dusty = bag((1L to 100L).toList.map(dust(ruby, _)))
      val mixed = bag(gear(1L) :: (2L to 30L).toList.map(dust(emerald, _)))
      assertTrue(dusty.occupied == 0L && dusty.freeSlots == 20L && dusty.hasRoomFor(20L)) &&
      assertTrue(mixed.occupied == 1L && mixed.freeSlots == 19L)
    },

    test("предел пыли — сотня на вид, у каждого вида свой счёт") {
      val hundred = bag((1L to 100L).toList.map(dust(ruby, _)))
      val ninety  = bag((1L to 99L).toList.map(dust(ruby, _)))
      assertTrue(MaterialKind.MaxDustPerKind == 100) &&
      assertTrue(ninety.dustCount(ruby) == 99 && ninety.hasRoomForDust(ruby)) &&
      assertTrue(hundred.dustCount(ruby) == 100 && !hundred.hasRoomForDust(ruby)) &&
      assertTrue(hundred.hasRoomForDust(emerald))   // изумрудной ещё ни одной
    },

    test("в бочке всё так же: пыль без места, но со своим пределом на вид") {
      val dusty = barrel((1L to 100L).toList.map(dust(ruby, _)))
      val mixed = barrel(gear(1L) :: (2L to 50L).toList.map(dust(emerald, _)))
      assertTrue(dusty.occupied == 0L && dusty.freeSlots == Barrel.MaxItems) &&
      assertTrue(!dusty.hasRoomForDust(ruby) && dusty.hasRoomForDust(emerald)) &&
      assertTrue(mixed.occupied == 1L && mixed.freeSlots == Barrel.MaxItems - 1L)
    }
  )
}
