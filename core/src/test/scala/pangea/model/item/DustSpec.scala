package pangea.model.item

import pangea.generator.item.MaterialGenerator
import pangea.model.barrel.Barrel
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory
import zio.test._

/** Невесомое добро — пыль и малые руны — места не занимает ни в сумке, ни в
  * бочке, но его не больше сотни одного вида в каждом хранилище. В кубе Азата
  * это обычные предметы и занимают один из девяти слотов (см. CubeState). */
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
      assertTrue(dusty.items.data.forall(_.weightless)) &&
      assertTrue(mixed.occupied == 1L && mixed.freeSlots == 19L)
    },

    test("предел — сотня на вид, у каждого вида свой счёт") {
      val hundred  = bag((1L to 100L).toList.map(dust(ruby, _)))
      val ninety   = bag((1L to 99L).toList.map(dust(ruby, _)))
      val rubyDust = dust(ruby, 999L)
      assertTrue(Item.HoardLimit == 100) &&
      assertTrue(ninety.hoardCount(rubyDust.hoardKey.get) == 99 && ninety.hasRoomForHoard(rubyDust)) &&
      assertTrue(hundred.hoardCount(rubyDust.hoardKey.get) == 100 && !hundred.hasRoomForHoard(rubyDust)) &&
      assertTrue(hundred.hasRoomForHoard(dust(emerald, 998L)))   // изумрудной ещё ни одной
    },

    test("малые руны — такое же невесомое добро: места нет, предел свой на узор") {
      import pangea.model.rune.{Rune, RuneStone, RuneStoneSize}
      import pangea.model.skill.Skill
      def small(rune: Rune, id: Long) = RuneStone.item(rune, RuneStoneSize.Small).copy(id = id)
      val sweeping = Rune.Active(Skill.SweepingStrike)
      val bleeding = Rune.Active(Skill.Bleeding)
      val big      = RuneStone.item(sweeping, RuneStoneSize.Big)
      val full     = bag((1L to 100L).toList.map(small(sweeping, _)))
      assertTrue(small(sweeping, 1L).isSmallRune && small(sweeping, 1L).weightless) &&
      assertTrue(!big.weightless && big.hoardKey.isEmpty) &&      // большая занимает слот
      assertTrue(full.occupied == 0L && full.freeSlots == 20L) &&
      assertTrue(!full.hasRoomForHoard(small(sweeping, 101L))) &&
      assertTrue(full.hasRoomForHoard(small(bleeding, 101L))) &&  // у другого узора свой счёт
      assertTrue(RuneStoneSize.Small.points == 1L && RuneStoneSize.Big.points == 5L) &&
      assertTrue(RuneStone.PiecesPerBig == 5)
    },

    test("в бочке всё так же: пыль без места, но со своим пределом на вид") {
      val dusty = barrel((1L to 100L).toList.map(dust(ruby, _)))
      val mixed = barrel(gear(1L) :: (2L to 50L).toList.map(dust(emerald, _)))
      assertTrue(dusty.occupied == 0L && dusty.freeSlots == Barrel.MaxItems) &&
      assertTrue(!dusty.hasRoomForHoard(dust(ruby, 999L)) && dusty.hasRoomForHoard(dust(emerald, 998L))) &&
      assertTrue(mixed.occupied == 1L && mixed.freeSlots == Barrel.MaxItems - 1L)
    }
  )
}
