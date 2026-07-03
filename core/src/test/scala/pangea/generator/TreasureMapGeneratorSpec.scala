package pangea.generator

import pangea.generator.item.TreasureMapGenerator
import pangea.model.item.{ItemType, MapZone, Rarity}
import zio.test._

object TreasureMapGeneratorSpec extends ZIOSpecDefault {

  override def spec = suite("TreasureMapGenerator")(

    test("зона выбирается по диапазону уровня") {
      assertTrue(MapZone.forLevel(1) == MapZone.Kinet) &&
        assertTrue(MapZone.forLevel(25) == MapZone.Kinet) &&
        assertTrue(MapZone.forLevel(26) == MapZone.TreeShip) &&
        assertTrue(MapZone.forLevel(75) == MapZone.DeadmansGorge) &&
        assertTrue(MapZone.forLevel(100) == MapZone.GiantSkeleton) &&
        assertTrue(MapZone.forLevel(125) == MapZone.IceForest) &&
        assertTrue(MapZone.forLevel(150) == MapZone.AbandonedTemple)
    },

    test("уровень за границами клампится в крайние зоны") {
      assertTrue(MapZone.forLevel(0) == MapZone.Kinet) &&
        assertTrue(MapZone.forLevel(-5) == MapZone.Kinet) &&
        assertTrue(MapZone.forLevel(9999) == MapZone.AbandonedTemple)
    },

    test("диапазоны зон покрывают весь интервал 1..150 без дыр") {
      val covered = (1 to 150).forall(l => MapZone.values.exists(_.levels.contains(l)))
      assertTrue(covered)
    },

    test("целая карта: тип, легендарная редкость, имя, описание зоны и подсказка осмотра") {
      val map = TreasureMapGenerator.create(dropLevel = 10, half = false)
      assertTrue(map.itemType == ItemType.TreasureMap) &&
        assertTrue(map.rarity == Rarity.Orange) &&
        assertTrue(map.isTreasureMap) &&
        assertTrue(map.name == MapZone.Kinet.mapName) &&
        assertTrue(map.mapDescription.exists(_.contains(MapZone.Kinet.description))) &&
        assertTrue(map.mapDescription.exists(_.contains(MapZone.InspectHint))) &&
        assertTrue(map.mapZone.contains(MapZone.Kinet))
    },

    test("половинка: свой тип, имя-половинка и общее описание-заглушка") {
      val half = TreasureMapGenerator.create(dropLevel = 10, half = true)
      assertTrue(half.itemType == ItemType.TreasureMapHalf) &&
        assertTrue(half.name == MapZone.Kinet.halfName) &&
        assertTrue(half.mapDescription.contains(MapZone.HalfDescription)) &&
        assertTrue(half.isTreasureMap)
    },

    test("карты не входят в надеваемые типы") {
      assertTrue(!ItemType.equippable.contains(ItemType.TreasureMap)) &&
        assertTrue(!ItemType.equippable.contains(ItemType.TreasureMapHalf))
    },

    test("описание Кинэт — то самое, а у прочих зон пока заглушка") {
      assertTrue(MapZone.Kinet.description.contains("пещере правее самой деревни")) &&
        assertTrue(MapZone.TreeShip.description.contains("ещё не составлено"))
    }
  )
}
