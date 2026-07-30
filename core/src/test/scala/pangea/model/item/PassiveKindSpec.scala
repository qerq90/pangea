package pangea.model.item

import zio.test._

object PassiveKindSpec extends ZIOSpecDefault {

  override def spec = suite("PassiveKind")(

    // Регрессия: если аргумент конструктора case object-а ссылается на член
    // собственного компаньона, первое обращение к самому объекту (напр.
    // PassiveKind.Taxidermist) инициализирует компаньон изнутри конструктора и
    // findValues кладёт в values null — poolFor падает с NPE.
    test("values не содержит null (инициализация без цикла через компаньон)") {
      val forced = PassiveKind.Taxidermist
      assertTrue(forced.eligibleSlots.nonEmpty) &&
      assertTrue(!PassiveKind.values.contains(null)) &&
      assertTrue(PassiveKind.values.contains(PassiveKind.Taxidermist))
    },

    test("poolFor даёт пассивки носимым слотам и пусто — оружию/фляге") {
      assertTrue(PassiveKind.poolFor(ItemType.Helmet).nonEmpty) &&
      assertTrue(PassiveKind.poolFor(ItemType.Gloves).nonEmpty) &&
      assertTrue(PassiveKind.poolFor(ItemType.Boots).nonEmpty) &&
      assertTrue(PassiveKind.poolFor(ItemType.Ring).nonEmpty) &&
      assertTrue(PassiveKind.poolFor(ItemType.Amulet).nonEmpty) &&
      assertTrue(PassiveKind.poolFor(ItemType.Weapon).isEmpty) &&
      assertTrue(PassiveKind.poolFor(ItemType.Flask).isEmpty) &&
      assertTrue(PassiveKind.poolFor(ItemType.ChestPlate).isEmpty) &&
      assertTrue(PassiveKind.poolFor(ItemType.Belt).isEmpty)
    }
  )
}
