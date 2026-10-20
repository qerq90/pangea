package pangea.service.state

import pangea.model.item.{BeltSlot, BrewKind, FlaskKind, Item, ItemDetails, ItemType, MaterialKind, Rarity}
import zio.test._

/** Содержимое пояса за героем: переезд в новый пояс и выкладывание при снятии. */
object BeltContentsSpec extends ZIOSpecDefault {

  private def belt(id: Long, rarity: Rarity, slots: List[BeltSlot] = Nil): Item =
    Item(id, s"Пояс $id", 1L, rarity, ItemType.Belt,
      attack = 0, accuracy = 0, energy = 0, armor = 1, defence = 0, evasion = 0, details = ItemDetails.Belt(slots))

  private val flask = BeltSlot.Flask(FlaskKind.Healer, Rarity.Blue, 3)
  private val dust  = BeltSlot.Dust(MaterialKind.RubyDust)
  private val drunk = BeltSlot.Brew(BrewKind.WolfCall, spent = true)

  override def spec = suite("BeltContents")(

    test("в пояс побольше переезжает всё, старый уходит пустым") {
      val s = BeltContents.swap(belt(1L, Rarity.Green, List(flask, dust)), belt(2L, Rarity.Blue))
      assertTrue(BeltContents.slotsOf(s.next) == List(flask, dust)) &&
      assertTrue(s.next.id == 2L && BeltContents.slotsOf(s.worn).isEmpty && s.worn.id == 1L) &&
      assertTrue(s.spilled.isEmpty && s.moved)
    },

    test("в пояс поменьше — полные ячейки первыми; лишнее вещами в сумку, лишняя пустая пропадает") {
      val s = BeltContents.swap(belt(1L, Rarity.Blue, List(drunk, dust, flask)), belt(2L, Rarity.Gray))
      assertTrue(BeltContents.slotsOf(s.next) == List(dust)) &&
      // фляга вернулась флягой со своими зарядами, выпитый отвар — пустая ячейка, его нет
      assertTrue(s.spilled.map(_.name) == List(FlaskKind.Healer.itemName(Rarity.Blue))) &&
      assertTrue(s.spilled.flatMap(_.details match {
        case f: ItemDetails.Flask => List(f.charges)
        case _                    => Nil
      }) == List(3))
    },

    test("то, что уже лежало в новом поясе, встаёт после переложенного") {
      val s = BeltContents.swap(belt(1L, Rarity.Gray, List(flask)), belt(2L, Rarity.Green, List(dust)))
      assertTrue(BeltContents.slotsOf(s.next) == List(flask, dust)) &&
      assertTrue(s.spilled.isEmpty)
    },

    test("со старого пустого пояса перекладывать нечего") {
      val s = BeltContents.swap(belt(1L, Rarity.Gray), belt(2L, Rarity.Green))
      assertTrue(!s.moved && s.spilled.isEmpty && BeltContents.slotsOf(s.next).isEmpty)
    },

    test("снятие: пояс пустой, содержимое — вещами; пустая фляга остаётся флягой, высыпанная пыль — ничем") {
      val emptyFlask = flask.copy(charges = 0)
      val (bare, items) = BeltContents.strip(belt(1L, Rarity.Blue, List(emptyFlask, dust.poured, dust)))
      assertTrue(BeltContents.slotsOf(bare).isEmpty && bare.id == 1L) &&
      assertTrue(items.map(_.name) == List(FlaskKind.Healer.itemName(Rarity.Blue), MaterialKind.RubyDust.displayName))
    }
  )
}
