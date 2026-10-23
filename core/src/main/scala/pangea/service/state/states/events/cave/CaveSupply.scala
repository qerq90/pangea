package pangea.service.state.states.events.cave

import pangea.model.item.{BrewKind, FlaskEffect, Item, ItemDetails, MaterialKind}
import pangea.service.state.BeltContents

/** Что вещь даёт пещере, если пустить её в дело на пороге. */
sealed trait CaveBoon
object CaveBoon {

  /** Мобы одурманены: атака и энергия у всех срезаны. Даёт сонный дурман и
    * дымная фляга — от них одинаково клонит в сон. */
  case object Dope extends CaveBoon

  /** Все мобы пещеры входят в бой отравленными (глефовый гриб). */
  case object Poison extends CaveBoon

  /** Вещь истрачена впустую: пещере от неё ни холодно, ни жарко. */
  case object None extends CaveBoon
}

/** Что можно пустить в дело на пороге пещеры и у каравана: вещь при герое — в
  * сумке или в руке (по id) — или вещь из ячейки пояса (по номеру ячейки). */
sealed trait Supply { def item: Item }
object Supply {
  final case class Carried(item: Item) extends Supply
  final case class OnBelt(slot: Int, item: Item) extends Supply
}

object CaveSupply {

  /** Что из пояса можно пустить в дело: по ячейке — вещь, собранная по её сути
    * (см. BeltContents.item). Высыпанной пыли, выпитого отвара и осыпавшейся
    * розы нет — брать из них нечего; пустая фляга есть: она так и остаётся
    * флягой. Номер — настоящий номер ячейки. */
  def beltSupplies(belt: Item): List[Supply.OnBelt] =
    BeltContents.slotsOf(belt).zipWithIndex.flatMap { case (slot, i) =>
      BeltContents.item(slot).map(Supply.OnBelt(i, _))
    }

  /** Пояс после того, как из ячейки `slot` взяли одно (см. `BeltSlot.taken`). */
  def beltTaken(belt: Item, slot: Int): Item = belt.details match {
    case b: ItemDetails.Belt => b.slots.lift(slot).fold(belt)(s => belt.copy(details = b.updated(slot, s.taken)))
    case _                   => belt
  }

  /** Чем эта вещь поможет в пещере. Смотрим по сути предмета, а не по имени:
    * дымная фляга опознаётся по своему эффекту, каким бы редким ни был экземпляр. */
  def boonOf(item: Item): CaveBoon = item.details match {
    case ItemDetails.Flask(FlaskEffect.Smoke(_), _, _)      => CaveBoon.Dope
    case ItemDetails.Brew(BrewKind.SleepingDope)            => CaveBoon.Dope
    case ItemDetails.Material(MaterialKind.GlaiveMushroom)  => CaveBoon.Poison
    case _                                                  => CaveBoon.None
  }

  /** Заряды вещи, если они у неё есть: такую не забирают целиком, у неё тратят
    * один глоток (фляга, пояс, божественное оружие, роза). */
  def charged(item: Item): Option[ItemDetails.Charged] = item.details match {
    case c: ItemDetails.Charged => Some(c)
    case _                      => None
  }
}
