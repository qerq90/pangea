package pangea.service.state.states.events.cave

import pangea.model.item.{BrewKind, FlaskEffect, Item, ItemDetails, MaterialKind}

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

object CaveSupply {

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
