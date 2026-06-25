package pangea.model.state

import enumeratum._

sealed trait StateType extends EnumEntry

object StateType extends Enum[StateType] with DoobieEnum[StateType] {

  val values = findValues

  // Battle 50% · FoundItem 25% · Spring 25%
  val events: List[StateType] = List(Battle, Battle, FoundItem, Spring)

  case object GlobalMap    extends StateType
  case object Registration extends StateType
  case object Dungeon      extends StateType
  case object HeroStats    extends StateType
  case object Battle       extends StateType
  case object Death        extends StateType

  case object FoundItem extends StateType
  case object Rest      extends StateType
  case object Spring    extends StateType
  case object Inventory  extends StateType
  case object Equipment  extends StateType
  case object Loot       extends StateType
  case object Merchant   extends StateType
  case object Tavern     extends StateType
  case object QuestBoard extends StateType
  case object Innkeeper  extends StateType
}
