package pangea.model.state

import enumeratum.{DoobieEnum, _}

//TODO add other states
sealed trait StateType extends EnumEntry

object StateType extends Enum[StateType] with DoobieEnum[StateType] {

  val values = findValues

  val events: List[StateType] = List(FoundItem)

  case object GlobalMap    extends StateType
  case object Registration extends StateType
  case object Dungeon      extends StateType

  case object FoundItem extends StateType
}
