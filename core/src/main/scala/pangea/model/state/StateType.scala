package pangea.model.state

import enumeratum.{DoobieEnum, _}

//TODO add other states
sealed trait StateType extends EnumEntry

object StateType extends Enum[StateType] with DoobieEnum[StateType] {

  val values = findValues

  case object GlobalMap    extends StateType
  case object Registration extends StateType
}
