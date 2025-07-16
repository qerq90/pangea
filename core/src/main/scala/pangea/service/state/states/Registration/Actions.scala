package pangea.service.state.states.Registration

import enumeratum._

trait Actions extends EnumEntry

object Actions extends Enum[Actions] {
  val values = findValues

  case object Text  extends Actions
  case object Start extends Actions
}
