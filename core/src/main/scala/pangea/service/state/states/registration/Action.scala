package pangea.service.state.states.registration

import enumeratum._
import io.circe._
import io.circe.syntax.EncoderOps

sealed trait Action extends EnumEntry

object Action extends Enum[Action] {
  val values = findValues

  case object Unknown         extends Action
  case object Text            extends Action
  case object Race            extends Action
  case object RaceDescription extends Action
  case object Travel          extends Action
  case object Travel1         extends Action
  case object Travel2         extends Action
  case object Travel3         extends Action
  case object Travel4         extends Action
  case object Travel5         extends Action
  case object Travel6         extends Action
  case object Travel7         extends Action
  case object Travel8         extends Action
  case object Travel9         extends Action
  case object EndOfTravel     extends Action

  implicit val encoder: Encoder[Action] = (a: Action) =>
    Map[String, String]("action" -> a.entryName).asJson

  implicit val decoder: Decoder[Action] = (cursor: HCursor) =>
    cursor.getOrElse[String]("action")("Unknown").map(Action.withName)

  implicit class ActionOps(action: Action) {
    def json: Json = action.asJson
  }

}
