package pangea.service.state.states.Registration

import enumeratum._
import io.circe._
import io.circe.syntax.EncoderOps

sealed trait Action extends EnumEntry

object Action extends Enum[Action] {
  val values = findValues

  case object Unknown extends Action
  case object Text    extends Action
  case object Start   extends Action

  implicit val encoder: Encoder[Action] = (a: Action) =>
    Map[String, String]("action" -> a.entryName).asJson

  implicit val decoder: Decoder[Action] = (cursor: HCursor) =>
    cursor.getOrElse[String]("action")("Unknown").map(Action.withName)

  implicit class ActionOps(action: Action) {
    def json: Json = action.asJson
  }
}
