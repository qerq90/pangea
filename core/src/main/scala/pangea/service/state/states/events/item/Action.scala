package pangea.service.state.states.events.item

import enumeratum._
import io.circe._
import io.circe.syntax.EncoderOps

sealed trait Action extends EnumEntry

object Action extends Enum[Action] {
  val values = findValues

  case object Text         extends Action
  case object TakeItem     extends Action
  case object DontTakeItem extends Action

  implicit val encoder: Encoder[Action] = (a: Action) =>
    Map[String, String]("action" -> a.entryName).asJson

  implicit val decoder: Decoder[Action] = (cursor: HCursor) =>
    cursor.getOrElse[String]("action")("Unknown").map(Action.withName)

  implicit class ActionOps(action: Action) {
    def json: Json = action.asJson
  }

}
