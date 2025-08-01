package pangea.service.state.states.dungeon

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

sealed trait Action extends EnumEntry

object Action extends Enum[Action] {
  override def values: IndexedSeq[Action] = findValues

  case object Text      extends Action
  case object FindEvent extends Action

  implicit val encoder: Encoder[Action] = (a: Action) =>
    Map[String, String]("action" -> a.entryName).asJson

  implicit val decoder: Decoder[Action] = (cursor: HCursor) =>
    cursor.getOrElse[String]("action")("Unknown").map(Action.withName)

  implicit class ActionOps(action: Action) {
    def json: Json = action.asJson
  }
}
