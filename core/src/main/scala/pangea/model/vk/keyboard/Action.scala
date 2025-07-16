package pangea.model.vk.keyboard

import io.circe.{Encoder, Json}
import io.circe.syntax.EncoderOps

sealed trait Action {
  def `type`: String
  def label: String
  def payload: Option[Json]
}

object Action {
  val payloadEmpty: Json = Json.Null

  case class Text(label_ : String, payload_ : Option[Json]) extends Action {
    override def `type`: String        = "text"
    override def label: String         = label_
    override def payload: Option[Json] = payload_
  }

  case class Callback(label_ : String, payload_ : Json) extends Action {
    override def `type`: String        = "callback"
    override def label: String         = label_
    override def payload: Option[Json] = Some(payload_)
  }

  implicit val encoder: Encoder[Action] =
    (action: Action) =>
      action.payload match {
        case None =>
          Map[String, String](
            "type"  -> action.`type`,
            "label" -> action.label
          ).asJson
        case Some(payload) =>
          Map[String, String](
            "type"    -> action.`type`,
            "label"   -> action.label,
            "payload" -> payload.toString()
          ).asJson
      }
}
