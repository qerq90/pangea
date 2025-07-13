package pangea.model.vk.keyboard

import io.circe.Encoder
import io.circe.syntax.EncoderOps

sealed trait Action {
  def `type`: String
  def label: String
  def payload: String
}

case class Text(label_ : String, payload_ : String) extends Action {
  override def `type`: String  = "text"
  override def label: String   = label_
  override def payload: String = payload_
}

case class Callback(label_ : String, payload_ : String) extends Action {
  override def `type`: String  = "callback"
  override def label: String   = label_
  override def payload: String = payload_
}

object Action {
  implicit val encoder: Encoder[Action] =
    (action: Action) =>
      Map[String, String](
        "type"    -> action.`type`,
        "label"   -> action.label,
        "payload" -> action.payload
      ).asJson
}
