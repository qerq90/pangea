package server.model

import io.circe.{Decoder, Json}
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
import io.circe.generic.semiauto.deriveDecoder
import server.model.VkEvent.ObjectMessage

case class VkEvent(`type`: String, `object`: ObjectMessage)

object VkEvent {
  implicit val configuration: Configuration =
    Configuration.default.withSnakeCaseMemberNames

  case class ObjectMessage(message: Message)

  case class Message(text: String, peerId: Long, payload: Option[String])

  implicit val decoder: Decoder[VkEvent] = deriveDecoder[VkEvent]

  implicit val ObjDecoder: Decoder[ObjectMessage] = deriveDecoder[ObjectMessage]

  implicit val messageDecoder: Decoder[Message] =
    deriveConfiguredDecoder[Message]
}
