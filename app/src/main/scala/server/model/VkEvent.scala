package server.model

import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
import io.circe.generic.semiauto.deriveDecoder
import server.model.VkEvent.ObjectMessage

case class VkEvent(`type`: String, `object`: ObjectMessage)

object VkEvent {
  implicit val configuration: Configuration =
    Configuration.default.withSnakeCaseMemberNames

  case class ObjectMessage(message: Message)

  /** Сообщение из ВК. В личке `peerId` совпадает с `fromId`, в беседе это её
   *  `peer_id` (2000000000 + chat_id), а автор виден только в `fromId`.
   *
   *  `replyMessage`/`fwdMessages` нужны команде «Передать» из общей беседы:
   *  адресат берётся из того, на чьё сообщение ответили или чьё переслали. */
  case class Message(
    id:           Long,
    text:         String,
    peerId:       Long,
    payload:      Option[String],
    fromId:       Option[Long]        = None,
    replyMessage: Option[Nested]      = None,
    fwdMessages:  Option[List[Nested]] = None
  ) {
    /** Беседа, а не личка: у бесед `peer_id` начинается с 2000000000. */
    def fromChat: Boolean = peerId >= VkEvent.ChatPeerBase

    /** Кому адресована команда: автор процитированного или пересланного. */
    def quotedAuthor: Option[Long] =
      replyMessage.flatMap(_.fromId).orElse(fwdMessages.flatMap(_.flatMap(_.fromId).headOption))
  }

  /** Вложенное сообщение (ответ или пересылка): нас интересует только автор. */
  case class Nested(fromId: Option[Long])

  /** С этого числа начинаются `peer_id` бесед. */
  val ChatPeerBase: Long = 2000000000L

  implicit val decoder: Decoder[VkEvent] = deriveDecoder[VkEvent]

  implicit val ObjDecoder: Decoder[ObjectMessage] = deriveDecoder[ObjectMessage]

  implicit val nestedDecoder: Decoder[Nested] = deriveConfiguredDecoder[Nested]

  implicit val messageDecoder: Decoder[Message] =
    deriveConfiguredDecoder[Message]
}
