package server.model

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

case class VkChecking(`type`: String, groupId: Long)

object VkChecking {

  implicit val decoder: Decoder[VkChecking] = deriveDecoder
  implicit val encoder: Encoder[VkChecking] = deriveEncoder
}
