package server.model

import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.{
  deriveConfiguredDecoder,
  deriveConfiguredEncoder
}
import io.circe.{Decoder, Encoder}

case class VkChecking(`type`: String, groupId: Long)

object VkChecking {

  implicit val configuration: Configuration =
    Configuration.default.withSnakeCaseMemberNames

  implicit val decoder: Decoder[VkChecking] = deriveConfiguredDecoder
  implicit val encoder: Encoder[VkChecking] = deriveConfiguredEncoder
}
