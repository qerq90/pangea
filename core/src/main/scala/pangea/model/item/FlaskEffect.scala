package pangea.model.item

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import pangea.model.battle.Buff

sealed trait FlaskEffect
object FlaskEffect {
  case class HealPercent(percent: Int)              extends FlaskEffect
  case class AddBuff(buff: Buff, rounds: Int)       extends FlaskEffect

  implicit val healPercentEncoder: Encoder[HealPercent] = deriveEncoder
  implicit val healPercentDecoder: Decoder[HealPercent] = deriveDecoder
  implicit val addBuffEncoder:     Encoder[AddBuff]     = deriveEncoder
  implicit val addBuffDecoder:     Decoder[AddBuff]     = deriveDecoder

  implicit val encoder: Encoder[FlaskEffect] = deriveEncoder
  implicit val decoder: Decoder[FlaskEffect] = deriveDecoder
}
