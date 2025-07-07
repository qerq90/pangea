package pangea.model.stats

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

case class FightStats(
  atk: Long,
  hp: Long,
  armor: Long,
  defence: Long
)

object FightStats {
  implicit val encoder: Encoder[FightStats] = deriveEncoder[FightStats]
  implicit val decoder: Decoder[FightStats] = deriveDecoder[FightStats]

  implicit val meta: Meta[FightStats] = new Meta(pgDecoderGet, pgEncoderPut)
}
