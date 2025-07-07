package pangea.model.stats

import doobie._
import doobie.postgres.circe.jsonb.implicits._
import io.circe._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

case class BaseStats(
  agi: Long,
  vit: Long,
  str: Long,
  int: Long
)

object BaseStats {
  implicit val encoder: Encoder[BaseStats] = deriveEncoder[BaseStats]
  implicit val decoder: Decoder[BaseStats] = deriveDecoder[BaseStats]

  implicit val meta: Meta[BaseStats] = new Meta(pgDecoderGet, pgEncoderPut)
}
