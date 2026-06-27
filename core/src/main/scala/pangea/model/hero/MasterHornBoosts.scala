package pangea.model.hero

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Сколько раз герой улучшал каждую характеристику у Мастера Горна. */
case class MasterHornBoosts(
  armor:         Long = 0L,
  evasion:       Long = 0L,
  attack:        Long = 0L,
  defence:       Long = 0L,
  accuracy:      Long = 0L,
  concentration: Long = 0L,
  inventory:     Long = 0L
)

object MasterHornBoosts {
  val empty: MasterHornBoosts = MasterHornBoosts()

  implicit val encoder: Encoder[MasterHornBoosts] = deriveEncoder[MasterHornBoosts]
  implicit val decoder: Decoder[MasterHornBoosts] = deriveDecoder[MasterHornBoosts]

  implicit val meta: Meta[MasterHornBoosts] = new Meta(pgDecoderGet, pgEncoderPut)
}
