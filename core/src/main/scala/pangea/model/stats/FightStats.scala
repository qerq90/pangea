package pangea.model.stats

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, Encoder, HCursor}

case class FightStats(
  atk:           Long,
  hp:            Long,
  armor:         Long,
  defence:       Long,
  evasion:       Long,
  accuracy:      Long,
  concentration: Long
)

object FightStats {
  implicit val encoder: Encoder[FightStats] = deriveEncoder[FightStats]

  implicit val decoder: Decoder[FightStats] = (c: HCursor) =>
    for {
      atk           <- c.get[Long]("atk")
      hp            <- c.get[Long]("hp")
      armor         <- c.get[Long]("armor")
      defence       <- c.get[Long]("defence")
      evasion       <- c.getOrElse[Long]("evasion")(0L)
      accuracy      <- c.getOrElse[Long]("accuracy")(0L)
      concentration <- c.getOrElse[Long]("concentration")(0L)
    } yield FightStats(atk, hp, armor, defence, evasion, accuracy, concentration)

  implicit val meta: Meta[FightStats] = new Meta(pgDecoderGet, pgEncoderPut)
}
