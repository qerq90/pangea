package pangea.model.hero

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

case class Equipment(
  helmetId: Long,
  shoulderPadsId: Long,
  chestPlateId: Long,
  braceletsId: Long,
  glovesId: Long,
  pantsId: Long,
  leggingsId: Long,
  amuletId: Long,
  firstRingId: Long,
  secondRingId: Long,
  beltId: Long,
  flaskId: Long,
  weaponId: Long,
  additionalWeaponId: Long
)

object Equipment {
  implicit val encoder: Encoder[Equipment] = deriveEncoder[Equipment]
  implicit val decoder: Decoder[Equipment] = deriveDecoder[Equipment]

  implicit val meta: Meta[Equipment] = new Meta(pgDecoderGet, pgEncoderPut)
}
