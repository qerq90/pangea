package pangea.model.hero

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import pangea.model.item.Item

case class Equipment(
  helmetId: Item,
  shoulderPadsId: Item,
  chestPlateId: Item,
  braceletsId: Item,
  glovesId: Item,
  pantsId: Item,
  leggingsId: Item,
  amuletId: Item,
  firstRingId: Item,
  secondRingId: Item,
  beltId: Item,
  flaskId: Item,
  weaponId: Item,
  additionalWeaponId: Item
)

object Equipment {
  implicit val encoder: Encoder[Equipment] = deriveEncoder[Equipment]
  implicit val decoder: Decoder[Equipment] = deriveDecoder[Equipment]

  implicit val meta: Meta[Equipment] = new Meta(pgDecoderGet, pgEncoderPut)
}
