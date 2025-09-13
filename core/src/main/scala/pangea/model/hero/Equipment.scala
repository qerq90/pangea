package pangea.model.hero

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import pangea.model.item.Item

case class Equipment(
  helmet: Item,
  shoulderPads: Item,
  chestPlate: Item,
  bracelets: Item,
  gloves: Item,
  pants: Item,
  boots: Item,
  amulet: Item,
  firstRing: Item,
  secondRing: Item,
  belt: Item,
  flask: Item,
  weapon: Item,
  additionalWeapon: Item
)

object Equipment {
  implicit val encoder: Encoder[Equipment] = deriveEncoder[Equipment]
  implicit val decoder: Decoder[Equipment] = deriveDecoder[Equipment]

  implicit val meta: Meta[Equipment] = new Meta(pgDecoderGet, pgEncoderPut)
}
