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
) {
  def allArmor: Long =
    helmet.armor + shoulderPads.armor + chestPlate.armor + bracelets.armor +
    gloves.armor + pants.armor + boots.armor + amulet.armor +
    firstRing.armor + secondRing.armor + belt.armor + flask.armor +
    weapon.armor + additionalWeapon.armor

  // Суммарная прибавка к HP от экипировки (сейчас её даёт только нагрудник)
  def allHp: Long =
    helmet.hp + shoulderPads.hp + chestPlate.hp + bracelets.hp +
    gloves.hp + pants.hp + boots.hp + amulet.hp +
    firstRing.hp + secondRing.hp + belt.hp + flask.hp +
    weapon.hp + additionalWeapon.hp
}

object Equipment {
  implicit val encoder: Encoder[Equipment] = deriveEncoder[Equipment]
  implicit val decoder: Decoder[Equipment] = deriveDecoder[Equipment]

  implicit val meta: Meta[Equipment] = new Meta(pgDecoderGet, pgEncoderPut)
}
