package pangea.model.item

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

case class Item(
  id: Long,
  name: String,
  itemType: ItemType,
  attack: Long,
  accuracy: Long,
  concentration: Long,
  armor: Long,
  defence: Long,
  evasion: Long
) {
  def withName(name: String): Item = copy(name = name)

  def withAttack(attack: Long): Item = copy(attack = attack)

  def withAccuracy(accuracy: Long): Item = copy(accuracy = accuracy)

  def withConcentration(concentration: Long): Item =
    copy(concentration = concentration)

  def withArmor(armor: Long): Item = copy(armor = armor)

  def withDefence(defence: Long): Item = copy(defence = defence)

  def withEvasion(evasion: Long): Item = copy(evasion = evasion)
}

object Item {
  def NoItem: Item = Item(0, "Пусто", ItemType.NoItem, 0, 0, 0, 0, 0, 0)

  implicit val encoder: Encoder[Item] = deriveEncoder[Item]
  implicit val decoder: Decoder[Item] = deriveDecoder[Item]

  implicit val meta: Meta[Item] = new Meta(pgDecoderGet, pgEncoderPut)
}
