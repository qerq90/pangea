package pangea.model.item

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

case class Item(
  id: Long,
  itemType: ItemType,
  attack: Long,
  accuracy: Long,
  armor: Long,
  defence: Long,
  evasion: Long
)

object Item {
  def NoItem: Item = Item(0, ItemType.NoItem, 0, 0, 0, 0, 0)

  implicit val encoder: Encoder[Item] = deriveEncoder[Item]
  implicit val decoder: Decoder[Item] = deriveDecoder[Item]

  implicit val meta: Meta[Item] = new Meta(pgDecoderGet, pgEncoderPut)
}
