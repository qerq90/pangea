package pangea.model.inventory

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import pangea.model.hero.HeroId
import pangea.model.item.Item

case class Inventory(
  id: Long,
  heroId: HeroId,
  maxItems: Long,
  items: List[Item]
) {
  def addItem(item: Item): Inventory = copy(items = items.appended(item))
}

object Inventory {
  implicit val encoderForItems: Encoder[List[Item]] = deriveEncoder[List[Item]]
  implicit val decoderForItems: Decoder[List[Item]] = deriveDecoder[List[Item]]

  implicit val meta: Meta[List[Item]] = new Meta(pgDecoderGet, pgEncoderPut)
}
