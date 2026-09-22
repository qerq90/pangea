package pangea.model.inventory

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.Items
import pangea.model.item.Item

case class Inventory(
  id: Long,
  heroId: HeroId,
  maxItems: Long,
  items: Items
) {
  def addItem(item: Item): Inventory =
    copy(items = items.copy(data = items.data.appended(item)))

  def withItems(items: List[Item]): Inventory = copy(items = Items(items))

  /** Сколько слотов занято: сюжетные предметы, пыль и малые руны места не занимают. */
  def occupied: Long = items.data.count(i => !i.isQuestItem && !i.weightless).toLong

  /** Сколько такого же невесомого добра уже лежит — предел у него вместо места. */
  def hoardCount(key: String): Int = items.data.count(_.hoardKey.contains(key))

  /** Влезет ли ещё одна такая невесомая вещь (пыль, малая руна). */
  def hasRoomForHoard(item: Item): Boolean =
    item.hoardKey.forall(k => hoardCount(k) < Item.HoardLimit)

  /** Свободных слотов в сумке (не уходит ниже нуля). */
  def freeSlots: Long = (maxItems - occupied).max(0L)

  /** Влезет ли ещё `count` обычных предметов. */
  def hasRoomFor(count: Long): Boolean = occupied + count <= maxItems
}

object Inventory {
  /** Вместимость сумки по умолчанию (слотов). */
  val DefaultCapacity: Long = 20L

  case class Items(data: List[Item])

  implicit val encoderForItems: Encoder[Items] = deriveEncoder[Items]
  implicit val decoderForItems: Decoder[Items] = deriveDecoder[Items]

  implicit val meta: Meta[Items] = new Meta(pgDecoderGet, pgEncoderPut)
}
