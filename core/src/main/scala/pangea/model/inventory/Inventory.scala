package pangea.model.inventory

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.Items
import pangea.model.item.{Item, MaterialKind}

case class Inventory(
  id: Long,
  heroId: HeroId,
  maxItems: Long,
  items: Items
) {
  def addItem(item: Item): Inventory =
    copy(items = items.copy(data = items.data.appended(item)))

  def withItems(items: List[Item]): Inventory = copy(items = Items(items))

  /** Сколько слотов занято: сюжетные предметы и пыль места не занимают. */
  def occupied: Long = items.data.count(i => !i.isQuestItem && !i.isDust).toLong

  /** Сколько горстей этой пыли уже лежит — предел у неё вместо места. */
  def dustCount(kind: MaterialKind): Int = items.data.count(_.dustKind.contains(kind))

  /** Влезет ли ещё одна горсть этого вида. */
  def hasRoomForDust(kind: MaterialKind): Boolean = dustCount(kind) < MaterialKind.MaxDustPerKind

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
