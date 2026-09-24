package pangea.model.auction

import enumeratum.{DoobieEnum, Enum, EnumEntry}
import pangea.model.hero.HeroId
import pangea.model.item.Item

/** Чем расплачиваются за лот. Дублоны лежат только на руках, серебро — ещё и в
 *  ячейке Торгового дома (см. `Purse`). */
sealed abstract class AuctionCurrency(val label: String, val emoji: String) extends EnumEntry
object AuctionCurrency extends Enum[AuctionCurrency] with DoobieEnum[AuctionCurrency] {
  case object Silver    extends AuctionCurrency("серебро", "🪙")
  case object Doubloons extends AuctionCurrency("дублоны", "🟡")

  val values: IndexedSeq[AuctionCurrency] = findValues
}

/** Что с лотом стало. `Active` — либо в продаже, либо уже просроченный и ждёт,
 *  когда хозяин заберёт вещь: разницу решает `expiresAt`, а не отдельный статус,
 *  поэтому фоновой задачи для истечения срока не нужно. */
sealed trait LotStatus extends EnumEntry
object LotStatus extends Enum[LotStatus] with DoobieEnum[LotStatus] {
  case object Active   extends LotStatus
  case object Sold     extends LotStatus
  /** Снят хозяином или забран после срока — вещь вернулась в сумку. */
  case object Returned extends LotStatus

  val values: IndexedSeq[LotStatus] = findValues
}

/** Лот аукциона в Торговом доме. Вещь лежит в лоте (а не в сумке продавца) всё
 *  время торгов: неделю, если её не купят раньше. */
final case class AuctionLot(
  id:        Long,
  sellerId:  HeroId,
  item:      Item,
  price:     Long,
  currency:  AuctionCurrency,
  status:    LotStatus,
  listedAt:  Long,
  expiresAt: Long,
  buyerId:   Option[Long]
) {
  /** Лот в продаже: можно купить. */
  def onSale(now: Long): Boolean = status == LotStatus.Active && expiresAt > now

  /** Неделя вышла, никто не купил — ждёт хозяина в «непроданных». */
  def unsold(now: Long): Boolean = status == LotStatus.Active && expiresAt <= now

  /** Сколько осталось до конца торгов, в часах (вниз). */
  def hoursLeft(now: Long): Long = ((expiresAt - now).max(0L)) / 3600000L

  def priceLine: String = s"${currency.emoji} $price"
}

object AuctionLot {
  /** Дешевле полусотни лот не выставить — ни в серебре, ни в дублонах. */
  val MinPrice: Long = 50L

  /** Потолок цены: защита от случайного нуля лишнего и от переполнения. */
  val MaxPrice: Long = 1000000000L

  /** Сбор Торгового дома за выставление — десятая часть запрошенной цены,
    * в той же монете и без возврата, даже если лот снимут. */
  val FeePct: Long = 10

  /** Лот живёт неделю, потом уходит в «непроданные». */
  val LifetimeMs: Long = 7L * 24L * 60L * 60L * 1000L

  def fee(price: Long): Long = ((price * FeePct) / 100L).max(1L)

  def fresh(sellerId: HeroId, item: Item, price: Long, currency: AuctionCurrency, now: Long): AuctionLot =
    AuctionLot(0L, sellerId, item, price, currency, LotStatus.Active, now, now + LifetimeMs, None)

  /** Объявление в общий чат: номер, вещь целиком и цена. */
  def announcement(lot: AuctionLot): String = {
    val stats = lot.item.statsLines
    val body  = if (stats.isEmpty) "" else "\n" + stats.mkString("\n")
    s"📢 Выставлен лот номер ${lot.id}\n${lot.item.displayTitle}$body\nЦена: ${lot.priceLine}"
  }
}
