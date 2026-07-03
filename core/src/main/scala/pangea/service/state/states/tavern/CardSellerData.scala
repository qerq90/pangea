package pangea.service.state.states.tavern

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Durable-состояние продавца карт (`heroes.card_seller_data`).
 *  - `offerUntil`  — пока `now < offerUntil`, продавец присутствует в таверне;
 *  - `price`       — цена текущего предложения в дублонах (стабильна на всё присутствие);
 *  - `nextRollAt`  — не ролить появление раньше этого момента: почасовой гейт, а после
 *                    покупки — кулдаун 20 часов. Один таймстамп на оба случая. */
final case class CardSellerData(
  offerUntil: Option[Long],
  price:      Option[Long],
  nextRollAt: Option[Long]
) {
  def present(nowMs: Long): Boolean = offerUntil.exists(_ > nowMs)
}

object CardSellerData {
  val empty: CardSellerData = CardSellerData(None, None, None)

  val RollChancePercent: Int  = 25
  val RollIntervalMs:    Long = 60L * 60L * 1000L        // «раз в час»
  val PresenceMs:        Long = 60L * 60L * 1000L        // продавец ждёт 1 час
  val PurchaseCooldownMs: Long = 20L * 60L * 60L * 1000L // после покупки — 20 часов
  val PriceMin: Long = 90L
  val PriceMax: Long = 210L

  implicit val encoder: Encoder[CardSellerData] = deriveEncoder
  implicit val decoder: Decoder[CardSellerData] = deriveDecoder
}
