package pangea.model.payment

import pangea.model.user.UserId

/** Номер заказа в нашей системе. Уходит в банк как `OrderId` и должен быть
  * уникальным: по нему заказ находится и в нотификации, и в личном кабинете. */
case class OrderId(value: String) extends AnyVal

/** Заказ на покупку дублонов.
  *
  * Цена, количество дублонов и адрес для чека лежат в самой строке, а не берутся
  * из прайс-листа по `sku`: прайс и почта могут поменяться между созданием
  * заказа и оплатой, а выдать нужно ровно обещанное.
  *
  * `granted` — флаг выдачи и единственная защита от двойного начисления:
  * `CONFIRMED` приходит минимум дважды (при одностадийной оплате вместе с
  * `AUTHORIZED`) и переотправляется сутками, если мы не ответили `OK`. */
case class Payment(
  id:            Long,
  orderId:       OrderId,
  userId:        UserId,
  sku:           String,
  amountKopecks: Long,
  doubloons:     Long,
  receiptEmail:  String,
  paymentId:     Option[String],
  paymentUrl:    Option[String],
  status:        PaymentStatus,
  bankStatus:    Option[String],
  granted:       Boolean,
  createdAt:     Long,
  updatedAt:     Long
) {

  /** Цена рублями — для экранов и писем, не для банка: в банк всегда уходят
    * копейки. */
  def rubles: String = Payment.rubles(amountKopecks)

  /** Ссылка на оплату жива, пока жив заказ: банк закрывает её по
    * `RedirectDueDate`, который мы считаем от создания заказа. */
  def linkAlive(now: Long, lifetimeMinutes: Int): Boolean =
    paymentUrl.nonEmpty && now < createdAt + lifetimeMinutes.toLong * 60000L
}

object Payment {

  /** `34900` → `349`, `34950` → `349,50`. Копейки показываем только когда они
    * есть: «349 ₽» читается лучше, чем «349,00 ₽». */
  def rubles(kopecks: Long): String = {
    val whole = kopecks / 100L
    val cents = kopecks % 100L
    if (cents == 0L) whole.toString else f"$whole,$cents%02d"
  }
}
