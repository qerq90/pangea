package pangea.model.payment

import enumeratum.{DoobieEnum, Enum, EnumEntry}

/** Состояние заказа у нас. Касса знает больше (`OpStateExt` отдаёт семь
  * кодов), но для выдачи дублонов важны только три вещи: деньги ещё в пути,
  * деньги взяты, заказ умер. Сырое состояние кассы при этом сохраняется в
  * `payments.bank_status` — чтобы разбор инцидента не упирался в наше
  * огрубление. */
sealed trait PaymentStatus extends EnumEntry {

  /** Деньги списаны — можно выдавать. */
  def paid: Boolean = this == PaymentStatus.Confirmed

  /** Заказ закрыт и больше ничего не ждёт: ни выдачи, ни опроса. */
  def settled: Boolean = this match {
    case PaymentStatus.New | PaymentStatus.Authorized => false
    case _                                            => true
  }
}

object PaymentStatus extends Enum[PaymentStatus] with DoobieEnum[PaymentStatus] {

  val values = findValues

  /** Заказ создан, оплата не завершена. */
  case object New extends PaymentStatus

  /** Деньги в движении: холд, зачисление магазину, проверка безопасности. Ещё
    * не наши. */
  case object Authorized extends PaymentStatus

  /** Деньги списаны. Единственный статус, по которому выдаём. */
  case object Confirmed extends PaymentStatus

  /** Отмена, отказ в зачислении, просрочка ссылки. */
  case object Rejected extends PaymentStatus

  /** Статус, которого мы не знаем. Выдавать по нему нельзя, а сырое значение
    * лежит в `bank_status`. */
  case object Unknown extends PaymentStatus

  /** Состояние операции из `OpStateExt`. Промежуточные склеиваются с
    * ближайшим нашим: пока деньги в движении, заказ для нас всё ещё в пути.
    * Выдаём только по 100 — это и есть «средства зачислены магазину», ровно по
    * нему касса шлёт ResultURL. */
  def fromRobokassa(code: Int): PaymentStatus = code match {
    case 5             => New
    case 20 | 50 | 80  => Authorized
    case 100           => Confirmed
    case 10 | 60       => Rejected
    case _             => Unknown
  }
}
