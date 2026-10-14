package pangea.model.payment

import enumeratum.{DoobieEnum, Enum, EnumEntry}

/** Состояние заказа у нас. Банк знает больше статусов (`AUTHORIZING`,
  * `CONFIRMING`, `REVERSING` и прочие промежуточные), но для выдачи дублонов
  * важны только три вещи: деньги ещё в пути, деньги взяты, заказ умер. Сырой
  * статус банка при этом сохраняется в `payments.bank_status` — чтобы разбор
  * инцидента не упирался в наше огрубление. */
sealed trait PaymentStatus extends EnumEntry {

  /** Деньги списаны — можно выдавать. */
  def paid: Boolean = this == PaymentStatus.Confirmed

  /** Заказ закрыт и больше ничего не ждёт: ни выдачи, ни опроса. */
  def settled: Boolean = this match {
    case PaymentStatus.New | PaymentStatus.FormShowed | PaymentStatus.Authorized => false
    case _                                                                       => true
  }
}

object PaymentStatus extends Enum[PaymentStatus] with DoobieEnum[PaymentStatus] {

  val values = findValues

  /** Заказ создан, игрок ещё не на форме. */
  case object New extends PaymentStatus

  /** Форма открыта, оплата не завершена. */
  case object FormShowed extends PaymentStatus

  /** Деньги захолдированы (двухстадийная оплата) — ещё не наши. */
  case object Authorized extends PaymentStatus

  /** Деньги списаны. Единственный статус, по которому выдаём. */
  case object Confirmed extends PaymentStatus

  /** Отказ, отмена, просрочка ссылки, кончились попытки. */
  case object Rejected extends PaymentStatus

  /** Возврат — полный или частичный. Дублоны при этом не отбираем (см.
    * `Donations`), но статус фиксируем и шумим в лог. */
  case object Refunded extends PaymentStatus

  /** Статус, которого мы не знаем. Выдавать по нему нельзя, а сырое значение
    * лежит в `bank_status`. */
  case object Unknown extends PaymentStatus

  /** Статус из ответа банка. Промежуточные склеиваются с ближайшим нашим: пока
    * деньги в движении, заказ для нас всё ещё в пути. */
  def fromBank(raw: String): PaymentStatus = raw.trim.toUpperCase match {
    case "NEW"                                               => New
    case "FORM_SHOWED" | "AUTHORIZING" | "3DS_CHECKING"      => FormShowed
    case "AUTHORIZED" | "CONFIRMING"                         => Authorized
    case "CONFIRMED"                                         => Confirmed
    case "REJECTED" | "DEADLINE_EXPIRED" | "CANCELED" |
         "ATTEMPTS_EXPIRED" | "REVERSED" | "PARTIAL_REVERSED" => Rejected
    case "REFUNDED" | "PARTIAL_REFUNDED" | "REFUNDING"       => Refunded
    case _                                                   => Unknown
  }
}
