package pangea.service.donation

import io.circe.Json
import pangea.client.tbank.{TBankClient, TBankConfig}
import pangea.dao.payment.PaymentDao
import pangea.engine.Journal
import pangea.model.payment.{DonationSku, Payment}
import pangea.model.user.{User, UserId}
import pangea.repository.user.UserRepository
import pangea.service.sender.Api
import zio.{Task, ZLayer}

/** Донат: продажа дублонов за рубли через кассу Т-Банка.
  *
  * Единственная точка, где дублоны начисляются за деньги, и единственная, где
  * этот путь идемпотентен. Выдача идёт только по статусу `CONFIRMED` и только
  * через [[PaymentDao.grantOnce]], потому что приходит она отовсюду сразу:
  * нотификация банка (минимум дважды), кнопка «Проверить оплату» и фоновая
  * сверка могут сработать одновременно. */
trait Donations {

  /** Включён ли донат: есть ключи терминала и непустой прайс-лист. */
  def enabled: Boolean

  def packs: List[DonationSku]

  def offerUrl: String

  def linkMinutes: Int

  /** Незакрытый заказ игрока, если есть. */
  def active(userId: UserId): Task[Option[Payment]]

  /** Заказ, о котором стоит ответить на «Проверить оплату»: незакрытый или
    * только что оплаченный. Отличается от [[active]] тем, что уже выданный
    * заказ отсюда виден — иначе игрок, нажавший кнопку после начисления,
    * услышал бы, что платежей за ним не числится. */
  def latest(userId: UserId): Task[Option[Payment]]

  /** Начать покупку: вернуть ссылку на оплату. */
  def start(user: User, sku: DonationSku, email: String, now: Long): Task[Donations.Start]

  /** Спросить банк о судьбе заказа и, если оплачен, выдать. За этим стоит
    * кнопка «Проверить оплату». */
  def refresh(payment: Payment, now: Long): Task[Donations.Settle]

  /** Нотификация банка: проверить подпись, обновить статус, выдать. */
  def notified(body: Json, now: Long): Task[Donations.Notified]

  /** Сообщить игроку о зачислении. Отдельно от выдачи: кнопка «Проверить
    * оплату» рисует свой экран, а вебхуку и сверке нужно именно письмо. */
  def announce(payment: Payment): Task[Unit]

  /** Один проход сверки: добрать потерянные нотификации и застрявшие выдачи. */
  def reconcile(now: Long): Task[Unit]
}

object Donations {

  /** Чем кончилась попытка начать покупку. */
  sealed trait Start
  object Start {

    /** Ссылка на оплату — новая или та же, если заказ уже был начат. */
    final case class Link(payment: Payment, url: String) extends Start

    /** Суточный лимит исчерпан. */
    case object LimitReached extends Start

    /** Касса не ответила. Заказ закрыт, деньги не тронуты. */
    case object Unavailable extends Start

    /** Донат выключен конфигом. */
    case object Disabled extends Start
  }

  /** Чем кончился опрос заказа. */
  sealed trait Settle
  object Settle {

    /** Выдали именно сейчас. */
    final case class Granted(doubloons: Long) extends Settle

    /** Оплачено и выдано раньше. */
    case object AlreadyGranted extends Settle

    /** Деньги ещё в пути. */
    case object Pending extends Settle

    /** Заказ умер: отказ, отмена, просрочка. */
    case object Failed extends Settle
  }

  /** Чем кончилась обработка нотификации. На всё, кроме испорченной подписи,
    * банку надо ответить `OK`: повтор такие случаи не вылечит, а слать их он
    * будет месяц. */
  sealed trait Notified
  object Notified {
    final case class Granted(payment: Payment) extends Notified
    case object Accepted                       extends Notified
    case object BadSignature                   extends Notified
    final case class Ignored(reason: String)   extends Notified
  }

  /** Сутки для суточного лимита. */
  val DayMs: Long = 24L * 60L * 60L * 1000L

  /** Насколько заказ должен «отстояться», прежде чем его трогает сверка:
    * нотификация обычно приходит за секунды, и гоняться за ней наперегонки
    * незачем. */
  val ReconcileAfterMs: Long = 5L * 60L * 1000L

  val ReconcileBatch: Int = 32

  val live: ZLayer[
    TBankConfig with TBankClient with PaymentDao with Api with UserRepository with Journal,
    Nothing,
    Donations
  ] =
    ZLayer.fromFunction(DonationsLive(_, _, _, _, _, _))
}
