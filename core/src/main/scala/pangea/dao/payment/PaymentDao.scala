package pangea.dao.payment

import doobie.util.transactor
import pangea.model.payment.{OrderId, Payment, PaymentStatus}
import pangea.model.user.UserId
import zio.{Task, ZLayer}

/** Итог попытки выдать дублоны по заказу. */
sealed trait GrantResult
object GrantResult {

  /** Выдали именно сейчас — заказ был не выдан, и начисление прошло. */
  case object Granted extends GrantResult

  /** Уже было выдано раньше. Нормальный исход: `CONFIRMED` приходит не один
    * раз, и каждый повтор обязан быть безвредным. */
  case object AlreadyGranted extends GrantResult

  /** Героя нет — игрок удалил персонажа (рестарт). Начислять некому, флаг
    * выдачи не ставим: заказ остаётся в очереди сверки и дойдёт до игрока,
    * когда герой появится снова. */
  case object NoHero extends GrantResult
}

trait PaymentDao {

  def insert(payment: Payment): Task[Long]

  /** Запомнить, что заказ принят банком: его id и ссылку на оплату. Ссылку
    * храним, чтобы игрок, ушедший с экрана и вернувшийся, получил ту же —
    * а не второй заказ на те же деньги. */
  def setStarted(orderId: OrderId, paymentId: String, paymentUrl: String, updatedAt: Long): Task[Unit]

  /** Обновить статус. `bankStatus` — сырое значение от банка, для разбора
    * инцидентов. */
  def setStatus(orderId: OrderId, status: PaymentStatus, bankStatus: String, updatedAt: Long): Task[Unit]

  def byOrderId(orderId: OrderId): Task[Option[Payment]]

  /** Все незакрытые заказы игрока, новые первыми. Их может быть несколько:
    * игрок начал покупку, передумал и выбрал другой пакет — ссылка на первый
    * заказ живёт свой час и остаётся оплачиваемой, поэтому показывать надо
    * все, а не только последний. */
  def pendingOf(userId: UserId): Task[List[Payment]]

  /** Самый свежий заказ игрока в любом статусе. Нужен кнопке «Проверить
    * оплату»: к моменту нажатия нотификация могла уже всё выдать и заказ
    * перестал быть активным — но игроку надо ответить «уже зачислено», а не
    * «платежей не числится». */
  def lastOf(userId: UserId): Task[Option[Payment]]

  /** Что ждёт сверки: незакрытые заказы и оплаченные, но ещё не выданные
    * (герой пропал, упала отправка). Старше `olderThan`, чтобы не гоняться за
    * платежами, по которым нотификация ещё в пути. */
  def unsettled(olderThan: Long, limit: Int): Task[List[Payment]]

  /** Сколько денег у игрока реально списано за период — для суточного лимита. */
  def paidSince(userId: UserId, since: Long): Task[Long]

  /** Выдать дублоны ровно один раз.
    *
    * Флаг выдачи и начисление идут одной транзакцией: иначе падение между
    * ними либо потеряет оплаченные дублоны, либо начислит их дважды. */
  def grantOnce(orderId: OrderId, userId: UserId, doubloons: Long, updatedAt: Long): Task[GrantResult]
}

object PaymentDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, PaymentDaoLive] =
    ZLayer.fromFunction(new PaymentDaoLive(_))
}
