package pangea.test

import pangea.dao.payment.{GrantResult, PaymentDao}
import pangea.model.payment.{OrderId, Payment, PaymentStatus}
import pangea.model.user.UserId
import zio.{Ref, Task, ZIO}

/** Платежи в памяти. Отдельно держим «кошели героев»: `grantOnce` в проде
  * начисляет дублоны той же транзакцией, что ставит флаг выдачи, и тесты
  * должны видеть ровно это — иначе идемпотентность проверить нечем. */
class TestPaymentDao(
  rowsRef:  Ref[Map[String, Payment]],
  nextRef:  Ref[Long],
  heroesRef: Ref[Map[UserId, Long]]
) extends PaymentDao {

  override def insert(payment: Payment): Task[Long] =
    for {
      id <- nextRef.getAndUpdate(_ + 1L)
      _  <- rowsRef.update(_.updated(payment.orderId.value, payment.copy(id = id)))
    } yield id

  override def setStarted(orderId: OrderId, paymentId: String, paymentUrl: String, updatedAt: Long): Task[Unit] =
    patch(orderId)(_.copy(paymentId = Some(paymentId), paymentUrl = Some(paymentUrl), updatedAt = updatedAt))

  override def setStatus(orderId: OrderId, status: PaymentStatus, bankStatus: String, updatedAt: Long): Task[Unit] =
    patch(orderId)(_.copy(status = status, bankStatus = Some(bankStatus), updatedAt = updatedAt))

  override def byOrderId(orderId: OrderId): Task[Option[Payment]] =
    rowsRef.get.map(_.get(orderId.value))

  override def pendingOf(userId: UserId): Task[List[Payment]] =
    rowsRef.get.map(
      _.values
        .filter(p => p.userId == userId && !p.status.settled)
        .toList
        .sortBy(-_.createdAt)
    )

  override def lastOf(userId: UserId): Task[Option[Payment]] =
    rowsRef.get.map(_.values.filter(_.userId == userId).toList.sortBy(-_.createdAt).headOption)

  override def unsettled(olderThan: Long, limit: Int): Task[List[Payment]] =
    rowsRef.get.map(
      _.values
        .filter(p => !p.granted && p.updatedAt <= olderThan && (!p.status.settled || p.status.paid))
        .toList
        .sortBy(_.updatedAt)
        .take(limit)
    )

  override def paidSince(userId: UserId, since: Long): Task[Long] =
    rowsRef.get.map(
      _.values
        .filter(p => p.userId == userId && p.createdAt >= since && p.status.paid)
        .map(_.amountKopecks)
        .sum
    )

  override def grantOnce(orderId: OrderId, userId: UserId, doubloons: Long, updatedAt: Long): Task[GrantResult] =
    rowsRef.get.map(_.get(orderId.value)).flatMap {
      case None                    => ZIO.succeed(GrantResult.AlreadyGranted)
      case Some(row) if row.granted => ZIO.succeed(GrantResult.AlreadyGranted)
      case Some(_) =>
        heroesRef.get.map(_.contains(userId)).flatMap {
          case false => ZIO.succeed(GrantResult.NoHero)
          case true =>
            patch(orderId)(_.copy(granted = true, updatedAt = updatedAt)) *>
              heroesRef.update(m => m.updated(userId, m.getOrElse(userId, 0L) + doubloons)) *>
              ZIO.succeed(GrantResult.Granted)
        }
    }

  /** Сколько дублонов начислено игроку через выдачу. */
  def doubloonsOf(userId: UserId): Task[Long] = heroesRef.get.map(_.getOrElse(userId, 0L))

  /** Герой появился — например, игрок заново создал персонажа после рестарта. */
  def addHero(userId: UserId): Task[Unit] = heroesRef.update(_.updated(userId, 0L))

  def all: Task[List[Payment]] = rowsRef.get.map(_.values.toList.sortBy(_.id))

  private def patch(orderId: OrderId)(f: Payment => Payment): Task[Unit] =
    rowsRef.update(m => m.get(orderId.value).fold(m)(row => m.updated(orderId.value, f(row))))
}

object TestPaymentDao {

  /** Пустая база платежей, где у перечисленных игроков есть герой. */
  def withHeroes(userIds: UserId*): Task[TestPaymentDao] =
    for {
      rows   <- Ref.make(Map.empty[String, Payment])
      next   <- Ref.make(1L)
      heroes <- Ref.make(userIds.map(_ -> 0L).toMap)
    } yield new TestPaymentDao(rows, next, heroes)

  /** База без героев вовсе — для случая «оплатил и удалил персонажа». */
  def noHeroes: Task[TestPaymentDao] = withHeroes()
}
