package pangea.test

import io.circe.Json
import pangea.model.payment.{DonationSku, OrderId, Payment}
import pangea.model.user.{User, UserId}
import pangea.service.donation.Donations
import zio.{Ref, Task, ZIO}

/** Донат для экранных тестов: ничего не считает, отдаёт заранее заданные
  * исходы и запоминает, о чём его спрашивали. Настоящая логика выдачи
  * проверяется на `DonationsLive` (см. `DonationsSpec`). */
class TestDonations(
  val enabled:     Boolean,
  val packs:       List[DonationSku],
  val offerUrl:    String,
  val linkMinutes: Int,
  pendingRef:      Ref[List[Payment]],
  refreshedRef:    Ref[List[Payment]],
  startRef:        Ref[Donations.Start],
  settleRef:       Ref[Donations.Settle],
  announcedRef:    Ref[List[Payment]],
  startedRef:      Ref[List[DonationSku]]
) extends Donations {

  override def pending(userId: UserId): Task[List[Payment]] = pendingRef.get

  override def latest(userId: UserId): Task[Option[Payment]] = pendingRef.get.map(_.headOption)

  override def find(userId: UserId, orderId: OrderId): Task[Option[Payment]] =
    pendingRef.get.map(_.find(_.orderId == orderId))

  override def start(user: User, sku: DonationSku, email: String, now: Long): Task[Donations.Start] =
    startedRef.update(_ :+ sku) *> startRef.get

  override def refresh(payment: Payment, now: Long): Task[Donations.Settle] =
    refreshedRef.update(_ :+ payment) *> settleRef.get

  override def notified(body: Json, now: Long): Task[Donations.Notified] =
    ZIO.succeed(Donations.Notified.Accepted)

  override def announce(payment: Payment): Task[Unit] = announcedRef.update(_ :+ payment)

  override def reconcile(now: Long): Task[Unit] = ZIO.unit

  /** За какие пакеты игрок пытался заплатить. */
  def started: Task[List[DonationSku]] = startedRef.get

  def announced: Task[List[Payment]] = announcedRef.get

  def setPending(payments: List[Payment]): Task[Unit] = pendingRef.set(payments)

  /** Какие заказы в итоге пошли на опрос — по ним видно, ту ли кнопку нажали. */
  def refreshed: Task[List[Payment]] = refreshedRef.get

  def setSettle(settle: Donations.Settle): Task[Unit] = settleRef.set(settle)
}

object TestDonations {

  /** Донат выключен — как на проде без ключей терминала. */
  val off: Donations = new Donations {
    override val enabled                   = false
    override val packs: List[DonationSku]  = Nil
    override val offerUrl                  = ""
    override val linkMinutes               = 60
    override def pending(userId: UserId)    = ZIO.succeed(List.empty[Payment])
    override def latest(userId: UserId)     = ZIO.none
    override def find(userId: UserId, orderId: OrderId) = ZIO.none
    override def start(user: User, sku: DonationSku, email: String, now: Long) =
      ZIO.succeed(Donations.Start.Disabled)
    override def refresh(payment: Payment, now: Long) = ZIO.succeed(Donations.Settle.Pending)
    override def notified(body: Json, now: Long)      = ZIO.succeed(Donations.Notified.Accepted)
    override def announce(payment: Payment)           = ZIO.unit
    override def reconcile(now: Long)                 = ZIO.unit
  }

  val pack100: DonationSku = DonationSku("d100", 100L, 9900L, "100 дублонов", "100 дублонов")
  val pack550: DonationSku = DonationSku("d550", 550L, 49900L, "550 дублонов", "550 дублонов")

  def on(
    packs:  List[DonationSku] = List(pack100, pack550),
    start:   Donations.Start  = Donations.Start.Unavailable,
    settle:  Donations.Settle = Donations.Settle.Pending,
    pending: List[Payment]    = Nil
  ): Task[TestDonations] =
    for {
      pendingRef   <- Ref.make(pending)
      refreshedRef <- Ref.make(List.empty[Payment])
      startRef     <- Ref.make(start)
      settleRef    <- Ref.make(settle)
      announcedRef <- Ref.make(List.empty[Payment])
      startedRef   <- Ref.make(List.empty[DonationSku])
    } yield new TestDonations(
      enabled     = true,
      packs       = packs,
      offerUrl    = "https://example.test/offer",
      linkMinutes = 60,
      pendingRef, refreshedRef, startRef, settleRef, announcedRef, startedRef
    )
}
