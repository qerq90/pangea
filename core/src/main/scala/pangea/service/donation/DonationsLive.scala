package pangea.service.donation

import io.circe.Json
import pangea.client.tbank.{InitRequest, TBankClient, TBankConfig}
import pangea.dao.payment.{GrantResult, PaymentDao}
import pangea.engine.Journal
import pangea.model.GameEvent
import pangea.model.payment.{DonationSku, OrderId, Payment, PaymentStatus}
import pangea.model.user.{User, UserId}
import pangea.repository.user.UserRepository
import pangea.service.sender.Api
import zio.{Random, Task, ZIO}

final case class DonationsLive(
  config:   TBankConfig,
  bank:     TBankClient,
  payments: PaymentDao,
  api:      Api,
  users:    UserRepository,
  journal:  Journal
) extends Donations {

  override def enabled: Boolean      = config.enabled
  override def packs: List[DonationSku] = config.packs
  override def offerUrl: String      = config.offerUrl
  override def linkMinutes: Int      = config.linkMinutes

  override def pending(userId: UserId): Task[List[Payment]] = payments.pendingOf(userId)

  override def find(userId: UserId, orderId: OrderId): Task[Option[Payment]] =
    payments.byOrderId(orderId).map(_.filter(_.userId == userId))

  override def latest(userId: UserId): Task[Option[Payment]] = payments.lastOf(userId)

  // ── Начало покупки ────────────────────────────────────────────────────────

  override def start(user: User, sku: DonationSku, email: String, now: Long): Task[Donations.Start] =
    if (!config.enabled) ZIO.succeed(Donations.Start.Disabled)
    else
      withinLimit(user.userId, sku, now).flatMap {
        case false => ZIO.succeed(Donations.Start.LimitReached)
        case true =>
          reusable(user.userId, sku, now).flatMap {
            case Some(existing) =>
              // Та же ссылка вместо второго заказа на те же деньги.
              ZIO.succeed(Donations.Start.Link(existing, existing.paymentUrl.getOrElse("")))
            case None => create(user, sku, email, now)
          }
      }

  /** Суточный лимит: считаем только реально списанное. `0` — без ограничения. */
  private def withinLimit(userId: UserId, sku: DonationSku, now: Long): Task[Boolean] =
    if (config.dailyLimit <= 0L) ZIO.succeed(true)
    else
      payments
        .paidSince(userId, now - Donations.DayMs)
        .map(spent => spent + sku.price <= config.dailyLimit)

  /** Начатый и ещё живой заказ на тот же пакет — чтобы вернувшийся на экран
    * игрок получил свою прежнюю ссылку. Ищем среди всех незакрытых, а не
    * только среди последнего: игрок мог начать второй заказ на другой пакет,
    * а потом вернуться к первому. */
  private def reusable(userId: UserId, sku: DonationSku, now: Long): Task[Option[Payment]] =
    payments
      .pendingOf(userId)
      .map(_.find(p => p.sku == sku.id && p.linkAlive(now, config.linkMinutes)))

  private def create(user: User, sku: DonationSku, email: String, now: Long): Task[Donations.Start] =
    for {
      orderId <- newOrderId(user.userId, now)
      draft = Payment(
                id            = 0L,
                orderId       = orderId,
                userId        = user.userId,
                sku           = sku.id,
                amountKopecks = sku.price,
                doubloons     = sku.doubloons,
                receiptEmail  = email,
                paymentId     = None,
                paymentUrl    = None,
                status        = PaymentStatus.New,
                bankStatus    = None,
                granted       = false,
                createdAt     = now,
                updatedAt     = now
              )
      id <- payments.insert(draft)
      request = InitRequest(
                  amount       = sku.price,
                  orderId      = orderId.value,
                  description  = sku.receipt,
                  customerKey  = user.userId.value.toString,
                  receiptEmail = email,
                  receiptName  = sku.receipt
                )
      result <- bank
                  .init(request, now + config.linkMinutes.toLong * 60000L)
                  .either
      start <- result match {
        case Right(response) if response.success && response.paymentUrl.exists(_.nonEmpty) =>
          val paymentId = response.paymentId.getOrElse("")
          val url       = response.paymentUrl.get
          payments.setStarted(orderId, paymentId, url, now) *>
            ZIO.logInfo(s"donation: заказ ${orderId.value} создан, ${sku.rubles} ₽, ${sku.doubloons} дублонов") *>
            ZIO.succeed(Donations.Start.Link(
              draft.copy(id = id, paymentId = Some(paymentId), paymentUrl = Some(url)), url))

        case Right(response) =>
          // Касса ответила отказом: заказ закрываем, денег с игрока не брали.
          payments.setStatus(orderId, PaymentStatus.Rejected, response.status.getOrElse("INIT_FAILED"), now) *>
            ZIO.logError(s"donation: Init отказал по заказу ${orderId.value}: ${response.problem}") *>
            ZIO.succeed(Donations.Start.Unavailable)

        case Left(error) =>
          payments.setStatus(orderId, PaymentStatus.Rejected, "INIT_ERROR", now) *>
            ZIO.logError(s"donation: Init упал по заказу ${orderId.value}: ${error.getMessage}") *>
            ZIO.succeed(Donations.Start.Unavailable)
      }
    } yield start

  /** Номер заказа: читаемый, уникальный и короче 50 символов, как требует
    * банк. Случайный хвост — на случай двух заказов в одну миллисекунду. */
  private def newOrderId(userId: UserId, now: Long): Task[OrderId] =
    Random.nextIntBetween(1000, 10000).map(salt => OrderId(s"d-${userId.value}-$now-$salt"))

  // ── Опрос и выдача ────────────────────────────────────────────────────────

  override def refresh(payment: Payment, now: Long): Task[Donations.Settle] =
    payment.paymentId match {
      // Заказ не доехал до банка — спрашивать не о чем.
      case None => ZIO.succeed(Donations.Settle.Pending)
      case Some(paymentId) =>
        bank.getState(paymentId).either.flatMap {
          case Left(error) =>
            ZIO.logWarning(s"donation: GetState по ${payment.orderId.value} не ответил: ${error.getMessage}")
              .as(Donations.Settle.Pending)
          case Right(response) =>
            val raw    = response.status.getOrElse("UNKNOWN")
            val status = PaymentStatus.fromBank(raw)
            payments.setStatus(payment.orderId, status, raw, now) *> settle(payment, status, now)
        }
    }

  /** Выдача по статусу — одна для всех трёх путей: нотификация, кнопка, сверка. */
  private def settle(payment: Payment, status: PaymentStatus, now: Long): Task[Donations.Settle] =
    if (!status.paid) ZIO.succeed(if (status.settled) Donations.Settle.Failed else Donations.Settle.Pending)
    else
      payments.grantOnce(payment.orderId, payment.userId, payment.doubloons, now).flatMap {
        case GrantResult.AlreadyGranted => ZIO.succeed(Donations.Settle.AlreadyGranted)
        case GrantResult.NoHero =>
          // Игрок удалил персонажа. Заказ остаётся в очереди сверки и дойдёт,
          // когда герой появится снова.
          ZIO.logWarning(s"donation: ${payment.orderId.value} оплачен, но героя нет — выдача отложена")
            .as(Donations.Settle.Pending)
        case GrantResult.Granted =>
          journal.append(GameEvent(payment.userId, "donation_granted", Json.obj(
            "order_id"  -> Json.fromString(payment.orderId.value),
            "sku"       -> Json.fromString(payment.sku),
            "kopecks"   -> Json.fromLong(payment.amountKopecks),
            "doubloons" -> Json.fromLong(payment.doubloons)
          ))) *>
            ZIO.logInfo(s"donation: выдано ${payment.doubloons} дублонов по ${payment.orderId.value}") *>
            ZIO.succeed(Donations.Settle.Granted(payment.doubloons))
      }

  // ── Нотификация банка ─────────────────────────────────────────────────────

  override def notified(body: Json, now: Long): Task[Donations.Notified] =
    body.asObject match {
      case None => ZIO.succeed(Donations.Notified.Ignored("тело не объект"))
      case Some(obj) if !bank.verify(obj) =>
        ZIO.logError("donation: нотификация с неверной подписью").as(Donations.Notified.BadSignature)
      case Some(obj) =>
        val orderId = obj("OrderId").flatMap(_.asString).map(OrderId.apply)
        val raw     = obj("Status").flatMap(_.asString).getOrElse("UNKNOWN")
        val amount  = obj("Amount").flatMap(_.asNumber).flatMap(_.toLong)
        orderId match {
          case None => ZIO.succeed(Donations.Notified.Ignored("нет OrderId"))
          case Some(id) =>
            payments.byOrderId(id).flatMap {
              case None =>
                // Чужой или уже вычищенный заказ. Повтор не поможет — закрываем.
                ZIO.logError(s"donation: нотификация по неизвестному заказу ${id.value}")
                  .as(Donations.Notified.Ignored("заказ не найден"))

              case Some(payment) if amount.exists(_ != payment.amountKopecks) =>
                // Подпись верна, а сумма не та: выдавать нельзя, повторять
                // бессмысленно — только разбор руками.
                ZIO.logError(s"donation: по заказу ${id.value} пришло ${amount.getOrElse(0L)} копеек " +
                  s"вместо ${payment.amountKopecks} — выдача остановлена")
                  .as(Donations.Notified.Ignored("сумма не совпала"))

              case Some(payment) =>
                val status = PaymentStatus.fromBank(raw)
                payments.setStatus(id, status, raw, now) *>
                  settle(payment, status, now).map {
                    case Donations.Settle.Granted(_) => Donations.Notified.Granted(payment)
                    case _                           => Donations.Notified.Accepted
                  }
            }
        }
    }

  // ── Письмо игроку ─────────────────────────────────────────────────────────

  /** Сообщение уходит БЕЗ клавиатуры: игрок в этот момент может стоять в бою
    * или в подземелье, и экран с кнопками затёр бы ему текущий. Экран без
    * кнопок прошлую клавиатуру не трогает (см. `Screen`). */
  override def announce(payment: Payment): Task[Unit] =
    users.getUserById(payment.userId).flatMap {
      case None => ZIO.logWarning(s"donation: некому сообщить о ${payment.orderId.value}")
      case Some(user) =>
        api
          .sendMessage(user, DonationsLive.granted(payment), List.empty, None)
          .catchAll(err =>
            // Дублоны уже на руках; недоставленное письмо само попадёт в
            // `send_failures`, и терять из-за него ответ банку нельзя.
            ZIO.logWarning(s"donation: письмо о ${payment.orderId.value} не ушло: ${err.getMessage}")
          )
    }

  // ── Сверка ────────────────────────────────────────────────────────────────

  /** Что чинит сверка: потерянную нотификацию (деньги взяты, мы не знаем) и
    * застрявшую выдачу (знаем, но героя не было). Первое добирает `GetState`,
    * второе — повторный `settle` по уже известному статусу. */
  override def reconcile(now: Long): Task[Unit] =
    payments.unsettled(now - Donations.ReconcileAfterMs, Donations.ReconcileBatch).flatMap { stuck =>
      ZIO.foreachDiscard(stuck) { payment =>
        val step =
          if (payment.status.paid) settle(payment, payment.status, now)
          else refresh(payment, now)
        step.flatMap {
          case Donations.Settle.Granted(_) => announce(payment)
          case _                           => ZIO.unit
        }.catchAll(err => ZIO.logError(s"donation: сверка ${payment.orderId.value} упала: ${err.getMessage}"))
      }
    }
}

object DonationsLive {

  def granted(payment: Payment): String =
    s"✨ Оплата прошла: зачислено 🟡 ${payment.doubloons} дублонов.\n" +
      s"Чек уйдёт на ${payment.receiptEmail}."
}
