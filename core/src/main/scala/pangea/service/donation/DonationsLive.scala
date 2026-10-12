package pangea.service.donation

import io.circe.Json
import pangea.client.robokassa.{OpState, PaymentRequest, RobokassaApi, RobokassaClient, RobokassaConfig}
import pangea.dao.payment.{GrantResult, PaymentDao}
import pangea.engine.Journal
import pangea.model.GameEvent
import pangea.model.payment.{DonationSku, OrderId, Payment, PaymentStatus}
import pangea.model.user.{User, UserId}
import pangea.repository.user.UserRepository
import pangea.service.sender.Api
import zio.{Random, Task, ZIO}

final case class DonationsLive(
  config:   RobokassaConfig,
  kassa:    RobokassaClient,
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

  /** Новый заказ. Строка вставляется первой: её `id` и есть `InvId` для
    * кассы, а ссылка собирается у нас же, без запроса наружу — поэтому
    * «касса не ответила» на этом шаге не бывает. */
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
      url = kassa.paymentUrl(PaymentRequest(
              invId       = id,
              amount      = sku.price,
              description = sku.receipt,
              email       = email,
              receiptName = sku.receipt,
              dueAtMs     = now + config.linkMinutes.toLong * 60000L
            ))
      _ <- payments.setStarted(orderId, id.toString, url, now)
      _ <- ZIO.logInfo(s"donation: заказ ${orderId.value} (InvId $id) создан, ${sku.rubles} ₽, ${sku.doubloons} дублонов")
    } yield Donations.Start.Link(draft.copy(id = id, paymentId = Some(id.toString), paymentUrl = Some(url)), url)

  /** Номер заказа: читаемый и уникальный. Случайный хвост — на случай двух
    * заказов в одну миллисекунду. */
  private def newOrderId(userId: UserId, now: Long): Task[OrderId] =
    Random.nextIntBetween(1000, 10000).map(salt => OrderId(s"d-${userId.value}-$now-$salt"))

  // ── Опрос и выдача ────────────────────────────────────────────────────────

  override def refresh(payment: Payment, now: Long): Task[Donations.Settle] =
    payment.paymentId.flatMap(_.toLongOption) match {
      // Заказ не дошёл до ссылки — спрашивать не о чем.
      case None => ZIO.succeed(Donations.Settle.Pending)
      case Some(invId) =>
        kassa.opState(invId).either.flatMap {
          case Left(error) =>
            ZIO.logWarning(s"donation: OpStateExt по ${payment.orderId.value} не ответил: ${error.getMessage}")
              .as(Donations.Settle.Pending)

          case Right(OpState.Found(code)) =>
            val status = PaymentStatus.fromRobokassa(code)
            payments.setStatus(payment.orderId, status, s"STATE_$code", now) *> settle(payment, status, now)

          // Операции нет: игрок ещё не ввёл реквизиты или бросил ссылку. Пока
          // она жива — ждём; потом закрываем, иначе сверка гоняла бы брошенный
          // заказ вечно. Если касса всё же примет оплату по «пережившей»
          // ссылке, уведомление выдаст её и по закрытому заказу.
          case Right(OpState.NotFound) =>
            val status = if (payment.linkAlive(now, config.linkMinutes)) PaymentStatus.New else PaymentStatus.Rejected
            payments.setStatus(payment.orderId, status, "NOT_FOUND", now) *> settle(payment, status, now)

          case Right(OpState.Failed(code, description)) =>
            ZIO.logError(s"donation: OpStateExt по ${payment.orderId.value} отказал: $code $description")
              .as(Donations.Settle.Pending)
        }
    }

  /** Выдача по статусу — одна для всех трёх путей: уведомление, кнопка, сверка. */
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

  // ── Уведомление ResultURL ─────────────────────────────────────────────────

  /** ResultURL касса шлёт только по успешной оплате, так что само уведомление с
    * верной подписью и есть «оплачено». Сумму всё равно сверяем: подпись
    * подтверждает, что прислала касса, но не то, что заплачено обещанное. */
  override def notified(params: Map[String, String], now: Long): Task[Donations.Notified] =
    if (!kassa.verify(params))
      ZIO.logError("donation: уведомление с неверной подписью").as(Donations.Notified.BadSignature)
    else
      params.get("InvId") match {
        case None => ZIO.succeed(Donations.Notified.Ignored("нет InvId"))
        case Some(invId) =>
          payments.byPaymentId(invId).flatMap {
            case None =>
              // Чужой или уже вычищенный заказ. Повтор не поможет — закрываем.
              ZIO.logError(s"donation: уведомление по неизвестному счёту $invId")
                .as(Donations.Notified.Ignored("заказ не найден"))

            case Some(payment) if !params.get("OutSum").flatMap(RobokassaApi.kopecks).contains(payment.amountKopecks) =>
              // Подпись верна, а сумма не та: выдавать нельзя, повторять
              // бессмысленно — только разбор руками.
              ZIO.logError(s"donation: по заказу ${payment.orderId.value} пришло OutSum=" +
                s"${params.getOrElse("OutSum", "")} вместо ${payment.amountKopecks} копеек — выдача остановлена")
                .as(Donations.Notified.Ignored("сумма не совпала"))

            case Some(payment) =>
              payments.setStatus(payment.orderId, PaymentStatus.Confirmed, "RESULT_URL", now) *>
                settle(payment, PaymentStatus.Confirmed, now).map {
                  case Donations.Settle.Granted(_) => Donations.Notified.Granted(payment)
                  case _                           => Donations.Notified.Accepted
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
            // `send_failures`, и терять из-за него ответ кассе нельзя.
            ZIO.logWarning(s"donation: письмо о ${payment.orderId.value} не ушло: ${err.getMessage}")
          )
    }

  // ── Сверка ────────────────────────────────────────────────────────────────

  /** Что чинит сверка: потерянное уведомление (деньги взяты, мы не знаем),
    * застрявшую выдачу (знаем, но героя не было) и брошенный заказ (ссылка
    * истекла, а касса так ничего и не увидела). Первое и третье добирает
    * `OpStateExt`, второе — повторный `settle` по уже известному статусу. */
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
