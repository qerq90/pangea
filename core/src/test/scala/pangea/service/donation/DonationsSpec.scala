package pangea.service.donation

import io.circe.{Json, JsonObject}
import pangea.client.tbank._
import pangea.model.payment.{DonationSku, PaymentStatus}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.test.{TestApi, TestJournal, TestPaymentDao, TestUserRepository}
import zio.{Ref, Task, ZIO}
import zio.test._

/** Донат: выдача дублонов за деньги.
  *
  * Главное, что здесь проверяется, — выдача ровно один раз. `CONFIRMED`
  * приходит минимум дважды (при одностадийной оплате вместе с `AUTHORIZED`), а
  * если мы не ответили `OK`, банк переотправляет нотификацию сутками. Плюс
  * кнопка «Проверить оплату» и фоновая сверка тянут за ту же ручку. */
object DonationsSpec extends ZIOSpecDefault {

  private val userId = UserId(7L)
  private val user   = User(userId, VkId("vk_7"), TelegramId("tg_7"), Some("player@mail.ru"))
  private val pack   = DonationSku("d100", 100L, 9900L, "100 дублонов", "100 дублонов")

  private def config(dailyLimit: Long) = TBankConfig(
    terminalKey     = "TERM",
    password        = "secret",
    apiUrl          = "https://securepay.test/v2",
    notificationUrl = "https://pangea.test/pay/tbank/notify",
    successUrl      = "https://vk.com/im?sel=-1",
    failUrl         = "https://vk.com/im?sel=-1",
    taxation        = "usn_income",
    tax             = "none",
    paymentObject   = "service",
    offerUrl        = "https://pangea.test/offer",
    linkMinutes     = 60,
    dailyLimit      = dailyLimit,
    packs           = List(pack)
  )

  private val okInit = InitResponse(
    success = true, status = Some("NEW"), paymentId = Some("pay-1"),
    paymentUrl = Some("https://securepay.test/form/1"),
    errorCode = None, message = None, details = None
  )

  /** Касса-заглушка: отдаёт заданные ответы и считает вызовы `Init`. */
  private class StubBank(
    initRef:  Ref[Either[Throwable, InitResponse]],
    stateRef: Ref[GetStateResponse],
    callsRef: Ref[Int],
    password: String
  ) extends TBankClient {
    override def init(request: InitRequest, dueAtMs: Long): Task[InitResponse] =
      callsRef.update(_ + 1) *> initRef.get.flatMap(ZIO.fromEither(_))
    override def getState(paymentId: String): Task[GetStateResponse] = stateRef.get
    override def verify(body: JsonObject): Boolean                   = TBankToken.verify(body, password)
    def initCalls: Task[Int]                                         = callsRef.get
    def setState(status: String): Task[Unit] =
      stateRef.set(GetStateResponse(success = true, status = Some(status), amount = Some(pack.price),
        errorCode = None, message = None))
    def setInit(result: Either[Throwable, InitResponse]): Task[Unit] = initRef.set(result)
  }

  private case class Kit(
    donations: Donations,
    payments:  TestPaymentDao,
    bank:      StubBank,
    api:       TestApi,
    journal:   TestJournal
  )

  private def kit(dailyLimit: Long = 0L, withHero: Boolean = true): Task[Kit] =
    for {
      payments <- if (withHero) TestPaymentDao.withHeroes(userId) else TestPaymentDao.noHeroes
      initRef  <- Ref.make[Either[Throwable, InitResponse]](Right(okInit))
      stateRef <- Ref.make(GetStateResponse(success = true, status = Some("NEW"),
                    amount = Some(pack.price), errorCode = None, message = None))
      calls    <- Ref.make(0)
      bank      = new StubBank(initRef, stateRef, calls, "secret")
      api      <- TestApi.make
      users    <- TestUserRepository.withUser(user)
      journal  <- TestJournal.make
      cfg       = config(dailyLimit)
    } yield Kit(DonationsLive(cfg, bank, payments, api, users, journal), payments, bank, api, journal)

  /** Нотификация с настоящей подписью — как от банка. */
  private def notification(orderId: String, status: String, amount: Long = pack.price): Json = {
    val body = JsonObject(
      "TerminalKey" -> Json.fromString("TERM"),
      "OrderId"     -> Json.fromString(orderId),
      "Success"     -> Json.fromBoolean(true),
      "Status"      -> Json.fromString(status),
      "PaymentId"   -> Json.fromString("pay-1"),
      "ErrorCode"   -> Json.fromString("0"),
      "Amount"      -> Json.fromLong(amount)
    )
    Json.fromJsonObject(body.add("Token", Json.fromString(TBankToken.sign(body, "secret"))))
  }

  private val now = 1760000000000L

  private def orderOf(payments: TestPaymentDao) = payments.all.map(_.head)

  override def spec = suite("Донат")(

    suite("создание заказа")(

      test("ссылка на оплату, заказ записан с ценой и объёмом из пакета") {
        for {
          k     <- kit()
          start <- k.donations.start(user, pack, "player@mail.ru", now)
          order <- orderOf(k.payments)
        } yield assertTrue(urlOf(start).contains("https://securepay.test/form/1")) &&
                assertTrue(order.amountKopecks == 9900L) &&
                assertTrue(order.doubloons == 100L) &&
                assertTrue(order.receiptEmail == "player@mail.ru") &&
                assertTrue(order.status == PaymentStatus.New) &&
                assertTrue(order.paymentUrl.contains("https://securepay.test/form/1")) &&
                assertTrue(!order.granted)
      },

      test("повторный заход за тем же пакетом отдаёт прежнюю ссылку, а не второй заказ") {
        for {
          k      <- kit()
          first  <- k.donations.start(user, pack, "player@mail.ru", now)
          second <- k.donations.start(user, pack, "player@mail.ru", now + 1000L)
          orders <- k.payments.all
          calls  <- k.bank.initCalls
        } yield assertTrue(orders.size == 1) &&
                assertTrue(calls == 1) &&
                assertTrue(urlOf(first) == urlOf(second))
      },

      test("касса не ответила — заказ закрыт, игроку «попробуйте позже»") {
        for {
          k     <- kit()
          _     <- k.bank.setInit(Left(TBankError.Transport("нет связи")))
          start <- k.donations.start(user, pack, "player@mail.ru", now)
          order <- orderOf(k.payments)
        } yield assertTrue(start == Donations.Start.Unavailable) &&
                assertTrue(order.status == PaymentStatus.Rejected) &&
                assertTrue(!order.granted)
      },

      test("суточный лимит считает только списанное") {
        for {
          k     <- kit(dailyLimit = 10000L)
          _     <- k.donations.start(user, pack, "player@mail.ru", now)
          order <- orderOf(k.payments)
          // Первый заказ ещё не оплачен — лимит не тронут.
          free  <- k.donations.start(user, pack, "player@mail.ru", now)
          _     <- k.payments.setStatus(order.orderId, PaymentStatus.Confirmed, "CONFIRMED", now)
          // А теперь 9900 из 10000 уже списаны, второй пакет не влезает.
          hit   <- k.donations.start(user, pack, "player@mail.ru", now + 1L)
        } yield assertTrue(free.isInstanceOf[Donations.Start.Link]) &&
                assertTrue(hit == Donations.Start.LimitReached)
      }
    ),

    suite("нотификация")(

      test("CONFIRMED выдаёт дублоны, повтор не выдаёт второй раз") {
        for {
          k      <- kit()
          _      <- k.donations.start(user, pack, "player@mail.ru", now)
          order  <- orderOf(k.payments)
          first  <- k.donations.notified(notification(order.orderId.value, "CONFIRMED"), now)
          after1 <- k.payments.doubloonsOf(userId)
          second <- k.donations.notified(notification(order.orderId.value, "CONFIRMED"), now + 10L)
          after2 <- k.payments.doubloonsOf(userId)
          events <- k.journal.logged
        } yield assertTrue(first.isInstanceOf[Donations.Notified.Granted]) &&
                assertTrue(after1 == 100L) &&
                assertTrue(second == Donations.Notified.Accepted) &&
                assertTrue(after2 == 100L) &&
                assertTrue(events.count(_.eventType == "donation_granted") == 1)
      },

      test("AUTHORIZED не выдаёт: при одностадийной оплате он приходит вместе с CONFIRMED") {
        for {
          k     <- kit()
          _     <- k.donations.start(user, pack, "player@mail.ru", now)
          order <- orderOf(k.payments)
          auth  <- k.donations.notified(notification(order.orderId.value, "AUTHORIZED"), now)
          mid   <- k.payments.doubloonsOf(userId)
          _     <- k.donations.notified(notification(order.orderId.value, "CONFIRMED"), now + 1L)
          after <- k.payments.doubloonsOf(userId)
        } yield assertTrue(auth == Donations.Notified.Accepted) &&
                assertTrue(mid == 0L) &&
                assertTrue(after == 100L)
      },

      test("чужая подпись ничего не выдаёт") {
        for {
          k     <- kit()
          _     <- k.donations.start(user, pack, "player@mail.ru", now)
          order <- orderOf(k.payments)
          forged = Json.fromJsonObject(JsonObject(
                     "OrderId" -> Json.fromString(order.orderId.value),
                     "Status"  -> Json.fromString("CONFIRMED"),
                     "Amount"  -> Json.fromLong(pack.price),
                     "Token"   -> Json.fromString("подделка")
                   ))
          result <- k.donations.notified(forged, now)
          after  <- k.payments.doubloonsOf(userId)
        } yield assertTrue(result == Donations.Notified.BadSignature) && assertTrue(after == 0L)
      },

      test("сумма не совпала — выдача остановлена, хотя подпись верна") {
        for {
          k      <- kit()
          _      <- k.donations.start(user, pack, "player@mail.ru", now)
          order  <- orderOf(k.payments)
          result <- k.donations.notified(notification(order.orderId.value, "CONFIRMED", amount = 100L), now)
          after  <- k.payments.doubloonsOf(userId)
        } yield assertTrue(result.isInstanceOf[Donations.Notified.Ignored]) && assertTrue(after == 0L)
      },

      test("неизвестный заказ закрываем: повтор его не вылечит") {
        for {
          k      <- kit()
          result <- k.donations.notified(notification("нет-такого", "CONFIRMED"), now)
        } yield assertTrue(result.isInstanceOf[Donations.Notified.Ignored])
      }
    ),

    suite("проверка оплаты и сверка")(

      test("«Проверить оплату» выдаёт один раз, второй раз — «уже выдано»") {
        for {
          k      <- kit()
          _      <- k.donations.start(user, pack, "player@mail.ru", now)
          order  <- orderOf(k.payments)
          _      <- k.bank.setState("CONFIRMED")
          first  <- k.donations.refresh(order, now)
          fresh  <- orderOf(k.payments)
          second <- k.donations.refresh(fresh, now + 10L)
          after  <- k.payments.doubloonsOf(userId)
        } yield assertTrue(first == Donations.Settle.Granted(100L)) &&
                assertTrue(second == Donations.Settle.AlreadyGranted) &&
                assertTrue(after == 100L)
      },

      test("отказ банка закрывает заказ") {
        for {
          k     <- kit()
          _     <- k.donations.start(user, pack, "player@mail.ru", now)
          order <- orderOf(k.payments)
          _     <- k.bank.setState("REJECTED")
          res   <- k.donations.refresh(order, now)
          after <- k.payments.doubloonsOf(userId)
        } yield assertTrue(res == Donations.Settle.Failed) && assertTrue(after == 0L)
      },

      test("героя нет — выдача откладывается и доходит, когда персонаж появится") {
        for {
          k      <- kit(withHero = false)
          _      <- k.donations.start(user, pack, "player@mail.ru", now)
          order  <- orderOf(k.payments)
          paid   <- k.donations.notified(notification(order.orderId.value, "CONFIRMED"), now)
          stuck  <- orderOf(k.payments)
          // Игрок заново создал персонажа, и сверка добирает оплаченное.
          _      <- k.payments.addHero(userId)
          _      <- k.donations.reconcile(now + Donations.ReconcileAfterMs + 1L)
          after  <- k.payments.doubloonsOf(userId)
          sent   <- k.api.sentMessages
        } yield assertTrue(paid == Donations.Notified.Accepted) &&
                assertTrue(!stuck.granted) &&
                assertTrue(stuck.status == PaymentStatus.Confirmed) &&
                assertTrue(after == 100L) &&
                assertTrue(sent.size == 1)
      },

      test("сверка добирает потерянную нотификацию через GetState") {
        for {
          k     <- kit()
          _     <- k.donations.start(user, pack, "player@mail.ru", now)
          _     <- k.bank.setState("CONFIRMED")
          _     <- k.donations.reconcile(now + Donations.ReconcileAfterMs + 1L)
          after <- k.payments.doubloonsOf(userId)
          sent  <- k.api.sentMessages
        } yield assertTrue(after == 100L) && assertTrue(sent.size == 1)
      },

      test("свежий заказ сверка не трогает: нотификация ещё в пути") {
        for {
          k     <- kit()
          _     <- k.donations.start(user, pack, "player@mail.ru", now)
          _     <- k.bank.setState("CONFIRMED")
          _     <- k.donations.reconcile(now)
          after <- k.payments.doubloonsOf(userId)
        } yield assertTrue(after == 0L)
      }
    ),

    test("письмо о зачислении уходит без клавиатуры: игрок может быть в бою") {
      for {
        k     <- kit()
        _     <- k.donations.start(user, pack, "player@mail.ru", now)
        order <- orderOf(k.payments)
        _     <- k.donations.announce(order)
        sent  <- k.api.sentMessages
      } yield assertTrue(sent.size == 1) &&
              assertTrue(sent.head._2.isEmpty) &&
              assertTrue(sent.head._1.contains("100"))
    }
  )

  private def urlOf(start: Donations.Start): Option[String] = start match {
    case Donations.Start.Link(_, url) => Some(url)
    case _                            => None
  }
}
