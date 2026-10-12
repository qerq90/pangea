package pangea.service.donation

import pangea.client.robokassa._
import pangea.model.payment.{DonationSku, PaymentStatus}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.test.{TestApi, TestJournal, TestPaymentDao, TestUserRepository}
import zio.{Ref, Task, ZIO}
import zio.test._

/** Донат: выдача дублонов за деньги.
  *
  * Главное, что здесь проверяется, — выдача ровно один раз. Уведомление
  * ResultURL касса может прислать повторно, а кнопка «Проверить оплату» и
  * фоновая сверка тянут за ту же ручку. */
object DonationsSpec extends ZIOSpecDefault {

  private val userId = UserId(7L)
  private val user   = User(userId, VkId("vk_7"), TelegramId("tg_7"), Some("player@mail.ru"))
  private val pack   = DonationSku("d100", 100L, 9900L, "100 дублонов", "100 дублонов")

  private def config(dailyLimit: Long) = RobokassaConfig(
    merchantLogin = "demo",
    password1     = "pass1",
    password2     = "pass2",
    test          = false,
    hash          = HashAlgo.Sha256,
    payUrl        = "https://auth.robokassa.test/Merchant/Index.aspx",
    opStateUrl    = "https://auth.robokassa.test/OpStateExt",
    sendReceipt   = false,
    tax           = "none",
    paymentObject = "service",
    offerUrl      = "https://pangea.test/offer",
    linkMinutes   = 60,
    dailyLimit    = dailyLimit,
    packs         = List(pack)
  )

  /** Касса-заглушка: ссылку и подпись считает по-настоящему, а состояние
    * операции отдаёт заданное. */
  private class StubKassa(cfg: RobokassaConfig, stateRef: Ref[Either[Throwable, OpState]]) extends RobokassaClient {
    override def paymentUrl(request: PaymentRequest): String = RobokassaApi.paymentUrl(cfg, request)
    override def opState(invId: Long): Task[OpState]         = stateRef.get.flatMap(ZIO.fromEither(_))
    override def verify(params: Map[String, String]): Boolean =
      RobokassaSignature.verifyResult(cfg.hash, params, cfg.password2)
    def setState(code: Int): Task[Unit]       = stateRef.set(Right(OpState.Found(code)))
    def setState(state: OpState): Task[Unit]  = stateRef.set(Right(state))
    def setError(error: Throwable): Task[Unit] = stateRef.set(Left(error))
  }

  private case class Kit(
    donations: Donations,
    payments:  TestPaymentDao,
    kassa:     StubKassa,
    api:       TestApi,
    journal:   TestJournal
  )

  private def kit(dailyLimit: Long = 0L, withHero: Boolean = true): Task[Kit] =
    for {
      payments <- if (withHero) TestPaymentDao.withHeroes(userId) else TestPaymentDao.noHeroes
      cfg       = config(dailyLimit)
      stateRef <- Ref.make[Either[Throwable, OpState]](Right(OpState.NotFound))
      kassa     = new StubKassa(cfg, stateRef)
      api      <- TestApi.make
      users    <- TestUserRepository.withUser(user)
      journal  <- TestJournal.make
    } yield Kit(DonationsLive(cfg, kassa, payments, api, users, journal), payments, kassa, api, journal)

  /** Уведомление ResultURL с настоящей подписью — как от кассы. `OutSum` в
    * том виде, в каком его шлёт касса: шесть знаков после точки. */
  private def notification(invId: String, outSum: String = "99.000000"): Map[String, String] =
    Map(
      "OutSum"         -> outSum,
      "InvId"          -> invId,
      "SignatureValue" -> RobokassaSignature.result(HashAlgo.Sha256, outSum, invId, Map.empty, "pass2"),
      "EMail"          -> "player@mail.ru",
      "PaymentMethod"  -> "BankCard"
    )

  private val now = 1760000000000L

  private def orderOf(payments: TestPaymentDao) = payments.all.map(_.head)

  private def invIdOf(payments: TestPaymentDao) = orderOf(payments).map(_.paymentId.get)

  private val linkMs = 60L * 60000L

  override def spec = suite("Донат")(

    suite("создание заказа")(

      test("ссылка на оплату, заказ записан с ценой, объёмом и номером счёта") {
        for {
          k     <- kit()
          start <- k.donations.start(user, pack, "player@mail.ru", now)
          order <- orderOf(k.payments)
          url    = urlOf(start).getOrElse("")
        } yield assertTrue(url.startsWith("https://auth.robokassa.test/Merchant/Index.aspx?")) &&
                // InvId — это id строки заказа: касса требует целое число.
                assertTrue(order.paymentId.contains(order.id.toString)) &&
                assertTrue(url.contains(s"InvId=${order.id}&")) &&
                assertTrue(url.contains("OutSum=99.00&")) &&
                assertTrue(order.amountKopecks == 9900L) &&
                assertTrue(order.doubloons == 100L) &&
                assertTrue(order.receiptEmail == "player@mail.ru") &&
                assertTrue(order.status == PaymentStatus.New) &&
                assertTrue(order.paymentUrl.contains(url)) &&
                assertTrue(!order.granted)
      },

      test("повторный заход за тем же пакетом отдаёт прежнюю ссылку, а не второй заказ") {
        for {
          k      <- kit()
          first  <- k.donations.start(user, pack, "player@mail.ru", now)
          second <- k.donations.start(user, pack, "player@mail.ru", now + 1000L)
          orders <- k.payments.all
        } yield assertTrue(orders.size == 1) && assertTrue(urlOf(first) == urlOf(second))
      },

      test("после истечения ссылки — новый заказ с новым номером счёта") {
        for {
          k      <- kit()
          _      <- k.donations.start(user, pack, "player@mail.ru", now)
          _      <- k.donations.start(user, pack, "player@mail.ru", now + linkMs + 1L)
          orders <- k.payments.all
        } yield assertTrue(orders.size == 2) && assertTrue(orders.flatMap(_.paymentId).distinct.size == 2)
      },

      test("суточный лимит считает только списанное") {
        for {
          k     <- kit(dailyLimit = 10000L)
          _     <- k.donations.start(user, pack, "player@mail.ru", now)
          order <- orderOf(k.payments)
          // Первый заказ ещё не оплачен — лимит не тронут.
          free  <- k.donations.start(user, pack, "player@mail.ru", now)
          _     <- k.payments.setStatus(order.orderId, PaymentStatus.Confirmed, "RESULT_URL", now)
          // А теперь 9900 из 10000 уже списаны, второй пакет не влезает.
          hit   <- k.donations.start(user, pack, "player@mail.ru", now + 1L)
        } yield assertTrue(free.isInstanceOf[Donations.Start.Link]) &&
                assertTrue(hit == Donations.Start.LimitReached)
      }
    ),

    suite("уведомление ResultURL")(

      test("выдаёт дублоны, повтор не выдаёт второй раз") {
        for {
          k      <- kit()
          _      <- k.donations.start(user, pack, "player@mail.ru", now)
          invId  <- invIdOf(k.payments)
          first  <- k.donations.notified(notification(invId), now)
          after1 <- k.payments.doubloonsOf(userId)
          second <- k.donations.notified(notification(invId), now + 10L)
          after2 <- k.payments.doubloonsOf(userId)
          order  <- orderOf(k.payments)
          events <- k.journal.logged
        } yield assertTrue(first.isInstanceOf[Donations.Notified.Granted]) &&
                assertTrue(after1 == 100L) &&
                assertTrue(second == Donations.Notified.Accepted) &&
                assertTrue(after2 == 100L) &&
                assertTrue(order.status == PaymentStatus.Confirmed) &&
                assertTrue(events.count(_.eventType == "donation_granted") == 1)
      },

      test("чужая подпись ничего не выдаёт") {
        for {
          k      <- kit()
          _      <- k.donations.start(user, pack, "player@mail.ru", now)
          invId  <- invIdOf(k.payments)
          forged  = notification(invId) + ("SignatureValue" -> "подделка")
          result <- k.donations.notified(forged, now)
          after  <- k.payments.doubloonsOf(userId)
        } yield assertTrue(result == Donations.Notified.BadSignature) && assertTrue(after == 0L)
      },

      test("сумма не совпала — выдача остановлена, хотя подпись верна") {
        for {
          k      <- kit()
          _      <- k.donations.start(user, pack, "player@mail.ru", now)
          invId  <- invIdOf(k.payments)
          result <- k.donations.notified(notification(invId, outSum = "1.000000"), now)
          after  <- k.payments.doubloonsOf(userId)
        } yield assertTrue(result.isInstanceOf[Donations.Notified.Ignored]) && assertTrue(after == 0L)
      },

      test("неизвестный счёт закрываем: повтор его не вылечит") {
        for {
          k      <- kit()
          result <- k.donations.notified(notification("999"), now)
        } yield assertTrue(result.isInstanceOf[Donations.Notified.Ignored])
      },

      test("оплата по истёкшему и уже закрытому заказу всё равно выдаётся") {
        for {
          k      <- kit()
          _      <- k.donations.start(user, pack, "player@mail.ru", now)
          invId  <- invIdOf(k.payments)
          // Сверка закрыла заказ: ссылка истекла, касса операции не видела.
          _      <- k.donations.reconcile(now + linkMs + 1L)
          closed <- orderOf(k.payments)
          // А игрок успел заплатить — касса держит ссылку по своему часу.
          result <- k.donations.notified(notification(invId), now + linkMs + 2L)
          after  <- k.payments.doubloonsOf(userId)
        } yield assertTrue(closed.status == PaymentStatus.Rejected) &&
                assertTrue(result.isInstanceOf[Donations.Notified.Granted]) &&
                assertTrue(after == 100L)
      }
    ),

    suite("проверка оплаты и сверка")(

      test("«Проверить оплату» выдаёт один раз, второй раз — «уже выдано»") {
        for {
          k      <- kit()
          _      <- k.donations.start(user, pack, "player@mail.ru", now)
          order  <- orderOf(k.payments)
          _      <- k.kassa.setState(100)
          first  <- k.donations.refresh(order, now)
          fresh  <- orderOf(k.payments)
          second <- k.donations.refresh(fresh, now + 10L)
          after  <- k.payments.doubloonsOf(userId)
        } yield assertTrue(first == Donations.Settle.Granted(100L)) &&
                assertTrue(second == Donations.Settle.AlreadyGranted) &&
                assertTrue(after == 100L)
      },

      test("отмена операции закрывает заказ") {
        for {
          k     <- kit()
          _     <- k.donations.start(user, pack, "player@mail.ru", now)
          order <- orderOf(k.payments)
          _     <- k.kassa.setState(10)
          res   <- k.donations.refresh(order, now)
          fresh <- orderOf(k.payments)
          after <- k.payments.doubloonsOf(userId)
        } yield assertTrue(res == Donations.Settle.Failed) &&
                assertTrue(fresh.bankStatus.contains("STATE_10")) &&
                assertTrue(after == 0L)
      },

      test("деньги в пути (холд, зачисление) — ещё не выдаём") {
        for {
          k     <- kit()
          _     <- k.donations.start(user, pack, "player@mail.ru", now)
          order <- orderOf(k.payments)
          _     <- k.kassa.setState(50)
          res   <- k.donations.refresh(order, now)
          after <- k.payments.doubloonsOf(userId)
        } yield assertTrue(res == Donations.Settle.Pending) && assertTrue(after == 0L)
      },

      test("операции ещё нет: пока ссылка жива — ждём, потом заказ закрыт") {
        for {
          k     <- kit()
          _     <- k.donations.start(user, pack, "player@mail.ru", now)
          order <- orderOf(k.payments)
          alive <- k.donations.refresh(order, now + 1000L)
          dead  <- k.donations.refresh(order, now + linkMs + 1L)
          fresh <- orderOf(k.payments)
        } yield assertTrue(alive == Donations.Settle.Pending) &&
                assertTrue(dead == Donations.Settle.Failed) &&
                assertTrue(fresh.status == PaymentStatus.Rejected)
      },

      test("касса не ответила или ответила ошибкой — заказ не трогаем") {
        for {
          k      <- kit()
          _      <- k.donations.start(user, pack, "player@mail.ru", now)
          order  <- orderOf(k.payments)
          _      <- k.kassa.setError(RobokassaError.Transport("нет связи"))
          down   <- k.donations.refresh(order, now + linkMs + 1L)
          _      <- k.kassa.setState(OpState.Failed(1, "Неверная цифровая подпись запроса"))
          broken <- k.donations.refresh(order, now + linkMs + 1L)
          fresh  <- orderOf(k.payments)
        } yield assertTrue(down == Donations.Settle.Pending) &&
                assertTrue(broken == Donations.Settle.Pending) &&
                assertTrue(fresh.status == PaymentStatus.New)
      },

      test("героя нет — выдача откладывается и доходит, когда персонаж появится") {
        for {
          k      <- kit(withHero = false)
          _      <- k.donations.start(user, pack, "player@mail.ru", now)
          invId  <- invIdOf(k.payments)
          paid   <- k.donations.notified(notification(invId), now)
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

      test("сверка добирает потерянное уведомление через OpStateExt") {
        for {
          k     <- kit()
          _     <- k.donations.start(user, pack, "player@mail.ru", now)
          _     <- k.kassa.setState(100)
          _     <- k.donations.reconcile(now + Donations.ReconcileAfterMs + 1L)
          after <- k.payments.doubloonsOf(userId)
          sent  <- k.api.sentMessages
        } yield assertTrue(after == 100L) && assertTrue(sent.size == 1)
      },

      test("свежий заказ сверка не трогает: уведомление ещё в пути") {
        for {
          k     <- kit()
          _     <- k.donations.start(user, pack, "player@mail.ru", now)
          _     <- k.kassa.setState(100)
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
