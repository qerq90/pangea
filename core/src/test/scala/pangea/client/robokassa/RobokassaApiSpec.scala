package pangea.client.robokassa

import pangea.model.payment.{DonationSku, Payment}
import pureconfig.ConfigSource
import pureconfig.generic.auto._
import zio.ZIO
import zio.test._

import java.io.File
import java.net.URLDecoder
import java.nio.charset.StandardCharsets

object RobokassaApiSpec extends ZIOSpecDefault {

  private val config = RobokassaConfig(
    merchantLogin = "demo",
    password1     = "pass1",
    password2     = "pass2",
    test          = false,
    hash          = HashAlgo.Sha256,
    payUrl        = "https://auth.robokassa.ru/Merchant/Index.aspx",
    opStateUrl    = "https://auth.robokassa.ru/Merchant/WebService/Service.asmx/OpStateExt",
    sendReceipt   = false,
    tax           = "none",
    paymentObject = "service",
    offerUrl      = "https://pangea.test/offer",
    linkMinutes   = 60,
    dailyLimit    = 0L,
    packs         = Nil
  )

  // 2025-10-09 08:53:20 UTC — 11:53 по Москве.
  private val request = PaymentRequest(
    invId       = 42L,
    amount      = 9900L,
    description = "100 дублонов",
    email       = "player@mail.ru",
    receiptName = "100 дублонов",
    dueAtMs     = 1760000000000L
  )

  /** Query ссылки как карта — значения раскодированы один раз, как их увидит
    * касса. */
  private def query(url: String): Map[String, String] =
    url.dropWhile(_ != '?').drop(1).split('&').toList.map { pair =>
      val (k, v) = pair.span(_ != '=')
      k -> URLDecoder.decode(v.drop(1), StandardCharsets.UTF_8)
    }.toMap

  // Независимо, в Python: urllib.parse.quote_plus от JSON чека ниже, затем
  // sha256('demo:99.00:42:' + закодированный + ':pass1').
  private val ReceiptJson =
    """{"items":[{"name":"100 дублонов","quantity":1,"sum":99.00,"payment_method":"full_payment",""" +
      """"payment_object":"service","tax":"none"}]}"""
  private val ReceiptEncoded =
    "%7B%22items%22%3A%5B%7B%22name%22%3A%22100+%D0%B4%D1%83%D0%B1%D0%BB%D0%BE%D0%BD%D0%BE%D0%B2%22%2C" +
      "%22quantity%22%3A1%2C%22sum%22%3A99.00%2C%22payment_method%22%3A%22full_payment%22%2C" +
      "%22payment_object%22%3A%22service%22%2C%22tax%22%3A%22none%22%7D%5D%7D"
  private val ReceiptSignature = "3a9844d54bf591331ac31d7171262d131ad516eb6f9d43be96f9605495cd9233"

  // Настоящий ответ кассы на запрос с несуществующим магазином — с BOM.
  private val UnknownShop =
    "﻿<?xml version=\"1.0\" encoding=\"utf-8\"?><OperationStateResponse " +
      "xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" " +
      "xmlns=\"http://merchant.roboxchange.com/WebService/\"><Result><Code>2</Code><Description>Информация о " +
      "магазине с таким MerchantLogin не найдена или магазин не активирован.</Description></Result>" +
      "</OperationStateResponse>"

  private def stateXml(result: Int, state: Option[Int]): String =
    "<?xml version=\"1.0\" encoding=\"utf-8\"?><OperationStateResponse " +
      "xmlns=\"http://merchant.roboxchange.com/WebService/\">" +
      s"<Result><Code>$result</Code></Result>" +
      state.fold("")(code =>
        s"<State><Code>$code</Code><RequestDate>2025-10-09T11:53:20+03:00</RequestDate></State>" +
          "<Info><IncCurrLabel>BankCard</IncCurrLabel><OutSum>96.53</OutSum></Info>") +
      "</OperationStateResponse>"

  private val ConfigPath = "app/src/main/resources/application.conf"

  override def spec = suite("Робокасса: провод")(

    suite("суммы")(
      test("копейки в OutSum — всегда два знака") {
        assertTrue(RobokassaApi.outSum(9900L) == "99.00") &&
        assertTrue(RobokassaApi.outSum(34950L) == "349.50") &&
        assertTrue(RobokassaApi.outSum(105L) == "1.05")
      },

      test("OutSum уведомления в копейки: шесть знаков после точки — норма") {
        assertTrue(RobokassaApi.kopecks("99.000000").contains(9900L)) &&
        assertTrue(RobokassaApi.kopecks("349.5").contains(34950L)) &&
        assertTrue(RobokassaApi.kopecks("99.001").isEmpty) &&
        assertTrue(RobokassaApi.kopecks("мусор").isEmpty)
      }
    ),

    test("срок ссылки — московское время без зоны") {
      assertTrue(RobokassaApi.dueDate(1760000000000L) == "2025-10-09T11:53")
    },

    suite("ссылка на оплату")(

      test("без чека: все поля на месте, подпись по формуле без Receipt") {
        val params = query(RobokassaApi.paymentUrl(config, request))
        assertTrue(params("MerchantLogin") == "demo") &&
        assertTrue(params("OutSum") == "99.00") &&
        assertTrue(params("InvId") == "42") &&
        assertTrue(params("Description") == "100 дублонов") &&
        assertTrue(params("Email") == "player@mail.ru") &&
        assertTrue(params("ExpirationDate") == "2025-10-09T11:53") &&
        assertTrue(params("SignatureValue") ==
          RobokassaSignature.payment(HashAlgo.Sha256, "demo", "99.00", 42L, None, "pass1")) &&
        assertTrue(!params.contains("Receipt")) &&
        assertTrue(!params.contains("IsTest"))
      },

      test("ссылка ведёт на форму оплаты") {
        assertTrue(RobokassaApi.paymentUrl(config, request).startsWith(config.payUrl + "?"))
      },

      test("с чеком: JSON позиции, в подписи и в форме — однократно закодированный") {
        val withReceipt = config.copy(sendReceipt = true)
        val params      = query(RobokassaApi.paymentUrl(withReceipt, request))
        assertTrue(RobokassaApi.receipt(withReceipt, request) == ReceiptJson) &&
        assertTrue(params("Receipt") == ReceiptEncoded) &&
        assertTrue(params("SignatureValue") == ReceiptSignature)
      },

      test("тестовый режим добавляет IsTest=1") {
        assertTrue(query(RobokassaApi.paymentUrl(config.copy(test = true), request)).get("IsTest").contains("1"))
      }
    ),

    test("запрос OpStateExt подписан паролем #2") {
      val params = query(RobokassaApi.opStateUrl(config, 42L))
      assertTrue(params("MerchantLogin") == "demo") &&
      assertTrue(params("InvoiceID") == "42") &&
      assertTrue(params("Signature") == RobokassaSignature.opState(HashAlgo.Sha256, "demo", 42L, "pass2"))
    },

    suite("ответ OpStateExt")(

      test("операция найдена — её состояние") {
        assertTrue(RobokassaApi.opState(stateXml(0, Some(100))) == Right(OpState.Found(100))) &&
        assertTrue(RobokassaApi.opState(stateXml(0, Some(5))) == Right(OpState.Found(5)))
      },

      test("код 3 — операции ещё нет") {
        assertTrue(RobokassaApi.opState(stateXml(3, None)) == Right(OpState.NotFound))
      },

      test("ошибка кассы — с описанием, BOM в начале ответа не мешает") {
        assertTrue(RobokassaApi.opState(UnknownShop) match {
          case Right(OpState.Failed(2, description)) => description.contains("MerchantLogin")
          case _                                     => false
        })
      },

      test("не XML или без кодов — ошибка разбора, а не выдумка") {
        assertTrue(RobokassaApi.opState("<html>502</html>").isLeft) &&
        assertTrue(RobokassaApi.opState("не xml").isLeft)
      },

      test("DTD в ответе не разбирается") {
        val xxe = "<?xml version=\"1.0\"?><!DOCTYPE r [<!ENTITY x SYSTEM \"file:///etc/passwd\">]>" +
          "<OperationStateResponse><Result><Code>0</Code></Result><State><Code>&x;</Code></State></OperationStateResponse>"
        assertTrue(RobokassaApi.opState(xxe).isLeft)
      }
    ),

    suite("конфиг")(

      /* Конфиг разбирается только при запуске, и опечатка в имени поля роняет
       * бота целиком — но уже на проде. Поэтому настоящий `application.conf`
       * читаем тестом. */
      test("настоящий application.conf читается, и без ключей донат выключен") {
        val file = new File(ConfigPath)
        for {
          _      <- ZIO.when(!file.exists())(ZIO.fail(new Throwable(s"не найден $ConfigPath")))
          loaded <- ZIO.attempt(ConfigSource.file(file).at("robokassa").loadOrThrow[RobokassaConfig])
        } yield assertTrue(loaded.hash == HashAlgo.Sha256) &&
                assertTrue(!loaded.test) &&
                assertTrue(!loaded.sendReceipt) &&
                assertTrue(loaded.linkMinutes > 0) &&
                assertTrue(loaded.packs.nonEmpty) &&
                assertTrue(DonationSku.validate(loaded.packs).isEmpty) &&
                // Без логина и паролей донат обязан быть выключен: иначе пустой
                // прод начнёт выписывать ссылки, которые касса не примет.
                assertTrue(!loaded.enabled)
      },

      test("донат включается только с логином, обоими паролями и прайсом") {
        val keyed = config.copy(packs = List(DonationSku("d1", 10L, 9900L, "сто", "сто")))
        assertTrue(keyed.enabled) &&
        assertTrue(!keyed.copy(packs = Nil).enabled) &&
        assertTrue(!keyed.copy(password2 = "").enabled) &&
        assertTrue(!keyed.copy(merchantLogin = "").enabled)
      },

      test("неизвестный алгоритм подписи роняет загрузку") {
        val raw = ConfigSource.string("hash = ripemd160").at("hash")
        assertTrue(raw.load[HashAlgo].isLeft)
      }
    ),

    suite("прайс-лист")(

      test("пустой прайс — не ошибка, это просто выключенный донат") {
        assertTrue(DonationSku.validate(Nil).isEmpty)
      },

      test("цена ниже минимума не проходит") {
        val cheap = DonationSku("d1", 10L, 500L, "мало", "мало")
        assertTrue(DonationSku.validate(List(cheap)).exists(_.contains("меньше минимальных")))
      },

      test("повторяющийся id не проходит: по нему находят пакет в заказе") {
        val pack = DonationSku("d1", 10L, 9900L, "сто", "сто")
        assertTrue(DonationSku.validate(List(pack, pack)).exists(_.contains("объявлен 2 раза")))
      },

      test("спецсимволы в наименовании не проходят") {
        val quoted = DonationSku("d1", 10L, 9900L, "сто", "Пакет \"Богач\"")
        assertTrue(DonationSku.validate(List(quoted)).exists(_.contains("' \" & < >")))
      },

      test("наименование длиннее 100 символов (предел Description) не проходит") {
        val long = DonationSku("d1", 10L, 9900L, "сто", "я" * 101)
        assertTrue(DonationSku.validate(List(long)).exists(_.contains("длиннее 100")))
      }
    ),

    test("рубли из копеек: копейки показываем только когда они есть") {
      assertTrue(Payment.rubles(9900L) == "99") &&
      assertTrue(Payment.rubles(34950L) == "349,50") &&
      assertTrue(Payment.rubles(105L) == "1,05")
    }
  )
}
