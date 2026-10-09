package pangea.client.tbank

import io.circe.{Json, JsonObject}
import pangea.model.payment.DonationSku
import zio.test._

/** Подпись — то место, где ошибка не видна до первого живого платежа: банк
  * просто ответит отказом, и причина будет не в коде, а в порядке склейки.
  * Поэтому проверяем её на готовых векторах из документации Т-Банка. */
object TBankTokenSpec extends ZIOSpecDefault {

  // Пример из раздела «Токен» документации. Внимание: в самой доке пример
  // внутренне противоречив — подпись посчитана по OrderId "00000", а в итоговом
  // теле запроса стоит 21090. Берём то, по чему считали.
  private val DocInitToken = "72dd466f8ace0a37a1f740ce5fb78101712bc0665d91a8108c7c8a0ccd426db2"

  private val docInitRoot = JsonObject(
    "TerminalKey" -> Json.fromString("MerchantTerminalKey"),
    "Amount"      -> Json.fromLong(19200L),
    "OrderId"     -> Json.fromString("00000"),
    "Description" -> Json.fromString("Подарочная карта на 1000 рублей")
  )

  // Пример из раздела «Токен HTTP(S)-уведомлений».
  private val DocNotifyToken = "1c0964277d0213349243065a0d5b838b8e90d2d25f740d0f2767836e710e80c8"

  private def docNotification(amount: Json) = JsonObject(
    "TerminalKey" -> Json.fromString("1234567890DEMO"),
    "OrderId"     -> Json.fromString("000000"),
    "Success"     -> Json.fromBoolean(true),
    "Status"      -> Json.fromString("AUTHORIZED"),
    "PaymentId"   -> Json.fromString("0000000"),
    "ErrorCode"   -> Json.fromString("0"),
    "Amount"      -> amount,
    "CardId"      -> Json.fromString("000000"),
    "Pan"         -> Json.fromString("200000******0000"),
    "ExpDate"     -> Json.fromString("1111"),
    "RebillId"    -> Json.fromString("000000")
  )

  private val config = TBankConfig(
    terminalKey     = "MerchantTerminalKey",
    password        = "11111111111111",
    apiUrl          = "https://securepay.tinkoff.ru/v2",
    notificationUrl = "https://pangea.test/pay/tbank/notify",
    successUrl      = "https://vk.com/im?sel=-1",
    failUrl         = "https://vk.com/im?sel=-1",
    taxation        = "usn_income",
    tax             = "none",
    paymentObject   = "service",
    offerUrl        = "https://pangea.test/offer",
    linkMinutes     = 60,
    dailyLimit      = 0L,
    packs           = Nil
  )

  override def spec = suite("Подпись Т-Банка")(

    test("вектор из доки для Init") {
      assertTrue(TBankToken.sign(docInitRoot, "11111111111111") == DocInitToken)
    },

    test("вектор из доки для нотификации") {
      val body = docNotification(Json.fromLong(1111L))
        .add("Token", Json.fromString(DocNotifyToken))
      assertTrue(TBankToken.sign(body, "11111111111") == DocNotifyToken) &&
      assertTrue(TBankToken.verify(body, "11111111111"))
    },

    test("Amount числом и строкой подписываются одинаково: банк присылает по-разному") {
      val asNumber = TBankToken.sign(docNotification(Json.fromLong(1111L)), "11111111111")
      val asString = TBankToken.sign(docNotification(Json.fromString("1111")), "11111111111")
      assertTrue(asNumber == DocNotifyToken) && assertTrue(asString == DocNotifyToken)
    },

    test("вложенные объекты и массивы в подпись не входят") {
      val withNested = docInitRoot
        .add("Receipt", Json.obj("Email" -> Json.fromString("a@b.ru")))
        .add("DATA", Json.obj("Phone" -> Json.fromString("+70000000000")))
        .add("Items", Json.arr(Json.fromString("что-то")))
      assertTrue(TBankToken.sign(withNested, "11111111111111") == DocInitToken)
    },

    test("сам Token в подписи не участвует") {
      val signed = docInitRoot.add("Token", Json.fromString("мусор"))
      assertTrue(TBankToken.sign(signed, "11111111111111") == DocInitToken)
    },

    test("null-поля пропускаются") {
      val withNull = docInitRoot.add("CustomerKey", Json.Null)
      assertTrue(TBankToken.sign(withNull, "11111111111111") == DocInitToken)
    },

    test("чужой пароль подпись не проходит") {
      val body = docNotification(Json.fromLong(1111L))
        .add("Token", Json.fromString(DocNotifyToken))
      assertTrue(!TBankToken.verify(body, "другой-пароль"))
    },

    test("нотификация без Token не проходит") {
      assertTrue(!TBankToken.verify(docNotification(Json.fromLong(1111L)), "11111111111"))
    },

    suite("тело Init")(

      test("Amount уходит числом, Receipt — вложенным объектом, подпись сходится") {
        val pack = DonationSku("d100", 100L, 19200L, "100 дублонов", "100 дублонов")
        val body = TBankApi.initBody(
          config,
          InitRequest(pack.price, "00000", pack.receipt, "7", "player@mail.ru", pack.receipt),
          dueAtMs = 1760000000000L
        )
        val json  = Json.fromJsonObject(body)
        val token = body("Token").flatMap(_.asString)
        // Пересчитываем подпись по тому же телу: так ловится и забытое поле,
        // и лишнее, попавшее в корень.
        val expected = TBankToken.sign(body.remove("Token").remove("Receipt"), config.password)
        assertTrue(json.hcursor.get[Long]("Amount").contains(19200L)) &&
        assertTrue(json.hcursor.downField("Receipt").downField("Email").as[String].contains("player@mail.ru")) &&
        assertTrue(token.contains(expected))
      },

      test("сумма позиций чека равна корневому Amount — иначе банк откажет") {
        val body  = TBankApi.initBody(
          config,
          InitRequest(49900L, "o-1", "550 дублонов", "7", "a@b.ru", "550 дублонов"),
          dueAtMs = 1760000000000L
        )
        val json  = Json.fromJsonObject(body)
        val root  = json.hcursor.get[Long]("Amount").toOption
        val items = json.hcursor.downField("Receipt").downField("Items").as[List[Json]].getOrElse(Nil)
        val sum   = items.flatMap(_.hcursor.get[Long]("Amount").toOption).sum
        assertTrue(root.contains(sum)) && assertTrue(items.size == 1)
      },

      test("RedirectDueDate в формате банка со смещением зоны") {
        val due = TBankApi.dueDate(1760000000000L)
        assertTrue(due.matches("""\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}[+-]\d{2}:\d{2}"""))
      }
    )
  )
}
