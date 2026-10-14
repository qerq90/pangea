package pangea.client.tbank

import io.circe.{Json, JsonObject}

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}

/** Что нужно знать, чтобы создать платёж. Всё остальное — ключи терминала,
  * адреса возврата, налоговые поля чека — клиент берёт из конфига сам, чтобы
  * игровой код про формат провода ничего не знал. */
case class InitRequest(
  amount:       Long,
  orderId:      String,
  description:  String,
  customerKey:  String,
  receiptEmail: String,
  receiptName:  String
)

/** Ответ на `Init`. Поля разбираем курсорами, а не выводом: `PaymentId` банк
  * отдаёт то строкой, то числом, и падать на этом из-за декодера не хочется. */
case class InitResponse(
  success:    Boolean,
  status:     Option[String],
  paymentId:  Option[String],
  paymentUrl: Option[String],
  errorCode:  Option[String],
  message:    Option[String],
  details:    Option[String]
) {

  /** Что писать в лог при неуспехе: игроку это показывать нельзя. */
  def problem: String =
    List(errorCode.map("code=" + _), message, details).flatten.mkString(" ")
}

case class GetStateResponse(
  success:   Boolean,
  status:    Option[String],
  amount:    Option[Long],
  errorCode: Option[String],
  message:   Option[String]
)

object TBankApi {

  /** Формат `RedirectDueDate`: `YYYY-MM-DDTHH24:MI:SS+GMT`. Зона — московская:
    * касса и личный кабинет живут в ней, и расхождение в сроке жизни ссылки
    * потом неудобно сверять. */
  private val DueDateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssXXX")
  private val Moscow        = ZoneId.of("Europe/Moscow")

  def dueDate(atMs: Long): String =
    Instant.ofEpochMilli(atMs).atZone(Moscow).toOffsetDateTime.format(DueDateFormat)

  /** Тело `Init` вместе с подписью.
    *
    * `Amount` уходит числом, как в доке, но в подпись то же значение попадает
    * строкой — об этом знает [[TBankToken]], и именно поэтому подпись считается
    * по готовому телу, а не по отдельному набору строк.
    *
    * `Receipt` — вложенный объект и в подписи не участвует; `Quantity` у нас
    * всегда 1, а `Amount` позиции равен `Price`, поэтому сумма позиций сходится
    * с корневым `Amount` сама, без арифметики на нашей стороне. */
  def initBody(config: TBankConfig, request: InitRequest, dueAtMs: Long): JsonObject = {
    val root = JsonObject(
      "TerminalKey"     -> Json.fromString(config.terminalKey),
      "Amount"          -> Json.fromLong(request.amount),
      "OrderId"         -> Json.fromString(request.orderId),
      "Description"     -> Json.fromString(request.description),
      "CustomerKey"     -> Json.fromString(request.customerKey),
      "NotificationURL" -> Json.fromString(config.notificationUrl),
      "SuccessURL"      -> Json.fromString(config.successUrl),
      "FailURL"         -> Json.fromString(config.failUrl),
      "PayType"         -> Json.fromString("O"),
      "Language"        -> Json.fromString("ru"),
      "RedirectDueDate" -> Json.fromString(dueDate(dueAtMs))
    )
    val receipt = Json.obj(
      "Email"    -> Json.fromString(request.receiptEmail),
      "Taxation" -> Json.fromString(config.taxation),
      "Items" -> Json.arr(
        Json.obj(
          "Name"          -> Json.fromString(request.receiptName),
          "Price"         -> Json.fromLong(request.amount),
          "Quantity"      -> Json.fromInt(1),
          "Amount"        -> Json.fromLong(request.amount),
          "Tax"           -> Json.fromString(config.tax),
          "PaymentMethod" -> Json.fromString("full_payment"),
          "PaymentObject" -> Json.fromString(config.paymentObject)
        )
      )
    )
    root
      .add("Token", Json.fromString(TBankToken.sign(root, config.password)))
      .add("Receipt", receipt)
  }

  def getStateBody(config: TBankConfig, paymentId: String): JsonObject = {
    val root = JsonObject(
      "TerminalKey" -> Json.fromString(config.terminalKey),
      "PaymentId"   -> Json.fromString(paymentId)
    )
    root.add("Token", Json.fromString(TBankToken.sign(root, config.password)))
  }

  def initResponse(json: Json): InitResponse = {
    val c = json.hcursor
    InitResponse(
      success    = c.get[Boolean]("Success").getOrElse(false),
      status     = str(json, "Status"),
      paymentId  = str(json, "PaymentId"),
      paymentUrl = str(json, "PaymentURL"),
      errorCode  = str(json, "ErrorCode").filter(_ != "0"),
      message    = str(json, "Message"),
      details    = str(json, "Details")
    )
  }

  def getStateResponse(json: Json): GetStateResponse =
    GetStateResponse(
      success   = json.hcursor.get[Boolean]("Success").getOrElse(false),
      status    = str(json, "Status"),
      amount    = json.hcursor.get[Long]("Amount").toOption,
      errorCode = str(json, "ErrorCode").filter(_ != "0"),
      message   = str(json, "Message")
    )

  /** Поле как строка, независимо от того, числом или строкой его отдал банк. */
  private def str(json: Json, field: String): Option[String] =
    json.hcursor.downField(field).focus.flatMap { value =>
      value.asString
        .orElse(value.asNumber.map(n => n.toLong.map(_.toString).getOrElse(n.toString)))
        .filter(_.nonEmpty)
    }
}
