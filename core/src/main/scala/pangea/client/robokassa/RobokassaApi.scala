package pangea.client.robokassa

import io.circe.Json

import java.io.StringReader
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}
import javax.xml.XMLConstants
import javax.xml.parsers.DocumentBuilderFactory
import org.xml.sax.InputSource

import scala.util.Try

/** Что нужно знать, чтобы выписать ссылку на оплату. Всё остальное — логин,
  * пароли, налоговые поля чека — берётся из конфига, чтобы игровой код про
  * формат провода ничего не знал. */
case class PaymentRequest(
  invId:       Long,
  amount:      Long,
  description: String,
  email:       String,
  receiptName: String,
  dueAtMs:     Long
)

/** Ответ `OpStateExt`. */
sealed trait OpState
object OpState {

  /** Операция есть, `code` — её состояние (5, 10, 20, 50, 60, 80, 100). */
  final case class Found(code: Int) extends OpState

  /** Операции нет (код результата 3). Нормально для свежего заказа: касса
    * заводит операцию, только когда покупатель подтвердил реквизиты, а не
    * когда открыл ссылку. */
  case object NotFound extends OpState

  /** Касса ответила ошибкой: подпись, магазин, внутренний сбой. */
  final case class Failed(code: Int, description: String) extends OpState
}

object RobokassaApi {

  /** Формат `ExpirationDate` — `YYYY-MM-DDThh:mm`, без зоны. Дока зону не
    * называет; касса московская, поэтому и время московское. Ошибка здесь
    * ничего не ломает: срок жизни заказа мы считаем сами, а оплата по
    * «пережившей» ссылке всё равно придёт уведомлением и будет выдана. */
  private val DueDateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm")
  private val Moscow        = ZoneId.of("Europe/Moscow")

  def dueDate(atMs: Long): String =
    Instant.ofEpochMilli(atMs).atZone(Moscow).format(DueDateFormat)

  /** Копейки в `OutSum`: `9900` → `99.00`. Ровно эта строка уходит в ссылку и
    * в подпись. */
  def outSum(kopecks: Long): String = BigDecimal(kopecks, 2).bigDecimal.toPlainString

  /** `OutSum` из уведомления в копейки: касса шлёт `99.000000`. Дробь меньше
    * копейки или мусор — `None`, и такую сумму мы не примем. */
  def kopecks(outSum: String): Option[Long] =
    Try(BigDecimal(outSum.trim) * 100).toOption.filter(_.isWhole).map(_.toLong)

  /** Состав чека: одна позиция на всю сумму. `sno` не передаём — для НПД его
    * в списке значений нет, а без него касса берёт настройку из кабинета. */
  def receipt(config: RobokassaConfig, request: PaymentRequest): String =
    Json.obj(
      "items" -> Json.arr(
        Json.obj(
          "name"           -> Json.fromString(request.receiptName),
          "quantity"       -> Json.fromInt(1),
          "sum"            -> Json.fromBigDecimal(BigDecimal(request.amount, 2)),
          "payment_method" -> Json.fromString("full_payment"),
          "payment_object" -> Json.fromString(config.paymentObject),
          "tax"            -> Json.fromString(config.tax)
        )
      )
    ).noSpaces

  /** Ссылка на оплату. Собирается целиком у нас: запроса к кассе на этом шаге
    * нет, она узнаёт о заказе, когда игрок откроет ссылку.
    *
    * `Receipt` по доке «закодирован в UTF-8 и затем в URL», и в подпись идёт
    * именно закодированная строка. В ссылке он поэтому кодируется второй раз:
    * так же, как его закодировал бы браузер, отправляя форму со скрытым полем
    * из примера кассы. */
  def paymentUrl(config: RobokassaConfig, request: PaymentRequest): String = {
    val sum     = outSum(request.amount)
    val receipt = Option.when(config.sendReceipt)(encode(RobokassaApi.receipt(config, request)))
    val signature =
      RobokassaSignature.payment(config.hash, config.merchantLogin, sum, request.invId, receipt, config.password1)
    val params =
      List(
        "MerchantLogin"  -> config.merchantLogin,
        "OutSum"         -> sum,
        "InvId"          -> request.invId.toString,
        "Description"    -> request.description,
        "Email"          -> request.email,
        "ExpirationDate" -> dueDate(request.dueAtMs),
        "Culture"        -> "ru",
        "Encoding"       -> "utf-8"
      ) ++
        receipt.map("Receipt" -> _).toList ++
        Option.when(config.test)("IsTest" -> "1").toList :+
        ("SignatureValue" -> signature)
    config.payUrl + "?" + query(params)
  }

  def opStateUrl(config: RobokassaConfig, invId: Long): String =
    config.opStateUrl + "?" + query(List(
      "MerchantLogin" -> config.merchantLogin,
      "InvoiceID"     -> invId.toString,
      "Signature"     -> RobokassaSignature.opState(config.hash, config.merchantLogin, invId, config.password2)
    ))

  /** Разбор ответа `OpStateExt`. Касса отдаёт XML с пространством имён и BOM в
    * начале; читаем по локальным именам и без DTD — внешние сущности в
    * ответе нам не нужны, а XXE не нужен тем более. */
  def opState(xml: String): Either[String, OpState] =
    Try {
      val factory = DocumentBuilderFactory.newInstance()
      factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
      factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
      factory.setExpandEntityReferences(false)
      val doc = factory.newDocumentBuilder().parse(new InputSource(new StringReader(xml.stripPrefix("﻿"))))

      def child(parent: String, name: String): Option[String] = {
        val parents = doc.getElementsByTagName(parent)
        Option.when(parents.getLength > 0)(parents.item(0)).flatMap { node =>
          val kids = node.getChildNodes
          (0 until kids.getLength).map(kids.item).find(_.getNodeName == name).map(_.getTextContent.trim)
        }
      }

      (child("Result", "Code").flatMap(_.toIntOption), child("State", "Code").flatMap(_.toIntOption)) match {
        case (Some(0), Some(state)) => Right(OpState.Found(state))
        case (Some(3), _)           => Right(OpState.NotFound)
        case (Some(code), _) if code != 0 =>
          Right(OpState.Failed(code, child("Result", "Description").getOrElse("")))
        case _ => Left(s"нет кода результата или состояния: ${RobokassaError.snippet(xml)}")
      }
    }.toEither.left.map(err => s"ответ не XML: ${err.getMessage}").flatten

  private def query(params: List[(String, String)]): String =
    params.map { case (k, v) => s"$k=${encode(v)}" }.mkString("&")

  private def encode(value: String): String = URLEncoder.encode(value, StandardCharsets.UTF_8)
}
