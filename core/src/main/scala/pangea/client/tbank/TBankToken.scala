package pangea.client.tbank

import io.circe.{Json, JsonObject}

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/** Подпись запросов и нотификаций Т-Банка.
  *
  * Алгоритм один и тот же в обе стороны: собрать скалярные поля корня парами
  * ключ-значение, добавить пару `Password`, отсортировать по ключу,
  * склеить ТОЛЬКО значения и взять SHA-256 в UTF-8.
  *
  * Два места, где легко ошибиться, и оба закрыты здесь:
  *  - вложенные объекты и массивы (`Receipt`, `DATA`, `Items`) в подпись НЕ
  *    входят, поэтому [[params]] их отбрасывает;
  *  - числа и булевы в подпись идут как в JSON (`19200`, `true`), а не как
  *    литералы Scala, поэтому [[scalar]] приводит их руками, а не через
  *    `Json.toString`, который строкам добавил бы кавычки.
  *
  * Ровно та же функция подписывает исходящий `Init` и проверяет входящую
  * нотификацию — один код, одна возможность ошибиться. */
object TBankToken {

  /** Пары для подписи из тела запроса или нотификации: только скалярные поля
    * корня, без самого `Token`. `null` пропускаем — дока прямо говорит, что
    * такие поля в формировании не участвуют. */
  def params(body: JsonObject): Map[String, String] =
    body.toIterable.collect {
      case (key, value) if key != "Token" && !value.isObject && !value.isArray && !value.isNull =>
        key -> scalar(value)
    }.toMap

  def sign(params: Map[String, String], password: String): String =
    sha256Hex((params + ("Password" -> password)).toList.sortBy(_._1).map(_._2).mkString)

  def sign(body: JsonObject, password: String): String = sign(params(body), password)

  /** Проверить подпись входящей нотификации. Регистр hex не важен, отсутствие
    * `Token` — это сразу «не прошло». */
  def verify(body: JsonObject, password: String): Boolean =
    body("Token").flatMap(_.asString).exists(_.equalsIgnoreCase(sign(body, password)))

  /** Скаляр так, как он выглядит в JSON: строка без кавычек, булево словом,
    * целое число без дробной части. */
  private def scalar(value: Json): String =
    value.asString
      .orElse(value.asBoolean.map(_.toString))
      .orElse(value.asNumber.map(n => n.toLong.map(_.toString).getOrElse(n.toString)))
      .getOrElse(value.noSpaces)

  private def sha256Hex(source: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(source.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"$byte%02x")
      .mkString
}
