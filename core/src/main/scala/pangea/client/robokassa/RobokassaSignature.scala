package pangea.client.robokassa

import enumeratum.{Enum, EnumEntry}
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/** Алгоритм контрольной суммы. Выбирается в технических настройках магазина в
  * личном кабинете Робокассы и должен совпадать с конфигом: иначе касса
  * отвергнет ссылку (ошибка 29), а мы — каждое уведомление. RIPEMD160 касса
  * тоже умеет, но в JDK его нет, поэтому здесь его нет и в кабинете его не
  * выбираем. */
sealed abstract class HashAlgo(override val entryName: String, val jca: String) extends EnumEntry

object HashAlgo extends Enum[HashAlgo] {

  val values = findValues

  case object Md5    extends HashAlgo("md5", "MD5")
  case object Sha1   extends HashAlgo("sha1", "SHA-1")
  case object Sha256 extends HashAlgo("sha256", "SHA-256")
  case object Sha384 extends HashAlgo("sha384", "SHA-384")
  case object Sha512 extends HashAlgo("sha512", "SHA-512")

  /** Опечатка в алгоритме роняет запуск, а не выключает донат молча: с
    * неверным алгоритмом не сойдётся ни одна подпись, и узнавать об этом лучше
    * сразу при деплое. */
  implicit val reader: ConfigReader[HashAlgo] =
    ConfigReader[String].emap(raw =>
      withNameInsensitiveOption(raw.trim)
        .toRight(CannotConvert(raw, "HashAlgo", s"ожидается одно из: ${values.map(_.entryName).mkString(", ")}")))
}

/** Подписи Робокассы.
  *
  * Формулы разные в каждом направлении, но устроены одинаково: значения через
  * двоеточие, пароль внутри строки, хеш в hex. Пароль #1 подписывает то, что
  * уходит от нас (ссылку на оплату), пароль #2 — то, что приходит от кассы
  * (ResultURL) и запросы состояния операции.
  *
  * Значения склеиваются ровно в том виде, в каком уходят или пришли по
  * проводу: `OutSum` уведомления приходит как `499.000000`, и подпись
  * посчитана именно по этой строке, а не по числу. */
object RobokassaSignature {

  /** Ссылка на оплату: `MerchantLogin:OutSum:InvId[:Receipt]:Пароль#1`.
    * `Receipt` — уже URL-кодированный, как того требует дока по фискализации. */
  def payment(
    algo:      HashAlgo,
    login:     String,
    outSum:    String,
    invId:     Long,
    receipt:   Option[String],
    password1: String
  ): String =
    hash(algo, (List(login, outSum, invId.toString) ++ receipt.toList :+ password1).mkString(":"))

  /** Уведомление ResultURL: `OutSum:InvId:Пароль#2[:Shp_*]`. Пользовательские
    * `Shp_*` идут после пароля, по алфавиту, парами `ключ=значение`. Мы их не
    * шлём, но если касса их вернёт, подпись без них не сойдётся. */
  def result(algo: HashAlgo, outSum: String, invId: String, shp: Map[String, String], password2: String): String = {
    val custom = shp.toList.sortBy(_._1).map { case (k, v) => s"$k=$v" }
    hash(algo, (List(outSum, invId, password2) ++ custom).mkString(":"))
  }

  /** Запрос состояния операции `OpStateExt`: `MerchantLogin:InvoiceID:Пароль#2`. */
  def opState(algo: HashAlgo, login: String, invId: Long, password2: String): String =
    hash(algo, s"$login:$invId:$password2")

  /** Проверить подпись уведомления ResultURL. Нет любого из обязательных полей —
    * сразу «не прошло». Регистр hex касса не гарантирует. Сравнение — за
    * постоянное время, как у пароля админки: иначе подпись подбирается
    * посимвольно по времени ответа. */
  def verifyResult(algo: HashAlgo, params: Map[String, String], password2: String): Boolean =
    (params.get("OutSum"), params.get("InvId"), params.get("SignatureValue")) match {
      case (Some(outSum), Some(invId), Some(offered)) =>
        val shp      = params.filter { case (k, _) => k.startsWith("Shp_") }
        val expected = result(algo, outSum, invId, shp, password2)
        MessageDigest.isEqual(
          expected.getBytes(StandardCharsets.UTF_8),
          offered.toLowerCase.getBytes(StandardCharsets.UTF_8)
        )
      case _ => false
    }

  private def hash(algo: HashAlgo, source: String): String =
    MessageDigest
      .getInstance(algo.jca)
      .digest(source.getBytes(StandardCharsets.UTF_8))
      .map(byte => f"$byte%02x")
      .mkString
}
