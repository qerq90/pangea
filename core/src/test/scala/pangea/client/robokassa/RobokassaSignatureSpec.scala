package pangea.client.robokassa

import zio.test._

/** Подпись — то место, где ошибка не видна до первого живого платежа: касса
  * просто ответит ошибкой 29, и причина будет не в коде, а в порядке склейки.
  * Готовых векторов в доке Робокассы нет, поэтому эталоны посчитаны отдельно,
  * штатными `md5`/`shasum` по строкам, собранным руками по формулам из доки. */
object RobokassaSignatureSpec extends ZIOSpecDefault {

  // printf '%s' 'demo:99.00:42:pass1' | md5
  private val PaymentMd5 = "8481596abde6d458ada57aa306ad1148"
  // printf '%s' 'demo:99.00:42:pass1' | shasum -a 256
  private val PaymentSha256 = "46dd00516ade33f4189ebe801da6ce65605c8e75c7bd4d9072a4e6b777779f03"
  // printf '%s' '99.000000:42:pass2' | shasum -a 256
  private val ResultSha256 = "fe6ca560a099e677a21a7338acebea0b4e85a644352b0ad9e950a17949bf00a9"
  // printf '%s' '99.000000:42:pass2:Shp_a=1:Shp_b=2' | shasum -a 256
  private val ResultShpSha256 = "d8bbc2270f0691c324b0c95b5783a9e056ed6b56559f6d7dcd8aa72a94270baa"
  // printf '%s' 'demo:42:pass2' | shasum -a 256
  private val OpStateSha256 = "3de13e898621a9e5b333d312de6fc432bd4b6e957c0857daef8853c476364f6d"

  private def notification(signature: String, extra: (String, String)*): Map[String, String] =
    Map("OutSum" -> "99.000000", "InvId" -> "42", "SignatureValue" -> signature) ++ extra

  override def spec = suite("Подпись Робокассы")(

    test("ссылка на оплату: MerchantLogin:OutSum:InvId:Пароль#1") {
      assertTrue(RobokassaSignature.payment(HashAlgo.Md5, "demo", "99.00", 42L, None, "pass1") == PaymentMd5) &&
      assertTrue(RobokassaSignature.payment(HashAlgo.Sha256, "demo", "99.00", 42L, None, "pass1") == PaymentSha256)
    },

    test("уведомление: OutSum подписан строкой, как пришёл, а не числом") {
      assertTrue(RobokassaSignature.result(HashAlgo.Sha256, "99.000000", "42", Map.empty, "pass2") == ResultSha256)
    },

    test("Shp_ идут после пароля и по алфавиту, в каком бы порядке ни пришли") {
      val shp = Map("Shp_b" -> "2", "Shp_a" -> "1")
      assertTrue(RobokassaSignature.result(HashAlgo.Sha256, "99.000000", "42", shp, "pass2") == ResultShpSha256)
    },

    test("OpStateExt: MerchantLogin:InvoiceID:Пароль#2") {
      assertTrue(RobokassaSignature.opState(HashAlgo.Sha256, "demo", 42L, "pass2") == OpStateSha256)
    },

    suite("проверка уведомления")(

      test("верная подпись проходит, регистр hex не важен") {
        assertTrue(RobokassaSignature.verifyResult(HashAlgo.Sha256, notification(ResultSha256), "pass2")) &&
        assertTrue(RobokassaSignature.verifyResult(HashAlgo.Sha256, notification(ResultSha256.toUpperCase), "pass2"))
      },

      test("Shp_ из уведомления участвуют в проверке") {
        val params = notification(ResultShpSha256, "Shp_a" -> "1", "Shp_b" -> "2")
        assertTrue(RobokassaSignature.verifyResult(HashAlgo.Sha256, params, "pass2"))
      },

      test("пароль #1 вместо #2, подменённая сумма, нет подписи — не проходит") {
        val tampered = notification(ResultSha256) + ("OutSum" -> "1.000000")
        assertTrue(!RobokassaSignature.verifyResult(HashAlgo.Sha256, notification(ResultSha256), "pass1")) &&
        assertTrue(!RobokassaSignature.verifyResult(HashAlgo.Sha256, tampered, "pass2")) &&
        assertTrue(!RobokassaSignature.verifyResult(HashAlgo.Sha256, notification(ResultSha256) - "SignatureValue", "pass2"))
      },

      test("алгоритм не тот, что в кабинете, — не проходит") {
        assertTrue(!RobokassaSignature.verifyResult(HashAlgo.Md5, notification(ResultSha256), "pass2"))
      }
    ),

    test("алгоритм из конфига читается без учёта регистра, мусор — нет") {
      assertTrue(HashAlgo.withNameInsensitiveOption("SHA256").contains(HashAlgo.Sha256)) &&
      assertTrue(HashAlgo.withNameInsensitiveOption("ripemd160").isEmpty)
    }
  )
}
