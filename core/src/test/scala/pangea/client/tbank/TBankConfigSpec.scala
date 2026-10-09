package pangea.client.tbank

import pangea.model.payment.{DonationSku, Payment}
import pureconfig.ConfigSource
import pureconfig.generic.auto._
import zio.ZIO
import zio.test._

import java.io.File

/** Конфиг кассы разбирается только при запуске, и опечатка в имени поля роняет
  * бота целиком — но уже на проде. Поэтому настоящий `application.conf` читаем
  * тестом: он единственный, кто поймает расхождение между HOCON и [[TBankConfig]]
  * до деплоя. */
object TBankConfigSpec extends ZIOSpecDefault {

  private val ConfigPath = "app/src/main/resources/application.conf"

  override def spec = suite("Конфиг кассы")(

    test("настоящий application.conf читается в TBankConfig") {
      val file = new File(ConfigPath)
      for {
        // Путь относительный: тесты идут из корня проекта. Если файл переехал,
        // тест должен упасть, а не промолчать.
        _      <- ZIO.when(!file.exists())(ZIO.fail(new Throwable(s"не найден $ConfigPath")))
        config <- ZIO.attempt(ConfigSource.file(file).at("tbank").loadOrThrow[TBankConfig])
      } yield assertTrue(config.apiUrl.endsWith("/v2")) &&
              assertTrue(config.initUrl.endsWith("/v2/Init")) &&
              assertTrue(config.getStateUrl.endsWith("/v2/GetState")) &&
              assertTrue(config.linkMinutes > 0) &&
              assertTrue(config.packs.nonEmpty) &&
              // Без ключей терминала донат обязан быть выключен: иначе пустой
              // прод начнёт создавать заказы, которые некому оплатить.
              assertTrue(!config.enabled)
    },

    test("прайс-лист из application.conf проходит собственную проверку") {
      for {
        config <- ZIO.attempt(ConfigSource.file(new File(ConfigPath)).at("tbank").loadOrThrow[TBankConfig])
      } yield assertTrue(DonationSku.validate(config.packs).isEmpty)
    },

    test("донат включается только с ключами терминала и непустым прайсом") {
      val base = ConfigSource
        .file(new File(ConfigPath))
        .at("tbank")
        .loadOrThrow[TBankConfig]
      val keyed = base.copy(terminalKey = "TERM", password = "secret")
      assertTrue(!base.enabled) &&
      assertTrue(keyed.enabled) &&
      assertTrue(!keyed.copy(packs = Nil).enabled) &&
      assertTrue(!keyed.copy(password = "").enabled)
    },

    suite("проверка прайс-листа")(

      test("пустой прайс — не ошибка, это просто выключенный донат") {
        assertTrue(DonationSku.validate(Nil).isEmpty)
      },

      test("цена ниже минимума по СБП не проходит") {
        val cheap = DonationSku("d1", 10L, 500L, "мало", "мало")
        assertTrue(DonationSku.validate(List(cheap)).exists(_.contains("меньше минимальных")))
      },

      test("повторяющийся id не проходит: по нему находят пакет в заказе") {
        val pack = DonationSku("d1", 10L, 9900L, "сто", "сто")
        assertTrue(DonationSku.validate(List(pack, pack)).exists(_.contains("объявлен 2 раза")))
      },

      test("запрещённые для чека символы не проходят") {
        val quoted = DonationSku("d1", 10L, 9900L, "сто", "Пакет \"Богач\"")
        assertTrue(DonationSku.validate(List(quoted)).exists(_.contains("' \" & < >")))
      },

      test("наименование длиннее 128 символов не проходит") {
        val long = DonationSku("d1", 10L, 9900L, "сто", "я" * 129)
        assertTrue(DonationSku.validate(List(long)).exists(_.contains("длиннее 128")))
      }
    ),

    test("рубли из копеек: копейки показываем только когда они есть") {
      assertTrue(Payment.rubles(9900L) == "99") &&
      assertTrue(Payment.rubles(49900L) == "499") &&
      assertTrue(Payment.rubles(34950L) == "349,50") &&
      assertTrue(Payment.rubles(100L) == "1") &&
      assertTrue(Payment.rubles(105L) == "1,05")
    }
  )
}
