package pangea.client.tbank

import pangea.model.payment.DonationSku
import pureconfig.ConfigSource
import pureconfig.generic.auto._
import zio.{ZIO, ZLayer}

/** Касса Т-Банка и прайс-лист доната.
  *
  * Всё, кроме прайса, — плоские строки, потому что меняются они без релиза:
  * `terminal-key`/`password` приходят из окружения, налоговые поля чека
  * зависят от бухгалтерии, а адреса возврата — от домена.
  *
  * @param apiUrl          база API без хвостового слэша (`.../v2`)
  * @param notificationUrl куда банк постит статусы; должен смотреть на
  *                        `POST /pay/tbank/notify` этого приложения
  * @param taxation        система налогообложения для чека: `osn`,
  *                        `usn_income`, `usn_income_outcome`, `patent`,
  *                        `envd`, `esn`
  * @param tax             ставка НДС позиции: `none` для УСН, иначе `vat0`,
  *                        `vat5`, `vat7`, `vat10`, `vat22` и расчётные
  * @param paymentObject   признак предмета расчёта (тег ФФД 1212)
  * @param linkMinutes     срок жизни ссылки на оплату
  * @param dailyLimit      сколько копеек игрок может оплатить за сутки;
  *                        `0` — без ограничения
  */
case class TBankConfig(
  terminalKey:     String,
  password:        String,
  apiUrl:          String,
  notificationUrl: String,
  successUrl:      String,
  failUrl:         String,
  taxation:        String,
  tax:             String,
  paymentObject:   String,
  offerUrl:        String,
  linkMinutes:     Int,
  dailyLimit:      Long,
  packs:           List[DonationSku]
) {

  /** Можно ли вообще продавать. Без ключей терминала или без прайса донат
    * просто выключен, и экран у Рахадима остаётся заглушкой. */
  def enabled: Boolean = terminalKey.nonEmpty && password.nonEmpty && packs.nonEmpty

  def pack(id: String): Option[DonationSku] = packs.find(_.id == id)

  def initUrl: String     = s"$apiUrl/Init"
  def getStateUrl: String = s"$apiUrl/GetState"
}

object TBankConfig {

  private val load = ZIO.attempt(ConfigSource.default.at("tbank").loadOrThrow[TBankConfig])

  /** Битый прайс-лист выключает донат целиком, а не роняет бота: опечатка в
    * цене не повод уводить игру в офлайн, но и продавать по ней нельзя. */
  val live: ZLayer[Any, Throwable, TBankConfig] =
    ZLayer.fromZIO(
      load.flatMap { config =>
        DonationSku.validate(config.packs) match {
          case Nil => ZIO.succeed(config)
          case problems =>
            ZIO.logError(s"Донат выключен, прайс-лист не прошёл проверку: ${problems.mkString("; ")}")
              .as(config.copy(packs = Nil))
        }
      }
    )
}
