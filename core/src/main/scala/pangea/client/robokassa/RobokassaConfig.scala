package pangea.client.robokassa

import pangea.model.payment.DonationSku
import pureconfig.ConfigSource
import pureconfig.generic.auto._
import zio.{ZIO, ZLayer}

/** Касса Робокассы и прайс-лист доната.
  *
  * Адреса ResultURL, SuccessURL и FailURL здесь нет: у Робокассы они задаются
  * в технических настройках магазина в личном кабинете, а не в запросе.
  *
  * @param merchantLogin идентификатор магазина
  * @param password1     пароль #1: подписывает ссылку на оплату
  * @param password2     пароль #2: проверяет ResultURL и подписывает `OpStateExt`
  * @param test          тестовый режим (`IsTest=1`). Пароли в нём — ТЕСТОВЫЕ из
  *                      кабинета, а `OpStateExt` тестовых операций не видит
  * @param hash          алгоритм подписи, как в настройках магазина
  * @param payUrl        форма оплаты
  * @param opStateUrl    XML-интерфейс состояния операции
  * @param sendReceipt   передавать ли `Receipt`. Самозанятому чек в «Мой налог»
  *                      выбивают Робочеки СМЗ; нужен ли им наш `Receipt`, дока
  *                      не говорит — поэтому выключатель, а не код
  * @param tax           ставка НДС позиции чека (`none` для НПД)
  * @param paymentObject признак предмета расчёта позиции чека
  * @param linkMinutes   срок жизни ссылки на оплату
  * @param dailyLimit    сколько копеек игрок может оплатить за сутки;
  *                      `0` — без ограничения
  */
case class RobokassaConfig(
  merchantLogin: String,
  password1:     String,
  password2:     String,
  test:          Boolean,
  hash:          HashAlgo,
  payUrl:        String,
  opStateUrl:    String,
  sendReceipt:   Boolean,
  tax:           String,
  paymentObject: String,
  offerUrl:      String,
  linkMinutes:   Int,
  dailyLimit:    Long,
  packs:         List[DonationSku]
) {

  /** Можно ли вообще продавать. Без логина, паролей или прайса донат просто
    * выключен, и экран у Рахадима остаётся заглушкой. */
  def enabled: Boolean =
    merchantLogin.nonEmpty && password1.nonEmpty && password2.nonEmpty && packs.nonEmpty

  def pack(id: String): Option[DonationSku] = packs.find(_.id == id)
}

object RobokassaConfig {

  private val load = ZIO.attempt(ConfigSource.default.at("robokassa").loadOrThrow[RobokassaConfig])

  /** Битый прайс-лист выключает донат целиком, а не роняет бота: опечатка в
    * цене не повод уводить игру в офлайн, но и продавать по ней нельзя. */
  val live: ZLayer[Any, Throwable, RobokassaConfig] =
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
