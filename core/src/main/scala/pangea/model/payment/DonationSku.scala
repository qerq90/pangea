package pangea.model.payment

/** Пакет дублонов из прайс-листа. Живёт в конфиге (`tbank.packs`), а не в коде:
  * цены меняются чаще, чем выходят релизы.
  *
  * @param id        ключ пакета; попадает в `payments.sku` и в payload кнопки,
  *                  поэтому менять его у уже продававшегося пакета не стоит
  * @param doubloons сколько дублонов начислить
  * @param price     цена в КОПЕЙКАХ — ровно в таком виде её ждёт `Amount` у
  *                  банка, поэтому и держим так, чтобы нигде не умножать
  * @param label     подпись кнопки
  * @param receipt   наименование позиции в кассовом чеке (тег ФФД 1030)
  */
case class DonationSku(
  id:        String,
  doubloons: Long,
  price:     Long,
  label:     String,
  receipt:   String
) {

  def rubles: String = Payment.rubles(price)
}

object DonationSku {

  /** Ограничения, за которые нельзя выходить: дальше начнёт ругаться либо банк,
    * либо касса, либо клавиатура ВК. */
  val MaxReceiptNameLength: Int = 128

  /** Минимум по СБП — 10 ₽; ниже этого пакет просто не оплатить частью
    * способов, так что считаем такой прайс ошибкой конфигурации. */
  val MinPrice: Long = 1000L

  /** Символы, которые Т-Банк просит не передавать в строковых значениях: они
    * ломают разбор на стороне банка и кассы. */
  private val Forbidden: Set[Char] = Set('\'', '"', '&', '<', '>')

  /** Проверка прайс-листа на старте. Пустой список — это не ошибка, а просто
    * выключенный донат; а вот битый пакет лучше поймать при запуске, чем на
    * первом живом платеже. */
  def validate(packs: List[DonationSku]): List[String] = {
    val dupes = packs.groupBy(_.id).collect { case (id, xs) if xs.sizeIs > 1 => s"пакет '$id' объявлен ${xs.size} раза" }
    val each = packs.flatMap { p =>
      List(
        Option.when(p.price < MinPrice)(s"пакет '${p.id}': цена ${p.rubles} ₽ меньше минимальных ${rublesOf(MinPrice)} ₽"),
        Option.when(p.doubloons <= 0L)(s"пакет '${p.id}': дублонов ${p.doubloons}"),
        Option.when(p.receipt.isEmpty)(s"пакет '${p.id}': пустое наименование для чека"),
        Option.when(p.receipt.length > MaxReceiptNameLength)(
          s"пакет '${p.id}': наименование для чека длиннее $MaxReceiptNameLength символов"),
        Option.when(p.receipt.exists(Forbidden))(
          s"пакет '${p.id}': в наименовании для чека есть ' \" & < >")
      ).flatten
    }
    dupes.toList ++ each
  }

  private def rublesOf(kopecks: Long): String = Payment.rubles(kopecks)
}
