package pangea.model.home

import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, Encoder, HCursor}

/** Дом героя — один из трёх, по ступеням. Продаёт их Юнона Кинтслев,
  * представитель городского совета. Дом выше покупается с зачётом того, что
  * есть: старый уходит обратно в управление совета, и его цена в дублонах
  * идёт в счёт нового. Продать или потерять дом иначе нельзя — только
  * рестартом. Что было в доме ниже, есть и в доме выше: хозяйство переезжает
  * вместе с героем.
  *
  * Числа — прямо в вариантах: вариант не трогает свой companion (иначе при
  * загрузке классов можно поймать взаимную блокировку). */
sealed abstract class House(
  val tier: Int,
  /** Ключ в `scenes.yaml`: `home.houses.<key>.*`. */
  val key: String,
  /** Цена в дублонах; её же совет засчитывает, когда дом сдают в зачёт следующего. */
  val doubloons: Long,
  /** Сколько спать на своей кровати, чтобы травмы прошли и силы вернулись. */
  val sleepMs: Long,
  /** Сколько после сна держится «выспался» — прибавка к опыту (см. [[HomeRates.RestedExpPct]]). */
  val restedMs: Long
)

object House {

  /** Домик в две комнаты: всё нужное и ничего сверх. Сон — 2 часа, «выспался» — 4. */
  case object Cottage extends House(1, "cottage", 100L, 2L * 3600000L, 4L * 3600000L)

  /** Добротный дом: свой алтарь Азата, верстак травника, библиотека. Сон — 1,5 часа, «выспался» — 8. */
  case object Townhouse extends House(2, "townhouse", 800L, 5400000L, 8L * 3600000L)

  /** Дом на парящем острове, куда ведёт ключ-портал. Сон — 1 час, «выспался» — 12. */
  case object Island extends House(3, "island", 2000L, 3600000L, 12L * 3600000L)

  val values: List[House] = List(Cottage, Townhouse, Island)

  def byTier(tier: Int): Option[House] = values.find(_.tier == tier)

  def byKey(key: String): Option[House] = values.find(_.key == key)

  /** Что можно купить, когда есть `current`: только дома выше него. */
  def forSale(current: Option[House]): List[House] = values.filter(h => current.forall(_.tier < h.tier))

  /** Цена дома в дублонах с зачётом того, что сдают в управление. */
  def doubloonPrice(target: House, current: Option[House]): Long =
    target.doubloons - current.fold(0L)(_.doubloons)

  /** За серебро продаётся только домик — и только тому, у кого дома ещё нет. */
  def silverPrice(target: House, current: Option[House]): Option[Long] =
    Option.when(target == Cottage && current.isEmpty)(HomeRates.CottageSilver)
}

/** Числа дома, которые не держит сам [[House]]. */
object HomeRates {

  /** Домик за серебро. */
  val CottageSilver: Long = 20000L

  /** «Выспался дома»: на столько процентов больше опыта из любого источника. */
  val RestedExpPct: Long = 10L

  /** Имя бафа «выспался» среди временных бафов героя (`Hero.statBoosts`). */
  val RestedBoost: String = "home:rested"
}

/** Всё о доме героя, что хранится в `heroes.home_data`: `tier` — ступень дома
  * (0 — дома нет), `metJunona` — была ли первая встреча: листок на доске у
  * ратуши и знакомство с Юноной. */
final case class HomeData(tier: Int = 0, metJunona: Boolean = false) {
  def house: Option[House] = House.byTier(tier)

  def owns: Boolean = house.isDefined
}

object HomeData {
  val empty: HomeData = HomeData()

  implicit val encoder: Encoder[HomeData] = deriveEncoder
  implicit val decoder: Decoder[HomeData] = (c: HCursor) =>
    for {
      tier <- c.getOrElse[Int]("tier")(0)
      met  <- c.getOrElse[Boolean]("metJunona")(false)
    } yield HomeData(tier, met)
}
