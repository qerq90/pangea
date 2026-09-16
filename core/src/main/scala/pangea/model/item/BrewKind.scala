package pangea.model.item

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import pangea.model.stats.ParamsBuff

/** Что делает отвар, когда его пьют из инвентаря. */
sealed trait BrewEffect
object BrewEffect {
  /** Снимает одну травму на выбор — лёгкую или среднюю; тяжёлые не берёт. */
  case object CureTrauma extends BrewEffect
  /** Заправляет надетую флягу целиком. */
  case object RefillFlask extends BrewEffect
  /** Быстрые отдыхи: столько в запас (BrewRates.InstantRests). */
  case object InstantRest extends BrewEffect
  /** Часовой баф к базовой характеристике; `name` — идентичность в StatBoosts. */
  final case class Boost(name: String, buff: ParamsBuff, label: String) extends BrewEffect
  /** Пить нечего: предмет для будущих заданий (сонный дурман). */
  case object Inert extends BrewEffect
  /** Не пьётся — продаётся Трактирщику за `price` серебра (шнапс). */
  final case class Sellable(price: Long) extends BrewEffect
}

/** Отвары из трав первого ранга: три травы в кубе Азата → один отвар. Лежит в
 *  инвентаре как [[ItemType.Brew]], складывается в стопку, пьётся с карточки
 *  предмета. Все числа — здесь ([[BrewRates]] — те, что читаются в конструкторах). */
sealed abstract class BrewKind(
  val label:       String,
  val recipe:      List[MaterialKind],
  val description: String,
  val effect:      BrewEffect
) extends EnumEntry {
  /** Можно ли выпить с карточки. */
  def drinkable: Boolean = effect match {
    case BrewEffect.Inert | BrewEffect.Sellable(_) => false
    case _                                         => true
  }
}

/** Числа отваров — отдельно от компаньона, чтобы варианты не читали его поля в
 *  конструкторе (см. FlaskRates: иначе класс-инициализация может встать в дедлок). */
object BrewRates {
  /** Часовой баф: сколько процентов и на сколько. */
  val BoostPct: Int        = 10
  val BoostDurationMs: Long = 60L * 60L * 1000L
  /** Шнапс: сколько Трактирщик платит за бутылку. */
  val SchnappsPrice: Long = 400L
  /** Бодрящий сбор: сколько быстрых отдыхов даёт. */
  val InstantRests: Int = 5
}

object BrewKind extends Enum[BrewKind] {
  import MaterialKind._

  case object BoneSetter extends BrewKind(
    "Костоправный отвар",
    List(Chamomile, Calendula, Nettle),
    "Густой горький отвар, от которого ноет всё, что ещё не срослось, — и срастается. Лёгкую или среднюю травму снимает за один глоток; с тяжёлой ему не совладать.",
    BrewEffect.CureTrauma)

  case object LivingWater extends BrewKind(
    "Живая вода",
    List(Sage, Chamomile, Nettle),
    "Прозрачная, чуть горчащая на языке. Один флакон заправляет надетую флягу до краёв — хоть посреди лабиринта.",
    BrewEffect.RefillFlask)

  case object Invigorating extends BrewKind(
    "Бодрящий сбор",
    List(Valerian, Sage, Eyebright),
    s"Пахнет так, что сон снимает как рукой. Выпить — и ближайшие отдыхи пройдут мгновенно: +${BrewRates.InstantRests} быстрых отдыхов.",
    BrewEffect.InstantRest)

  case object ClearEye extends BrewKind(
    "Настой ясного глаза",
    List(Eyebright, Valerian, Chamomile),
    s"Дрожь в руках уходит, взгляд становится цепким. +${BrewRates.BoostPct}% к ловкости на час; с зельями Густаво складывается.",
    BrewEffect.Boost("herb:agi", ParamsBuff(0, 0, BrewRates.BoostPct, 0), "Ловкость"))

  case object WormwoodTincture extends BrewKind(
    "Полынная настойка",
    List(Wormwood, Nettle, Calendula),
    s"Горькая до слёз, зато плечи расправляет. +${BrewRates.BoostPct}% к силе на час; с зельями Густаво складывается.",
    BrewEffect.Boost("herb:str", ParamsBuff(BrewRates.BoostPct, 0, 0, 0), "Сила"))

  case object SeerDope extends BrewKind(
    "Дурман провидца",
    List(Wormwood, Belladonna, Eyebright),
    s"Мутная жижа с запахом полыни. Мысли становятся ясными и быстрыми — если не заглядываться на видения. +${BrewRates.BoostPct}% к интеллекту на час; с зельями Густаво складывается.",
    BrewEffect.Boost("herb:int", ParamsBuff(0, 0, 0, BrewRates.BoostPct), "Интеллект"))

  case object SleepingDope extends BrewKind(
    "Сонный дурман",
    List(Belladonna, Chamomile, Valerian),
    "Тёмная тягучая настойка. Пары одной капли валят с ног быка. Пить самому — глупо; но кто-нибудь наверняка захочет её купить или подлить.",
    BrewEffect.Inert)

  case object Schnapps extends BrewKind(
    "Шнапс из красавки",
    List(Belladonna, Wormwood, Calendula),
    s"Крепкий, дурманящий, с ягодным послевкусием. Пить его не стоит, а вот Трактирщик берёт по ${BrewRates.SchnappsPrice} серебра за бутылку.",
    BrewEffect.Sellable(BrewRates.SchnappsPrice))

  val values: IndexedSeq[BrewKind] = findValues

  /** Отвар, который варится из этого набора трав (порядок не важен), если такой есть. */
  def forHerbs(herbs: List[MaterialKind]): Option[BrewKind] =
    values.find(k => k.recipe.sorted(Ordering.by[MaterialKind, String](_.entryName)) ==
                     herbs.sorted(Ordering.by[MaterialKind, String](_.entryName)))

  /** Предмет-шаблон (id = -1, присвоит персист). */
  def item(kind: BrewKind): Item =
    Item(
      id = -1L,
      name = kind.label,
      lvl = 1L,
      rarity = Rarity.Green,
      itemType = ItemType.Brew,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Brew(kind)
    )

  implicit val encoder: Encoder[BrewKind] = (k: BrewKind) => k.entryName.asJson
  implicit val decoder: Decoder[BrewKind] = (c: HCursor) => c.as[String].map(BrewKind.withName)
}
