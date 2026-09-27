package pangea.model.item

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import pangea.model.hero.WeaponCoat
import pangea.model.stats.ParamsBuff

/** Что делает отвар, когда его пьют из инвентаря. */
sealed trait BrewEffect
object BrewEffect {
  /** Снимает одну травму на выбор — лёгкую или среднюю; тяжёлые не берёт. */
  case object CureTrauma extends BrewEffect
  /** Заправляет надетую флягу целиком — если она из этих источников. */
  final case class RefillFlask(sources: Set[RefillSource]) extends BrewEffect
  /** Смазка оружия на ближайший бой (яд или кровь); заодно заправляет флягу того же толка. */
  final case class Coat(coat: WeaponCoat, refills: RefillSource) extends BrewEffect
  /** Быстрые отдыхи: столько в запас (BrewRates.InstantRests). */
  case object InstantRest extends BrewEffect
  /** Часовой баф к базовой характеристике; `name` — идентичность в StatBoosts. */
  final case class Boost(name: String, buff: ParamsBuff, label: String) extends BrewEffect
  /** Пить нечего: предмет для будущих заданий (сонный дурман). */
  case object Inert extends BrewEffect
  /** Не пьётся — продаётся Трактирщику за `price` серебра (шнапс). */
  final case class Sellable(price: Long) extends BrewEffect
  /** Бросок в бою: взрыв по всему полю, каждому врагу поровну. */
  case object Bomb extends BrewEffect
  /** Запах, на который приходит Белый волк: ближайшая встреча в лабиринте — он. */
  case object WolfCall extends BrewEffect
  /** Призрачные копии: первые `copies` ударов по герою уходят в них. */
  final case class Mirror(copies: Int) extends BrewEffect
  /** Приговор: герой называет расу, и следующая встреча — её именное существо,
    * добитое кем-то до него. */
  case object Sentence extends BrewEffect
  /** Пузырь: ближайшая смерть героя не состоится — он останется с одним HP. */
  case object Bubble extends BrewEffect
}

/** Отвары: три травы в кубе Азата → порции ([[portions]]). Из трав первого
 *  ранга выходит по две, из редких второго — по одной: такие травы штучные, и
 *  отвары у них под стать. Лежит в инвентаре как [[ItemType.Brew]],
 *  складывается в стопку, пьётся с карточки предмета. Все числа — здесь
 *  ([[BrewRates]] — те, что читаются в конструкторах). */
sealed abstract class BrewKind(
  val label:       String,
  val recipe:      List[MaterialKind],
  val description: String,
  val effect:      BrewEffect
) extends EnumEntry {
  /** Пьётся ли с карточки (кнопка «Выпить»). Заправка и смазка — свои кнопки,
   *  а грибную смесь не пьют вовсе: её бросают под ноги врагам в бою. */
  def drinkable: Boolean = effect match {
    case BrewEffect.CureTrauma | BrewEffect.InstantRest | BrewEffect.Boost(_, _, _) => true
    case BrewEffect.WolfCall | BrewEffect.Mirror(_)                                 => true
    case BrewEffect.Sentence | BrewEffect.Bubble                                     => true
    case _                                                                           => false
  }

  /** Отвар, который идёт в рецепт вместе с травами: перегонка берёт готовую
    * склянку и доводит её до ума. Метод, а не поле с умолчанием: значение по
    * умолчанию в конструкторе живёт в компаньоне, и вариант ждал бы его
    * инициализации — дедлок на ровном месте (см. заметку у `poisons` розы). */
  def base: Option[BrewKind] = this match {
    case BrewKind.Moonshine => Some(BrewKind.Schnapps)
    case _                  => None
  }

  /** Сколько склянок выходит из одного рецепта: редкая трава в составе — одна. */
  def portions: Int =
    if (recipe.exists(_.herbRank >= 2)) BrewRates.RarePortions else BrewRates.Portions

  /** Какие фляги заправляет. */
  def refills: Set[RefillSource] = effect match {
    case BrewEffect.RefillFlask(sources) => sources
    case BrewEffect.Coat(_, source)      => Set(source)
    case _                               => Set.empty
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
  /** Сколько порций даёт один рецепт в кубе. */
  val Portions: Int = 2

  /** Столько выходит из рецепта с редкой травой второго ранга. */
  val RarePortions: Int = 1

  /** Зеркальный настой: сколько ударов по герою уходит в призрачные копии. */
  val MirrorCopies: Int = 3

  /** Волчий зов: сколько держится запах, на который приходит волк (час). */
  val WolfCallMs: Long = 60L * 60L * 1000L

  /** Грибная смесь: сколько урона достаётся каждому врагу за уровень героя.
    * Вдвое меньше, чем у раскрывшейся розы, — зато варится из трав. */
  val MushroomDamagePerLvl: Long = 25L

  /** Имя бафа «на запах пришёл волк» в [[pangea.model.stats.StatBoosts]]. */
  val WolfCallBoost: String = "brew:wolfCall"

  /** Приговор: префикс бафа (за ним — раса) и сколько он держится. */
  val SentenceBoost: String = "brew:sentence:"
  val SentenceMs: Long      = 60L * 60L * 1000L

  /** Сколько процентов HP и брони остаётся у приговорённого к встрече. */
  val SentenceHpPct: Long = 25L

  /** Сколько ходов он терпит, прежде чем удрать. */
  val SentenceRounds: Int = 3

  /** Пузырь: имя бафа. Срока у него нет — он ждёт смерти, сколько бы её ни ждать. */
  val BubbleBoost: String = "brew:bubble"

  /** Самогон: во сколько раз Трактирщик платит за него больше, чем за шнапс. */
  val MoonshineFactor: Long = 3L
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
    "Прозрачная, чуть горчащая на языке. Один флакон заправляет до краёв флягу целителя, кузнеца, бодрости, очищения или стихийную — хоть посреди лабиринта.",
    BrewEffect.RefillFlask(Set(RefillSource.Water, RefillSource.Alchemy)))

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
    "Тёмная тягучая настойка. Пары одной капли валят с ног быка. Пить самому — глупо; зато дымная фляга заправляется ею до краёв. А кто-нибудь наверняка захочет её купить или подлить.",
    BrewEffect.RefillFlask(Set(RefillSource.Sleep)))

  case object Schnapps extends BrewKind(
    "Шнапс из красавки",
    List(Belladonna, Wormwood, Calendula),
    s"Крепкий, дурманящий, с ягодным послевкусием. Пить его не стоит, а вот Трактирщик берёт по ${BrewRates.SchnappsPrice} серебра за бутылку.",
    BrewEffect.Sellable(BrewRates.SchnappsPrice))

  case object VenomSalve extends BrewKind(
    "Ядовитая смазка",
    List(Belladonna, Wormwood, Nettle),
    "Тёмная маслянистая мазь с запахом красавки. Смазать клинок — и в ближайшем бою каждый удар по HP травит врага; заодно заправляет флягу яда до краёв.",
    BrewEffect.Coat(WeaponCoat.Poison, RefillSource.Poison))

  case object BloodSalve extends BrewKind(
    "Кровавая смазка",
    List(Nettle, Sage, Wormwood),
    "Едкая мазь, от которой не затягиваются раны. Смазать клинок — и в ближайшем бою каждый удар по HP пускает врагу кровь; заодно заправляет флягу крови до краёв.",
    BrewEffect.Coat(WeaponCoat.Bleed, RefillSource.Bleed))

  // ── Отвары из редких трав второго ранга: по одной склянке за рецепт ──────

  case object MushroomMix extends BrewKind(
    "Грибная смесь",
    List(GlaiveMushroom, Nettle, Sage),
    "Толчёные глефовые грибы в склянке, переложенные крапивой, чтобы не рвануло раньше времени. Её не пьют — её бросают под ноги: в бою достаётся всем, кто стоит в поле, и споры травят выживших.",
    BrewEffect.Bomb)

  case object WolfCall extends BrewKind(
    "Волчий зов",
    List(WolfHops, Sage, Wormwood),
    "Жжёный хмель с чем-то ещё, чего не разобрать. Выпить — и запах пойдёт от вас самих: тот, кто держится позади и не показывается, подойдёт ближе. Час, не больше: потом выветрится.",
    BrewEffect.WolfCall)

  case object Sentence extends BrewKind(
    "Зелье приговора",
    List(DoomFlower, Belladonna, Chamomile),
    "Чёрная взвесь, в которой что-то шевелится, стоит отвести взгляд. Выпивший называет род — и тот, кого назвали, уже приговорён: кто-то доберётся до него первым, а герою останется добить. Час, пока имя держится на языке.",
    BrewEffect.Sentence)

  case object Bubble extends BrewKind(
    "Пузырьковый нектар",
    List(BubbleLily, Chamomile, Valerian),
    "Сладкий нектар, заключённый в тонкую плёнку, — она не лопается даже на языке. Выпившего он от чего-то бережёт; от чего именно, травники не сходятся, а те, кто проверил, рассказывают об этом неохотно.",
    BrewEffect.Bubble)

  case object WolfBeer extends BrewKind(
    "Волчье пиво",
    List(WolfHops, Calendula, Chamomile),
    s"Тёмное, густое, с горчинкой от жжёного хмеля. Пьётся тяжело, зато после него и стоится, и держится крепче: +${BrewRates.BoostPct}% к выносливости на час; с зельями Густаво складывается.",
    BrewEffect.Boost("herb:vit", ParamsBuff(0, BrewRates.BoostPct, 0, 0), "Выносливость"))

  case object Moonshine extends BrewKind(
    "Самогон из красавки",
    List(Wormwood, Sage),
    s"Шнапс, перегнанный ещё раз и ещё немного. Пить это не стоит тем более, а вот Трактирщик берёт по ${BrewRates.SchnappsPrice * BrewRates.MoonshineFactor} серебра за бутыль — и не спрашивает, из чего гнали.",
    BrewEffect.Sellable(BrewRates.SchnappsPrice * BrewRates.MoonshineFactor))

  case object MirrorBrew extends BrewKind(
    "Зеркальный настой",
    List(MirageFlower, Valerian, Eyebright),
    s"Мутная вода, в которой отражаешься трижды. Выпить перед боем — и первые ${BrewRates.MirrorCopies} удара уйдут в призрачные копии; копии тают одна за другой, а вы остаётесь.",
    BrewEffect.Mirror(BrewRates.MirrorCopies))

  val values: IndexedSeq[BrewKind] = findValues

  /** Отвары первого ранга — те, что варятся из одних простых трав; за полный
   *  набор сваренных даётся «Зельевар I» (см. Achievement.Brewer1). Отвары на
   *  редкой траве и перегонка сюда не входят: до них зельевару ещё расти. */
  val rank1: IndexedSeq[BrewKind] =
    values.filter(k => k.base.isEmpty && k.recipe.forall(_.herbRank <= 1))

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
