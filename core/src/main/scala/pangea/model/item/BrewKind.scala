package pangea.model.item

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import pangea.model.battle.Regen
import pangea.model.hero.WeaponCoat
import pangea.model.stats.ParamsBuff

/** Что делает отвар, когда его пьют из инвентаря. */
sealed trait BrewEffect
object BrewEffect {
  /** Снимает одну травму на выбор — лёгкую или среднюю; тяжёлые не берёт. */
  case object CureTrauma extends BrewEffect
  /** Заправляет флягу на поясе целиком — если она из этих источников. */
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
  /** Смрад, который ведёт героя к Гнилому Джо: ближайшая встреча в лабиринте — он. */
  case object RotCall extends BrewEffect
  /** Призрачные копии: первые `copies` ударов по герою уходят в них. */
  final case class Mirror(copies: Int) extends BrewEffect
  /** Приговор: герой называет расу, и следующая встреча — её именное существо,
    * добитое кем-то до него. */
  case object Sentence extends BrewEffect
  /** Пузырь: ближайшая смерть героя не состоится — он останется с одним HP. */
  case object Bubble extends BrewEffect
  /** Боевой отвар: в бою делает ровно то же, что делала склянка того же толка
    * (см. [[PotionKind]]). С карточки его не пьют — он работает из ячейки
    * пояса, и больше нигде. */
  final case class Potion(kind: PotionKind) extends BrewEffect
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
  /** Применяется ли с карточки (кнопка «Использовать»). Заправка и смазка — свои кнопки,
   *  а грибную смесь не пьют вовсе: её бросают под ноги врагам в бою. */
  def drinkable: Boolean = effect match {
    case BrewEffect.CureTrauma | BrewEffect.InstantRest | BrewEffect.Boost(_, _, _) => true
    case BrewEffect.WolfCall | BrewEffect.RotCall | BrewEffect.Mirror(_)            => true
    case BrewEffect.Sentence | BrewEffect.Bubble                                     => true
    case _                                                                           => false
  }

  /** Кладётся ли в ячейку пояса. На пояс идёт всё, чем в бою можно
    * воспользоваться, — то есть почти всё: что пьют, что льют на клинок и что
    * бросают под ноги. Не идёт четыре: шнапс с самогоном носят Трактирщику, а
    * не в бой; сонным дурманом дымную флягу заправляют в спокойном месте — в
    * сече переливать его из склянки во флягу поздно; а приговор пьют, когда
    * выбирают, с кем встретиться, — такое решают до боя, не посреди него. */
  def beltable: Boolean = this match {
    case BrewKind.Schnapps | BrewKind.Moonshine    => false
    case BrewKind.SleepingDope | BrewKind.Sentence => false
    case _                                         => true
  }

  /** Боевой отвар — тот, что заменяет склянку: только из ячейки пояса и только
    * в бою (см. [[BrewEffect.Potion]]). */
  def combat: Boolean = effect match {
    case BrewEffect.Potion(_) => true
    case _                    => false
  }

  /** Отвар, который идёт в рецепт вместе с травами: перегонка берёт готовую
    * склянку и доводит её до ума. Метод, а не поле с умолчанием: значение по
    * умолчанию в конструкторе живёт в компаньоне, и вариант ждал бы его
    * инициализации — дедлок на ровном месте (см. заметку у `poisons` розы). */
  def base: Option[BrewKind] = this match {
    case BrewKind.Moonshine => Some(BrewKind.Schnapps)
    case _                  => None
  }

  /** Сколько склянок выходит из одного рецепта: редкая трава в составе — одна;
    * собранное не из трав (узелок гнилого зова) — тоже одно. */
  def portions: Int =
    if (recipe.exists(_.herbRank >= 2) || !recipe.forall(_.isHerb)) BrewRates.RarePortions
    else BrewRates.Portions

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

  /** Гнилой зов: сколько смрад узелка ведёт героя к Гнилому Джо (час), и имя
    * его бафа. */
  val RotCallMs: Long      = 60L * 60L * 1000L
  val RotCallBoost: String = "brew:rotCall"

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

  // ── Боевые отвары: числа — от склянок со старых поясов, но лечение и броня
  // крепче (по 40%): отвар занимает ячейку пояса, а не идёт зарядом в придачу.

  /** Сколько здоровья возвращает зверобойный отвар (% от максимума). */
  val PotionHealPct: Int = 40

  /** Сколько брони возвращает дубильный настой (% от максимума). */
  val PotionArmorPct: Int = 40

  /** Сколько энергии возвращает «Второе дыхание» (% от максимума). */
  val PotionEnergyPct: Int = 25

  /** Прибавка кошачьего настоя к уклонению (в очках шанса). */
  val PotionDodgePct: Int = 10

  /** Прибавка яростной настойки к атаке (% от текущей). */
  val PotionAtkPct: Int = 5

  /** Прибавка отвара стойкости к защите (% от текущей). */
  val PotionDefencePct: Int = 10

  /** Сколько раундов держатся временные прибавки боевых отваров. */
  val PotionBuffRounds: Int = 5

  /** Живучий сбор: сколько HP в раунд. Число общее с регеном — каждый раунд
    * оно слабеет на [[Regen.DecayPerRound]]. */
  val PotionRegenPct: Int = Regen.OnDrink
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

  // ── Боевые отвары: те, что носят в ячейке пояса ──────────────────────────
  // Все они идут через зверобой: трава простая, но единственная из простых,
  // что держит отвар, годный для сечи. С карточки их не пьют — они работают
  // из пояса и делают то же, что делали склянки на старых поясах.

  case object WortDecoction extends BrewKind(
    "Зверобойный отвар",
    List(StJohnsWort, Chamomile, Calendula),
    s"Красный, как свежий порез, и пахнет раздавленным стеблем. На полке ему не место — его носят при себе: глоток посреди сечи возвращает ${BrewRates.PotionHealPct}% здоровья и держит на ногах того, кому держаться уже нечем.",
    BrewEffect.Potion(PotionKind.Healing))

  case object HardyBrew extends BrewKind(
    "Живучий сбор",
    List(StJohnsWort, Chamomile, Nettle, Sage),
    s"Горький сбор, заваренный до черноты. Пьётся не ради сил, а ради упрямства: тело начинает затягивать своё само — ${BrewRates.PotionRegenPct}% здоровья в раунд, и с каждым раундом всё скупее.",
    BrewEffect.Potion(PotionKind.Regeneration))

  case object TanningBrew extends BrewKind(
    "Дубильный настой",
    List(StJohnsWort, Nettle, Wormwood, Sage),
    s"Вяжущая бурда цвета крепкого чая, и пить её незачем — ею поливают доспех. Кожа на глазах дубеет, ремни стягиваются, бляхи садятся на место, и броня снова держит удар: ${BrewRates.PotionArmorPct}% её возвращается.",
    BrewEffect.Potion(PotionKind.Metal))

  case object SteadfastBrew extends BrewKind(
    "Отвар стойкости",
    List(StJohnsWort, Nettle, Calendula),
    s"Тёплый, с привкусом железа. От него тяжелеют ноги и разом пропадает охота отступать: +${BrewRates.PotionDefencePct}% к защите на ${BrewRates.PotionBuffRounds} раундов.",
    BrewEffect.Potion(PotionKind.Defence))

  case object CatBrew extends BrewKind(
    "Кошачий настой",
    List(StJohnsWort, Valerian),
    s"Пахнет так, что на запах из темноты выходят коты — и смотрят, не мигая. Выпивший перенимает их шаг: +${BrewRates.PotionDodgePct}% к уклонению на ${BrewRates.PotionBuffRounds} раундов.",
    BrewEffect.Potion(PotionKind.Evasion))

  case object SecondWind extends BrewKind(
    "Второе дыхание",
    List(StJohnsWort, Sage),
    s"Обжигает горло и разом разгоняет туман в голове. Возвращает ${BrewRates.PotionEnergyPct}% энергии — ровно столько, чтобы хватило ещё на одно умение.",
    BrewEffect.Potion(PotionKind.Energy))

  case object FuryTincture extends BrewKind(
    "Яростная настойка",
    List(StJohnsWort, Wormwood, Belladonna),
    s"Горечь, от которой темнеет в глазах и становится всё равно, сколько их против вас: +${BrewRates.PotionAtkPct}% к атаке на ${BrewRates.PotionBuffRounds} раундов.",
    BrewEffect.Potion(PotionKind.Attack))

  case object StingingExtract extends BrewKind(
    "Жгучая вытяжка",
    List(StJohnsWort, Belladonna, Nettle),
    "Чёрная вытяжка, которой мажут не рану, а кромку. Жжёт даже сквозь ткань, а на лезвии держится до конца боя — и каждый удар по живому оставляет в нём яд.",
    BrewEffect.Potion(PotionKind.Poison))

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

  /** Не отвар, а материальный компонент: узелок из того, что срезают с крыс в
    * канализации. Его не пьют — развязывают, и смрад ведёт героя к Гнилому
    * Джо. Собирается в кубе своим рецептом (см. CubeCraft) и в отвары первого
    * ранга не входит. */
  case object RotCall extends BrewKind(
    "Гнилой зов",
    // Шкурки с крыс падают чаще хвостов, поэтому их в узелок идёт больше.
    List(PlagueWorms, RatTail, RatTail, RatPelt, RatPelt, RatPelt),
    "Тугой узелок, от которого несёт падалью. Развяжешь — и смрад сам поведёт тебя туда, где гниль гуще, к тому, кто гнилью живёт. Час, не больше: потом выдохнется.",
    BrewEffect.RotCall)

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
   *  набор сваренных даётся «Зельевар I» (см. Achievement.Brewer1). Боевые
   *  отвары на зверобое тоже отсюда: их восемь, и они в набор входят. Отвары на
   *  редкой траве, перегонка и то, что варят не из трав (гнилой зов), сюда не
   *  входят. */
  val rank1: IndexedSeq[BrewKind] =
    values.filter(k => k.base.isEmpty && k.recipe.forall(m => m.isHerb && m.herbRank <= 1))

  /** Боевые отвары — те, что носят в ячейке пояса (см. [[BrewKind.combat]]). */
  val combatBrews: IndexedSeq[BrewKind] = values.filter(_.combat)

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
