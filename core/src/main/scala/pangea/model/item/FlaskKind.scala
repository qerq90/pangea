package pangea.model.item

import enumeratum._
import pangea.model.battle.Element

/** Разновидность фляги: имя, короткая подпись для боя и механика
 *  ([[FlaskEffect]]). Все числа фляг живут здесь. Фляга к уровню не привязана —
 *  редкость даёт лишь запас зарядов ([[FlaskKind.chargesFor]]) и цену у Ришелье
 *  ([[FlaskKind.priceFor]]); сила эффекта у всех редкостей одна. */
sealed abstract class FlaskKind(
  val label:  String,
  val effect: FlaskEffect
) extends EnumEntry {
  /** Полное имя предмета с меткой редкости: «🔵 Фляга кузнеца». */
  def itemName(rarity: Rarity): String = s"${rarity.emoji} $label"
}

/** Числа фляг. Вынесены из компаньона [[FlaskKind]] нарочно: варианты читают их
 *  в конструкторе, а `findValues` компаньона инициализирует варианты — два
 *  потока, взявшиеся за вариант и за компаньон одновременно, ждали бы друг
 *  друга навсегда (класс-инициализация в JVM). */
object FlaskRates {
  /** Фляга целителя: % макс. HP и % макс. энергии за глоток. */
  val HealPct: Int       = 25
  val HealEnergyPct: Int = 5
  /** Фляга кузнеца: % макс. брони. */
  val ArmorPct: Int = 25
  /** Фляга бодрости: % макс. энергии. */
  val EnergyPct: Int = 30
  /** Вампирская: сколько ударов по HP и на сколько % урона они лечат. */
  val VampiricHits: Int = 3
  val VampiricPct: Int  = 30
  /** Отрава: сколько раундов оружие смазано и на сколько % пускает кровь за удар
   *  (яд стакается как от отравленного оружия — см. `Poison.OnHit`). */
  val VenomRounds: Int   = 5
  val VenomBleedPct: Int = 2
}

object FlaskKind extends Enum[FlaskKind] {

  case object Healer    extends FlaskKind("Фляга целителя",  FlaskEffect.HealPercent(FlaskRates.HealPct))
  case object Smith     extends FlaskKind("Фляга кузнеца",   FlaskEffect.ArmorPercent(FlaskRates.ArmorPct))
  case object Vigor     extends FlaskKind("Фляга бодрости",  FlaskEffect.EnergyPercent(FlaskRates.EnergyPct))
  case object Fire      extends FlaskKind("Огненная фляга",  FlaskEffect.Splash(Element.Fire))
  case object Cold      extends FlaskKind("Морозная фляга",  FlaskEffect.Splash(Element.Cold))
  case object Lightning extends FlaskKind("Грозовая фляга",  FlaskEffect.Splash(Element.Lightning))
  case object Air       extends FlaskKind("Ветреная фляга",  FlaskEffect.Splash(Element.Air))
  case object Cleansing extends FlaskKind("Фляга очищения",  FlaskEffect.Cleanse)
  case object Smoke     extends FlaskKind("Дымная фляга",    FlaskEffect.Smoke)
  case object Vampiric  extends FlaskKind("Вампирская фляга", FlaskEffect.Vampiric(FlaskRates.VampiricHits, FlaskRates.VampiricPct))
  case object Venom     extends FlaskKind("Фляга отравы",    FlaskEffect.Venom(FlaskRates.VenomRounds))

  val values: IndexedSeq[FlaskKind] = findValues

  /** Стихийные фляги — одна «семья» при выпадении: сперва решается, что выпала
   *  стихийная, потом какая именно стихия. */
  val elemental: List[FlaskKind] = List(Fire, Cold, Lightning, Air)

  /** Семьи для равновероятного выбора при дропе: восемь, стихийные считаются одной. */
  val families: List[List[FlaskKind]] =
    List(List(Healer), List(Smith), List(Vigor), elemental, List(Cleansing), List(Smoke), List(Vampiric), List(Venom))

  /** Запас зарядов по редкости. */
  def chargesFor(rarity: Rarity): Int = rarity match {
    case Rarity.Gray   => 3
    case Rarity.White  => 4
    case Rarity.Green  => 5
    case Rarity.Blue   => 6
    case Rarity.Purple => 7
    case Rarity.Violet => 8
    case Rarity.Orange => 10
  }

  /** Цена у Ришелье: сто серебра за серую и по сотне за каждую следующую
   *  редкость; легендарная — тысяча. */
  def priceFor(rarity: Rarity): Long = rarity match {
    case Rarity.Gray   => 100L
    case Rarity.White  => 200L
    case Rarity.Green  => 300L
    case Rarity.Blue   => 400L
    case Rarity.Purple => 500L
    case Rarity.Violet => 600L
    case Rarity.Orange => 1000L
  }

  /** Редкость выпавшей фляги (в %, сумма = 100). */
  val rarityWeights: List[(Rarity, Int)] = List(
    Rarity.Gray   -> 35,
    Rarity.White  -> 25,
    Rarity.Green  -> 20,
    Rarity.Blue   -> 10,
    Rarity.Purple -> 5,
    Rarity.Violet -> 4,
    Rarity.Orange -> 1
  )

  /** Короткая подпись стихийной фляги для кнопки боя. */
  def splashShort(e: Element): String = e match {
    case Element.Fire      => "Огонь"
    case Element.Cold      => "Мороз"
    case Element.Lightning => "Гроза"
    case Element.Air       => "Ветер"
  }

  /** Что даёт прок стихии — для описания в инвентаре. */
  def splashText(e: Element): String = e match {
    case Element.Fire      => "враг гарантированно вспыхивает (горение, как от прока Огня)."
    case Element.Cold      => s"холод гарантированно сковывает защиту врага (−${Element.Cold.DefenceReductionCut}% защиты)."
    case Element.Lightning => s"молния гарантированно выжигает ${Element.LightningEnergyBurnPct}% энергии врага."
    case Element.Air       => s"ветер на ${Element.Air.ProcTurns} хода поднимает вам точность и уклонение на ${Element.Air.ProcBonusPct}%."
  }

  /** Вид по эффекту предмета (для кнопок и описаний старых фляг). */
  def ofEffect(effect: FlaskEffect): Option[FlaskKind] = values.find(_.effect == effect)
}
