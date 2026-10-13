package pangea.model.hero

import enumeratum._
import pangea.model.item.ItemSet

/** Достижения героя: выдаются сюжетом один раз и навсегда, каждое несёт свой
  * бонус. Хранятся ключами в `heroes.achievements`. */
sealed abstract class Achievement(val title: String, val bonusLine: String) extends EnumEntry

/** Числа достижений, которые читают и сами варианты, — отдельно от компаньона:
  * вариант enum не должен ходить в свой же компаньон, пока тот собирает
  * `values` (иначе инициализация класса встаёт в дедлок). */
object AchievementRates {

  /** На сколько процентов достижение за собранный набор поднимает его главный стат. */
  val SetStatPct: Long = 3L
}

object Achievement extends Enum[Achievement] {
  val values: IndexedSeq[Achievement] = findValues

  /** Довёл долг Кельвина до конца — Мариса может спать спокойно. */
  case object MarisaSavior extends Achievement("Спаситель Марисы", "+10% к выпадаемому серебру в событиях лабиринта")

  /** Оставил Марису посреди материка с пустыми руками. */
  case object Scoundrel extends Achievement("Мерзавец", "+5% к наносимому и получаемому урону в бою")

  /** Сварил в кубе хотя бы по разу каждый отвар первого ранга. */
  case object Brewer1 extends Achievement("Зельевар I", "+2 к интеллекту")

  /** Довёз в деревню мурлоков двадцать единиц снаряжения. */
  case object MurlocFavorite extends Achievement("Любимец Мурлоков", "+1 к ловкости")

  /** Вырезал деревню мурлоков. */
  case object MurlocBane extends Achievement("Гроза Мурлоков", "+1 к силе")

  // ── Собранные наборы ────────────────────────────────────────────────────────
  // За каждый набор, надетый целиком — все двенадцать вещей разом, — своё
  // достижение: прибавка к главному стату набора, та же, что у его двойки, но
  // меньше и навсегда, даже когда набор снят.

  case object StoneGuardSet extends Achievement("Живая крепость",
    s"+${AchievementRates.SetStatPct}% к защите")

  case object WildFlameSet extends Achievement("Укротитель пламени",
    s"+${AchievementRates.SetStatPct}% к атаке")

  case object GhoulSet extends Achievement("Кровопийца",
    s"+${AchievementRates.SetStatPct}% к уклонению")

  case object HunterSet extends Achievement("Зоркий глаз",
    s"+${AchievementRates.SetStatPct}% к точности")

  case object RatSet extends Achievement("Свой среди крыс",
    s"+${AchievementRates.SetStatPct}% к броне")

  case object NecromancerSet extends Achievement("Повелитель мёртвых",
    s"+${AchievementRates.SetStatPct}% к интеллекту")

  /** Достижение за набор, собранный целиком. */
  def forSet(set: ItemSet): Achievement = set match {
    case ItemSet.StoneGuard  => StoneGuardSet
    case ItemSet.WildFlame   => WildFlameSet
    case ItemSet.Ghoul       => GhoulSet
    case ItemSet.Hunter      => HunterSet
    case ItemSet.Rat         => RatSet
    case ItemSet.Necromancer => NecromancerSet
  }

  /** Сколько вещей набора надо надеть разом, чтобы он считался собранным. */
  val FullSetPieces: Int = ItemSet.Thresholds.max

  private def setPct(hero: Hero, a: Achievement): Long =
    if (hero.hasAchievement(a)) AchievementRates.SetStatPct else 0L

  /** +% к защите, атаке, уклонению и точности от собранных наборов. */
  def defencePct(hero: Hero): Long  = setPct(hero, StoneGuardSet)
  def attackPct(hero: Hero): Long   = setPct(hero, WildFlameSet)
  def evasionPct(hero: Hero): Long  = setPct(hero, GhoulSet)
  def accuracyPct(hero: Hero): Long = setPct(hero, HunterSet)

  /** +% к потолку брони от «Своего среди крыс» — как и у двойки «Крысы», в
    * потолок, иначе прибавку срезала бы первая же починка. */
  def armorPct(hero: Hero): Long = setPct(hero, RatSet)

  /** +% к интеллекту от «Повелителя мёртвых». */
  def intPct(hero: Hero): Long = setPct(hero, NecromancerSet)

  /** Сколько интеллекта прибавляет «Зельевар I». */
  val BrewerIntBonus: Long = 2L

  /** Сколько ловкости прибавляет «Любимец Мурлоков» и силы — «Гроза Мурлоков». */
  val MurlocFavoriteAgiBonus: Long = 1L
  val MurlocBaneStrBonus: Long     = 1L

  /** Плоская прибавка к интеллекту от достижений героя. */
  def intBonus(hero: Hero): Long =
    if (hero.hasAchievement(Brewer1)) BrewerIntBonus else 0L

  /** Плоская прибавка к ловкости от достижений героя. */
  def agiBonus(hero: Hero): Long =
    if (hero.hasAchievement(MurlocFavorite)) MurlocFavoriteAgiBonus else 0L

  /** Плоская прибавка к силе от достижений героя. */
  def strBonus(hero: Hero): Long =
    if (hero.hasAchievement(MurlocBane)) MurlocBaneStrBonus else 0L

  /** Сколько процентов сверху к серебру, выпадающему в лабиринте. */
  val SaviorSilverBonusPct: Long = 10L

  /** На сколько процентов Мерзавец бьёт сильнее — и получает сильнее. */
  val ScoundrelDamageBonusPct: Long = 5L

  /** Множитель серебра из лабиринта с учётом достижений героя. */
  def silverPct(hero: Hero): Long =
    100L + (if (hero.hasAchievement(MarisaSavior)) SaviorSilverBonusPct else 0L)

  /** Множитель урона в бою (в обе стороны) с учётом достижений героя. */
  def damagePct(hero: Hero): Long =
    100L + (if (hero.hasAchievement(Scoundrel)) ScoundrelDamageBonusPct else 0L)
}
