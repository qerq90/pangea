package pangea.model.hero

import enumeratum._

/** Достижения героя: выдаются сюжетом один раз и навсегда, каждое несёт свой
  * бонус. Хранятся ключами в `heroes.achievements`. */
sealed abstract class Achievement(val title: String, val bonusLine: String) extends EnumEntry

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
