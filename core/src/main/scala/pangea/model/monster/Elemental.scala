package pangea.model.monster

import enumeratum._
import pangea.model.battle.Element
import pangea.model.stats.FightStats

/** Вид элементаля-минибосса. Все элементали — раса [[Race.Elemental]] и потому
 *  не подвержены яду и кровотечению; каждый вид дополнительно завязан на свою
 *  стихию и получает от неё свои иммунитеты и уязвимости.
 *
 *  Статы считаются от `BossLvL` (см. [[Elemental.bossLvl]]) — базовые числа
 *  живут ЗДЕСЬ, на варианте, как у [[pangea.model.item.PassiveKind]]. */
sealed abstract class Elemental(
  val label:    String,
  val genitive: String,
  val element:  Element
) extends EnumEntry {

  /** Боевые статы элементаля на данном уровне босса. */
  def stats(bossLvl: Long): FightStats

  /** Сколько энергии элементаль восстанавливает за раунд. */
  def energyRegen(bossLvl: Long): Long

  /** Опыт за победу. */
  def expReward(bossLvl: Long): Long
}

object Elemental extends Enum[Elemental] {

  val values: IndexedSeq[Elemental] = findValues

  /** Уровень босса: `(уровень героя − 1) / 7`, округление вниз, минимум 1. */
  def bossLvl(heroLvl: Long): Long = ((heroLvl - 1L) / 7L).max(1L)

  // ── Огненный ────────────────────────────────────────────────────────────────
  case object Fire extends Elemental("Огненный", "Огненного", Element.Fire) {

    val HpPerLvl: Long       = 1500L
    val ArmorPerLvl: Long    = 750L
    val AtkPerLvl: Long      = 250L
    val EnergyPerLvl: Long   = 150L
    val AccuracyPerLvl: Long = 250L
    val EvasionPerLvl: Long  = 100L
    val EnergyRegenPerLvl: Long = 7L
    val ExpPerLvl: Long      = 200L

    /** Доля урона, которую элементаль получает от огненного оружия — своя стихия
     *  ему почти не вредит. */
    val FireDamageTakenPct: Long = 20L

    /** Насколько сильнее бьёт по нему холод — противоположная стихия. */
    val ColdDamageTakenPct: Long = 150L

    /** Шипы: доля урона обычной атаки, возвращаемая атакующему, пока у элементаля
     *  цела броня. Вместе с шипами он поджигает героя. */
    val ThornsPct: Long   = 1L
    val ThornsBurnPct: Int = 1

    /** Шанс, что обычная атака элементаля подожжёт героя. */
    val IgniteChancePct: Long = 50L

    // ── Способности (применяются по кругу) ──────────────────────────────────
    /** Огненный всплеск: доля атаки в урон и на сколько % поджигает героя. */
    val SplashDamageFactor: Double = 0.4
    val SplashBurnPct: Int         = 1
    val SplashCostPerLvl: Long     = 7L

    /** Сфера огня: сколько сфер собирается до смерча и сколько стоит каждая. */
    val OrbsToBurst: Int       = 3
    val OrbCostPerLvl: Long    = 14L
    /** Смерч добавляет к двойной атаке по столько % от макс. HP и брони героя. */
    val BurstHeroStatPct: Long = 5L
    /** Шанс травмы, если смерч дошёл до HP героя. */
    val BurstTraumaChancePct: Long = 20L

    /** Огненный щит: сколько % максимумов элементаль себе возвращает. */
    val ShieldArmorPct: Long  = 20L
    val ShieldHpPct: Long     = 5L
    val ShieldCostPerLvl: Long = 7L

    def stats(bossLvl: Long): FightStats = FightStats(
      atk      = AtkPerLvl * bossLvl,
      hp       = HpPerLvl * bossLvl,
      armor    = ArmorPerLvl * bossLvl,
      defence  = 0L,
      evasion  = EvasionPerLvl * bossLvl,
      accuracy = AccuracyPerLvl * bossLvl,
      energy   = EnergyPerLvl * bossLvl
    )

    def energyRegen(bossLvl: Long): Long = EnergyRegenPerLvl * bossLvl
    def expReward(bossLvl: Long): Long   = ExpPerLvl * bossLvl
  }

  /** Вид по названию стихии — для восстановления из сохранённого боя. */
  def byName(name: String): Option[Elemental] = values.find(_.entryName == name)
}
