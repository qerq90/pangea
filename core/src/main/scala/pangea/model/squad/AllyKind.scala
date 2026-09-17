package pangea.model.squad

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import pangea.model.battle.Element
import pangea.model.monster.Race
import pangea.model.stats.FightStats

/** Наёмник-союзник: кто он, чем бьёт и сколько стоит. Сила растёт с уровнем
 *  героя (`AliensLvL` = уровень героя): статы — ставка × уровень. Умения у всех
 *  одни и те же ([[AllySkill]]), различаются лишь числа и стихия удара. */
sealed abstract class AllyKind(
  val name:     String,
  val race:     Race,
  val element:  Element,
  val hpPerLvl:       Long,
  val armorPerLvl:    Long,
  val atkPerLvl:      Long,
  val energyPerLvl:   Long,
  val accuracyPerLvl: Long,
  val defencePerLvl:  Long,
  val evasionPerLvl:  Long
) extends EnumEntry {

  def stats(lvl: Long): FightStats = FightStats(
    atk      = atkPerLvl * lvl,
    hp       = hpPerLvl * lvl,
    armor    = armorPerLvl * lvl,
    defence  = defencePerLvl * lvl,
    evasion  = evasionPerLvl * lvl,
    accuracy = accuracyPerLvl * lvl,
    energy   = energyPerLvl * lvl
  )

  /** Сколько энергии восстанавливает за раунд. */
  def energyRegen(lvl: Long): Long = AllyRates.EnergyRegenPerLvl * lvl
}

/** Числа союзников — отдельно от компаньона (см. FlaskRates). */
object AllyRates {
  val EnergyRegenPerLvl: Long = 7L

  /** Найм Йоргена: серебро за каждый уровень героя. */
  val HumanSilverPerLvl: Long = 500L
  /** Найм Плюха и Брамбла: отваров — уровень героя на это, но не меньше одного. */
  val BrewPerLevels: Long = 5L

  /** Сколько союзник отсутствует после свитка (сутки). */
  val AwayMs: Long = 24L * 60L * 60L * 1000L

  /** На сколько нанимается наёмник — и сколько потом отдыхает в отлучке. */
  val HireMs: Long = 12L * 60L * 60L * 1000L
  val OffDutyMs: Long = 12L * 60L * 60L * 1000L

  /** Мест в отряде вместе с героем. */
  val Positions: Int = 4
}

object AllyKind extends Enum[AllyKind] {

  /** Йорген Кремень: воин, бьёт огнём. */
  case object Human extends AllyKind("Йорген Кремень", Race.Human, Element.Fire,
    hpPerLvl = 125L, armorPerLvl = 150L, atkPerLvl = 20L, energyPerLvl = 100L,
    accuracyPerLvl = 100L, defencePerLvl = 40L, evasionPerLvl = 100L)

  /** Плюх: мурлок-ловкач, бьёт молнией. */
  case object Murloc extends AllyKind("Плюх", Race.Murloc, Element.Lightning,
    hpPerLvl = 100L, armorPerLvl = 100L, atkPerLvl = 30L, energyPerLvl = 100L,
    accuracyPerLvl = 150L, defencePerLvl = 30L, evasionPerLvl = 150L)

  /** Брамбл Медноус: гном-воин, бьёт холодом. */
  case object Gnome extends AllyKind("Брамбл Медноус", Race.Gnome, Element.Cold,
    hpPerLvl = 80L, armorPerLvl = 175L, atkPerLvl = 20L, energyPerLvl = 100L,
    accuracyPerLvl = 100L, defencePerLvl = 50L, evasionPerLvl = 60L)

  val values: IndexedSeq[AllyKind] = findValues

  /** Сколько отваров берут Плюх и Брамбл на этом уровне героя. */
  def brewsFor(lvl: Long): Long = (lvl / AllyRates.BrewPerLevels).max(1L)

  implicit val encoder: Encoder[AllyKind] = (k: AllyKind) => k.entryName.asJson
  implicit val decoder: Decoder[AllyKind] = (c: HCursor) => c.as[String].map(AllyKind.withName)
}

/** Умения союзника — одни на всех: применяются случайно из тех, на что хватает
 *  энергии и что сейчас к месту. */
sealed abstract class AllySkill(val costPerLvl: Long) extends EnumEntry {
  def cost(lvl: Long): Long = costPerLvl * lvl
}

object AllySkill extends Enum[AllySkill] {
  /** Быстрый удар: половина атаки. */
  case object QuickStrike     extends AllySkill(5L)
  /** Исцеляющая фляга: 30% макс. HP, только раненому. */
  case object HealingFlask    extends AllySkill(8L)
  /** Экстренная починка: 20% макс. брони, только с побитой бронёй. */
  case object EmergencyRepair extends AllySkill(10L)
  /** Дробящий удар: 0,4 атаки прямо в HP, мимо брони и защиты. */
  case object CrushingStrike  extends AllySkill(16L)

  val values: IndexedSeq[AllySkill] = findValues

  val QuickStrikeFactor: Double    = 0.5
  val HealPct: Long                = 30L
  val RepairPct: Long              = 20L
  val CrushingStrikeFactor: Double = 0.4
}
