package pangea.model.skill

import enumeratum._
import io.circe.{Decoder, Encoder}
import pangea.model.hero.Hero

/**
 * Активный навык — кнопка в бою, заканчивающая ход. Падает на оружие (3 «оружейных»)
 * и на нагрудник (2 «бронных») в виде ровно одного навыка на предмет. Все формулы
 * аддитивные по статам героя; ±20% разброс применяется в момент броска снаружи.
 *
 * `kind` определяет, чем считается результат — урон по врагу или лечение себя.
 * Защита моба не учитывается (у мобов её нет).
 */
sealed abstract class Skill(
  val label:           String,
  val cooldown:        Int,
  val kind:            Skill.Kind,
  val hitTemplate:     String
) extends EnumEntry {
  /** Базовое значение эффекта без рандома: для DamageSkill — урон, для HealSkill — лечение. */
  def baseValue(hero: Hero, nowMs: Long): Double

  /** Учитывается ли defence моба при расчёте урона от этого скилла. По умолчанию
   *  нет — большинство скиллов «пробивают» защиту. Включается точечно (например,
   *  быстрый удар бьёт слабо и упирается в броню). */
  def appliesDefenceReduction: Boolean = false
}

object Skill extends Enum[Skill] {
  val values: IndexedSeq[Skill] = findValues

  sealed trait Kind
  object Kind {
    case object Damage extends Kind
    case object Heal   extends Kind
  }

  // ── Оружейные навыки (выпадают на Weapon) ───────────────────────────────────
  case object SweepingStrike extends Skill(
    label       = "Размашистый удар",
    cooldown    = 2,
    kind        = Kind.Damage,
    hitTemplate = "Размашистый удар по дуге достиг врага нанеся ему {} урона!"
  ) {
    def baseValue(hero: Hero, nowMs: Long): Double = {
      val b = hero.effectiveBaseStats(nowMs)
      val f = hero.effectiveFightStats(nowMs)
      0.4 * b.str + 0.4 * b.int + 0.4 * f.atk
    }
  }

  case object QuickStrike extends Skill(
    label       = "Быстрый удар",
    cooldown    = 1,
    kind        = Kind.Damage,
    hitTemplate = "Быстрый выпад застал врасплох нанеся {} урона!"
  ) {
    def baseValue(hero: Hero, nowMs: Long): Double = {
      val b = hero.effectiveBaseStats(nowMs)
      val f = hero.effectiveFightStats(nowMs)
      0.25 * b.agi + 0.4 * f.accuracy + 0.5 * b.int
    }
    override val appliesDefenceReduction: Boolean = true
  }

  case object CunningStrike extends Skill(
    label       = "Хитрый удар",
    cooldown    = 3,
    kind        = Kind.Damage,
    hitTemplate = "Подловив оппонента вы нашли его слепую зону и наносите ему {} урона!"
  ) {
    def baseValue(hero: Hero, nowMs: Long): Double = {
      val b = hero.effectiveBaseStats(nowMs)
      val f = hero.effectiveFightStats(nowMs)
      0.75 * b.str + 0.5 * b.int + 0.2 * f.atk + 0.5 * f.accuracy
    }
  }

  // ── Бронные навыки (выпадают на ChestPlate) ────────────────────────────────
  case object MinorHeal extends Skill(
    label       = "Малое исцеление",
    cooldown    = 2,
    kind        = Kind.Heal,
    hitTemplate = "Применив заклинание вы исцеляетесь на {} HP!"
  ) {
    def baseValue(hero: Hero, nowMs: Long): Double = {
      val b = hero.effectiveBaseStats(nowMs)
      0.75 * b.vit + 1.0 * b.int + 0.2 * hero.effectiveMaxHp(nowMs)
    }
  }

  case object Ram extends Skill(
    label       = "Таран",
    cooldown    = 3,
    kind        = Kind.Damage,
    hitTemplate = "Используя тяжесть своего тела и доспеха, вы врезаетесь во врага и вдавливаете его в стену, нанеся ему {} урона!"
  ) {
    def baseValue(hero: Hero, nowMs: Long): Double = {
      val b = hero.effectiveBaseStats(nowMs)
      val f = hero.effectiveFightStats(nowMs)
      0.5 * f.defence + 0.75 * b.vit + 0.4 * b.int + 0.1 * hero.effectiveMaxHp(nowMs)
    }
  }

  val weaponSkills: List[Skill] = List(SweepingStrike, QuickStrike, CunningStrike)
  val armorSkills:  List[Skill] = List(MinorHeal, Ram)

  implicit val encoder: Encoder[Skill] = Encoder.encodeString.contramap(_.entryName)
  implicit val decoder: Decoder[Skill] = Decoder.decodeString.emap(s =>
    withNameOption(s).toRight(s"Unknown skill: $s"))
}
