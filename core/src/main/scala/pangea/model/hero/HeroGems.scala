package pangea.model.hero

import pangea.model.battle.Element
import pangea.model.item.{Gem, GemKind}

/** Типизированный фасад над камнями в гнёздах снаряжения. Как [[HeroPassives]],
 *  каждая точка применения спрашивает готовый модификатор; числа берутся с
 *  вариантов [[GemKind]]. В отличие от пассивок, камни СТАКАЮТСЯ: несколько камней
 *  одного вида складывают эффект (по грейдам).
 *
 *  Разбиение на `weapon`/`armor` — камни в оружии дают «оружейную» грань, в прочем
 *  снаряжении — «броневую» (см. [[Equipment.weaponGems]]/[[Equipment.armorGems]]).
 *  Стихийные грани (сапфир/рубин/бриллиант/топаз в оружии, сапфир в снаряжении)
 *  ОТЛОЖЕНЫ — здесь не учитываются. */
final case class HeroGems(weapon: List[Gem], armor: List[Gem]) {

  private def gradeSum(gems: List[Gem], kind: GemKind): Long =
    gems.collect { case g if g.kind == kind => g.grade.toLong }.sum

  // ── Оружейные грани ──────────────────────────────────────────────────────────
  /** Вампиризм: % нанесённого по HP урона, возвращаемого героем в лечение (череп). */
  def vampirismPct: Long = gradeSum(weapon, GemKind.Skull) * GemKind.Skull.WeaponVampPctPerGrade

  /** +% к восстановлению энергии в бою от черепов в оружии. */
  def energyRegenBonusPct: Long = gradeSum(weapon, GemKind.Skull) * GemKind.Skull.WeaponEnergyRegenPctPerGrade

  /** Итоговый множитель регена энергии от камней (≥1.0). */
  def energyRegenMult: Double = 1.0 + energyRegenBonusPct / 100.0

  /** +% к итоговой точности от аметистов в оружии. */
  def accuracyBonusPct: Long = gradeSum(weapon, GemKind.Amethyst) * GemKind.Amethyst.WeaponAccuracyPctPerGrade

  /** Яд, накладываемый при уроне по HP (изумруд в оружии), в % макс.HP моба.
   *  1.5% за грейд, округляется к целому %. 0 — если изумрудов в оружии нет. */
  def weaponPoisonPct: Int = {
    val tenths = gradeSum(weapon, GemKind.Emerald) * GemKind.Emerald.WeaponPoisonTenthPctPerGrade
    ((tenths + 5) / 10).toInt
  }

  // ── Броневые грани ───────────────────────────────────────────────────────────
  /** Реген HP каждый раунд, в промилле макс.HP (череп в снаряжении, 0.5%/грейд). */
  def hpRegenPerMille: Long = gradeSum(armor, GemKind.Skull) * GemKind.Skull.ArmorHpRegenPerMillePerGrade

  /** +% к итоговой защите (аметист в снаряжении). */
  def defenceBonusPct: Long = gradeSum(armor, GemKind.Amethyst) * GemKind.Amethyst.ArmorDefencePctPerGrade

  /** +% к итоговому уклонению (изумруд в снаряжении). */
  def evasionBonusPct: Long = gradeSum(armor, GemKind.Emerald) * GemKind.Emerald.ArmorEvasionPctPerGrade

  /** +% к макс. HP (рубин в снаряжении). */
  def maxHpBonusPct: Long = gradeSum(armor, GemKind.Ruby) * GemKind.Ruby.ArmorMaxHpPctPerGrade

  /** Плоская прибавка к макс. HP (рубин: 10 + 30·(грейд−1) за каждый камень). */
  def flatHp: Long = armor.collect { case g if g.kind == GemKind.Ruby => GemKind.Ruby.flatHp(g.grade) }.sum

  /** +% к макс. Энергии (бриллиант в снаряжении). */
  def energyBonusPct: Long = gradeSum(armor, GemKind.Diamond) * GemKind.Diamond.ArmorEnergyPctPerGrade

  /** +% к шансу получения экипировки в дропе (топаз в снаряжении). */
  def gearDropBonusPct: Long = gradeSum(armor, GemKind.Topaz) * GemKind.Topaz.ArmorGearChancePctPerGrade

  // ── Стихии оружия ────────────────────────────────────────────────────────────
  /** Стихии камней в оружии (без повторов — модификаторы урона стихии не стакают
   *  между одинаковыми камнями, но грейды суммируются в [[elementalDamageMult]]). */
  def weaponElements: Set[Element] = weapon.flatMap(g => Element.of(g.kind)).toSet

  def hasElement(e: Element): Boolean = weaponElements.contains(e)

  /** Множитель стихийного урона от грейдов ВСЕХ стихийных камней в оружии:
   *  1 + 2%·(сумма грейдов). 1.0, если стихийных камней нет. */
  def elementalDamageMult: Double = {
    val gradeSum = weapon.collect { case g if Element.of(g.kind).isDefined => g.grade.toLong }.sum
    1.0 + 0.02 * gradeSum
  }

  /** Множитель урона по броне цели от стихий оружия (произведение armorMult). */
  def armorDamageMult: Double = weaponElements.foldLeft(1.0)((m, e) => m * e.armorMult)

  /** Множитель урона по HP цели от стихий оружия (произведение hpMult). */
  def hpDamageMult: Double = weaponElements.foldLeft(1.0)((m, e) => m * e.hpMult)

  /** Доля урона по броне, дополнительно бьющая по HP (молния, иначе 0). */
  def lightningArmorToHpFrac: Double =
    if (hasElement(Element.Lightning)) Element.Lightning.ArmorToHpFrac else 0.0

  /** Постоянный бонус (в %) к итоговым уклонению/точности от воздуха в оружии (0, если нет). */
  def airEvasionAccuracyBonusPct: Long =
    if (hasElement(Element.Air)) Element.Air.AlwaysBonusPct else 0L
}

object HeroGems {
  val empty: HeroGems = HeroGems(Nil, Nil)
}
