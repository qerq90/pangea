package pangea.model.hero

import pangea.model.item.PassiveKind

/** Типизированный фасад над набором пассивок героя. Каждая точка применения
 *  (бой/лут/подземелье/инвентарь) спрашивает у него ИМЕННО тот модификатор,
 *  который ей нужен, а не проверяет строки/наличие конкретного enum-а вручную.
 *  Числа берутся с самих [[PassiveKind]] — здесь только композиция.
 *
 *  Дубли уже схлопнуты во множестве `kinds` («работает только одна»). */
final case class HeroPassives(kinds: Set[PassiveKind]) {

  private def has(k: PassiveKind): Boolean = kinds.contains(k)

  // ── Подземелье (шанс встречи боевого события) ───────────────────────────────
  /** Множитель к весу боевого события: Охотника ×1.2, Скрытный ×0.8. Обе на шлеме,
   *  поэтому одновременно активна максимум одна. */
  def battleEncounterFactor: Double = {
    var f = 1.0
    if (has(PassiveKind.Hunter)) f *= 1.0 + PassiveKind.Hunter.EncounterBonusPct / 100.0
    if (has(PassiveKind.Stealthy)) f *= 1.0 - PassiveKind.Stealthy.EncounterReductionPct / 100.0
    f
  }

  // ── Уклонение / попадание ────────────────────────────────────────────────────
  /** Прибавка (в п.п.) к итоговому шансу уклонения героя: Быстрые ноги +5,
   *  Сливающиеся −шанс попадания врага = +уклонение героя. */
  def dodgeBonusPct: Long = {
    var b = 0L
    if (has(PassiveKind.QuickFeet)) b += PassiveKind.QuickFeet.DodgeBonusPct
    if (has(PassiveKind.Blending)) b += PassiveKind.Blending.EnemyHitPenaltyPct
    b
  }

  /** Дополнительная прибавка (в п.п.) к шансу уклона ТОЛЬКО при бегстве
   *  (Сверкающие +25). Применяется поверх [[dodgeBonusPct]]. */
  def fleeDodgeBonusPct: Long =
    if (has(PassiveKind.Glittering)) PassiveKind.Glittering.FleeDodgeBonusPct else 0L

  // ── Плоские бафы статов (не мультипликативные — как зелья пояса) ─────────────
  /** Плоская прибавка к защите: +5% от переданного значения защиты (Укреплённые). */
  def defenceFlatBonus(defence: Long): Long =
    if (has(PassiveKind.Reinforced)) (defence * PassiveKind.Reinforced.DefenceBonusPct / 100L).max(1L) else 0L

  /** Плоская прибавка к точности: +5% от переданного значения точности (Точности). */
  def accuracyFlatBonus(accuracy: Long): Long =
    if (has(PassiveKind.Precise)) (accuracy * PassiveKind.Precise.AccuracyBonusPct / 100L).max(1L) else 0L

  // ── Урон и лечение ───────────────────────────────────────────────────────────
  /** Множитель финального урона игрока: Разбойника ×1.05. */
  def finalDamageMult: Double =
    if (has(PassiveKind.Robber)) 1.0 + PassiveKind.Robber.DamageBonusPct / 100.0 else 1.0

  /** Множитель активного лечения в бою (скилл/фляга/зелье пояса): Целителя ×1.10. */
  def healMult: Double =
    if (has(PassiveKind.Healer)) 1.0 + PassiveKind.Healer.HealBonusPct / 100.0 else 1.0

  /** Множитель восстановления энергии в бою: Сосредоточенный ×1.10. */
  def energyRegenMult: Double =
    if (has(PassiveKind.Focused)) 1.0 + PassiveKind.Focused.EnergyRegenBonusPct / 100.0 else 1.0

  // ── Реген каждый ход ─────────────────────────────────────────────────────────
  /** % макс. HP, восстанавливаемый каждый раунд перед атакой (Целебный). */
  def hpRegenPct: Long = if (has(PassiveKind.Healing)) PassiveKind.Healing.HpRegenPct else 0L

  /** % макс. брони, восстанавливаемый каждый ход перед атакой (Самовосстанавливающийся). */
  def armorRegenPct: Long =
    if (has(PassiveKind.SelfRepairing)) PassiveKind.SelfRepairing.ArmorRegenPct else 0L

  // ── Пороговые срабатывания в бою ─────────────────────────────────────────────
  def hasImpenetrable: Boolean = has(PassiveKind.Impenetrable)
  def hasToughness: Boolean    = has(PassiveKind.Toughness)
  def hasSpiky: Boolean        = has(PassiveKind.Spiky)
  def hasQuickHands: Boolean   = has(PassiveKind.QuickHands)

  // ── Лут ──────────────────────────────────────────────────────────────────────
  def hasTaxidermist: Boolean = has(PassiveKind.Taxidermist)
  def hasJeweler: Boolean     = has(PassiveKind.Jeweler)
  def hasMarauder: Boolean    = has(PassiveKind.Marauder)

  // ── Инвентарь ────────────────────────────────────────────────────────────────
  /** Дополнительные слоты сумки от Тайника (0, если пассивки нет). */
  def extraInventorySlots: Long = if (has(PassiveKind.Stash)) PassiveKind.Stash.ExtraSlots else 0L
}

object HeroPassives {
  val empty: HeroPassives = HeroPassives(Set.empty)
}
