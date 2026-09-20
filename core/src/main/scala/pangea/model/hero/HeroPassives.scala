package pangea.model.hero

import pangea.model.item.PassiveKind

/** Типизированный фасад над набором пассивок героя. Каждая точка применения
 *  (бой/лут/подземелье/инвентарь) спрашивает у него ИМЕННО тот модификатор,
 *  который ей нужен, а не проверяет строки/наличие конкретного enum-а вручную.
 *  Числа берутся с самих [[PassiveKind]] — здесь только композиция.
 *
 *  Дубли уже схлопнуты во множестве `kinds` («работает только одна»).
 *  `strength` — множитель к числу пассивки от понимания её руны (см.
 *  `RuneData.mult`); чего нет в карте — ×1. Растёт само число (процент, доля),
 *  а не шанс срабатывания у пороговых пассивок; бинарные (Мародёр) и Тайник
 *  (его слоты вшиты в вместимость сумки при надевании) не растут. */
final case class HeroPassives(kinds: Set[PassiveKind], strength: Map[PassiveKind, Double] = Map.empty) {

  private def has(k: PassiveKind): Boolean = kinds.contains(k)

  /** Число пассивки с учётом понимания; шансы не выше 100. */
  private def scaled(k: PassiveKind, base: Long): Long = (base * strength.getOrElse(k, 1.0)).toLong
  private def scaledPct(k: PassiveKind, base: Long): Long = scaled(k, base).min(100L)

  // ── Подземелье (шанс встречи боевого события) ───────────────────────────────
  /** Множитель к весу боевого события: Охотник ×1.2, Скрытность ×0.8. Обе на шлеме,
   *  поэтому одновременно активна максимум одна. */
  def battleEncounterFactor: Double = {
    var f = 1.0
    if (has(PassiveKind.Hunter)) f *= 1.0 + scaled(PassiveKind.Hunter, PassiveKind.Hunter.EncounterBonusPct) / 100.0
    if (has(PassiveKind.Stealthy)) f *= 1.0 - scaledPct(PassiveKind.Stealthy, PassiveKind.Stealthy.EncounterReductionPct) / 100.0
    f
  }

  // ── Уклонение / попадание ────────────────────────────────────────────────────
  /** Прибавка (в п.п.) к итоговому шансу уклонения героя: Быстрые ноги +5. */
  def dodgeBonusPct: Long = {
    var b = 0L
    if (has(PassiveKind.QuickFeet)) b += scaled(PassiveKind.QuickFeet, PassiveKind.QuickFeet.DodgeBonusPct)
    b
  }

  /** Дополнительная прибавка (в п.п.) к шансу уклона ТОЛЬКО при бегстве
   *  (Сверкающий +25). Применяется поверх [[dodgeBonusPct]]. */
  def fleeDodgeBonusPct: Long =
    if (has(PassiveKind.Glittering)) scaled(PassiveKind.Glittering, PassiveKind.Glittering.FleeDodgeBonusPct) else 0L

  /** Множитель точности МОБА, атакующего героя: Сливающийся ×0.9 (−10%). Снижает
   *  саму точность атакующего в формуле уклонения ([[BattleState.dodgeChance]]),
   *  а не добавляет уклонение герою напрямую. */
  def enemyAccuracyMult: Double =
    if (has(PassiveKind.Blending)) 1.0 - scaledPct(PassiveKind.Blending, PassiveKind.Blending.EnemyAccuracyReductionPct) / 100.0 else 1.0

  // ── Плоские бафы статов (не мультипликативные — как зелья пояса) ─────────────
  /** Плоская прибавка к защите: +5% от переданного значения защиты (Укреплённый). */
  def defenceFlatBonus(defence: Long): Long =
    if (has(PassiveKind.Reinforced)) (defence * scaled(PassiveKind.Reinforced, PassiveKind.Reinforced.DefenceBonusPct) / 100L).max(1L) else 0L

  /** Плоская прибавка к точности: +5% от переданного значения точности (Точность). */
  def accuracyFlatBonus(accuracy: Long): Long =
    if (has(PassiveKind.Precise)) (accuracy * scaled(PassiveKind.Precise, PassiveKind.Precise.AccuracyBonusPct) / 100L).max(1L) else 0L

  // ── Урон и лечение ───────────────────────────────────────────────────────────
  /** Множитель финального урона игрока: Разбойник ×1.05. */
  def finalDamageMult: Double =
    if (has(PassiveKind.Robber)) 1.0 + scaled(PassiveKind.Robber, PassiveKind.Robber.DamageBonusPct) / 100.0 else 1.0

  /** Множитель активного лечения в бою (скилл/фляга/зелье пояса): Целитель ×1.10. */
  def healMult: Double =
    if (has(PassiveKind.Healer)) 1.0 + scaled(PassiveKind.Healer, PassiveKind.Healer.HealBonusPct) / 100.0 else 1.0

  /** Множитель восстановления энергии в бою: Сосредоточенность ×1.10. */
  def energyRegenMult: Double =
    if (has(PassiveKind.Focused)) 1.0 + scaled(PassiveKind.Focused, PassiveKind.Focused.EnergyRegenBonusPct) / 100.0 else 1.0

  // ── Реген каждый ход ─────────────────────────────────────────────────────────
  /** % макс. HP, восстанавливаемый каждый раунд перед атакой (Целебный). */
  def hpRegenPct: Long = if (has(PassiveKind.Healing)) scaledPct(PassiveKind.Healing, PassiveKind.Healing.HpRegenPct) else 0L

  /** % макс. брони, восстанавливаемый каждый ход перед атакой (Самовосстанавливающийся). */
  def armorRegenPct: Long =
    if (has(PassiveKind.SelfRepairing)) scaledPct(PassiveKind.SelfRepairing, PassiveKind.SelfRepairing.ArmorRegenPct) else 0L

  // ── Пороговые срабатывания в бою ─────────────────────────────────────────────
  def hasImpenetrable: Boolean = has(PassiveKind.Impenetrable)
  def hasToughness: Boolean    = has(PassiveKind.Toughness)
  def hasSpiky: Boolean        = has(PassiveKind.Spiky)
  def hasQuickHands: Boolean   = has(PassiveKind.QuickHands)

  /** Непробиваемый: шанс как есть, срез урона растёт с пониманием (не выше 100 %). */
  def impenetrableReductionPct: Long = scaledPct(PassiveKind.Impenetrable, PassiveKind.Impenetrable.ReductionPct)

  /** Крепкость: шанс как есть, доля восстановленной брони растёт. */
  def toughnessRestorePct: Long = scaledPct(PassiveKind.Toughness, PassiveKind.Toughness.RestorePct)

  /** Шипастый: доля возвращённого урона. */
  def thornsPct: Long = scaled(PassiveKind.Spiky, PassiveKind.Spiky.ThornsPct)

  /** Быстрые руки: шанс повторить расходник — единственное число пассивки. */
  def quickHandsChancePct: Long = scaledPct(PassiveKind.QuickHands, PassiveKind.QuickHands.RepeatChancePct)

  // ── Лут ──────────────────────────────────────────────────────────────────────
  def hasTaxidermist: Boolean = has(PassiveKind.Taxidermist)
  def hasJeweler: Boolean     = has(PassiveKind.Jeweler)
  def hasMarauder: Boolean    = has(PassiveKind.Marauder)

  /** Шанс доп. трофея (0 — пассивки нет) и доп. серебра: их единственное число. */
  def taxidermistChancePct: Long = if (hasTaxidermist) scaledPct(PassiveKind.Taxidermist, PassiveKind.Taxidermist.TrophyChancePct) else 0L
  def jewelerChancePct: Long     = if (hasJeweler) scaledPct(PassiveKind.Jeweler, PassiveKind.Jeweler.SilverChancePct) else 0L

  // ── Инвентарь ────────────────────────────────────────────────────────────────
  /** Дополнительные слоты сумки от Тайника (0, если пассивки нет). */
  def extraInventorySlots: Long = if (has(PassiveKind.Stash)) PassiveKind.Stash.ExtraSlots else 0L
}

object HeroPassives {
  val empty: HeroPassives = HeroPassives(Set.empty)
}
