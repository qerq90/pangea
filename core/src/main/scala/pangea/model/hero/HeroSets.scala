package pangea.model.hero

import pangea.model.item.{ItemSet, SetBonus}

/** Типизированный фасад над наборами надетого снаряжения. Как [[HeroPassives]] и
 *  [[HeroGems]], каждая точка применения спрашивает готовый модификатор, а числа
 *  живут на вариантах [[ItemSet]].
 *
 *  Наборы независимы: бонусы каждого открываются по своему счётчику надетых
 *  предметов, поэтому 12 предметов из шести разных наборов дают шесть двойных
 *  бонусов. Снял предмет — счётчик упал, порог закрылся.
 *
 *  `counts` приходит из `Equipment.setCounts` и уже учитывает только 12 сетовых
 *  слотов (без фляги и доп. оружия). */
final case class HeroSets(counts: Map[ItemSet, Int]) {

  /** Сколько предметов набора надето. */
  def pieces(set: ItemSet): Int = counts.getOrElse(set, 0)

  /** Открытые бонусы набора — все пороги, до которых добрал игрок. */
  def bonuses(set: ItemSet): List[SetBonus] =
    set.bonuses.filter(_.pieces <= pieces(set))

  /** Все открытые бонусы по всем наборам, в порядке объявления наборов. */
  def activeBonuses: List[(ItemSet, List[SetBonus])] =
    ItemSet.values.toList.map(s => s -> bonuses(s)).filter(_._2.nonEmpty)

  /** Открыт ли конкретный порог набора и действует ли он (у неактивных бонусов
   *  механики пока нет — см. [[SetBonus.active]]). */
  def has(set: ItemSet, pieces: Int): Boolean =
    this.pieces(set) >= pieces && set.bonusAt(pieces).exists(_.active)

  // ── Готовые модификаторы ────────────────────────────────────────────────────

  /** +% к защите от «Каменного стража» (порог 2). */
  def defenceBonusPct: Long = if (has(ItemSet.StoneGuard, 2)) ItemSet.StoneGuard.DefencePct else 0L

  /** +% к атаке от «Дикого пламени» (порог 2). */
  def attackBonusPct: Long = if (has(ItemSet.WildFlame, 2)) ItemSet.WildFlame.AttackPct else 0L

  /** +% к уклонению от «Упыря» (порог 2). */
  def evasionBonusPct: Long = if (has(ItemSet.Ghoul, 2)) ItemSet.Ghoul.EvasionPct else 0L

  /** +% к точности от «Охотника» (порог 2). */
  def accuracyBonusPct: Long = if (has(ItemSet.Hunter, 2)) ItemSet.Hunter.AccuracyPct else 0L

  /** Плоская прибавка к макс. HP: +300 за каждый набор, добравший до порога 8. */
  def flatHp: Long = ItemSet.values.count(has(_, 8)) * ItemSet.HpFlatBonus

  /** +% к макс. HP: +10% за каждый набор, добравший до порога 8. */
  def maxHpBonusPct: Long = ItemSet.values.count(has(_, 8)) * ItemSet.HpPctBonus

  /** +% к макс. энергии от «Охотника» (порог 4). */
  def energyBonusPct: Long = if (has(ItemSet.Hunter, 4)) ItemSet.Hunter.EnergyPct else 0L

  /** Множитель вклада ловкости в реген энергии за раунд («Охотник», порог 4). */
  def agiEnergyRegenMult: Long =
    if (has(ItemSet.Hunter, 4)) ItemSet.Hunter.AgiRegenMult else 1L

  // ── «Упырь» ─────────────────────────────────────────────────────────────────

  /** % ВСЕГО нанесённого урона (по броне и по HP), возвращаемого в лечение
   *  (порог 4). В отличие от вампиризма черепа, который считает только урон по
   *  HP: у набора в тексте «от нанесённого урона», без оговорки про HP. */
  def lifestealPct: Long = if (has(ItemSet.Ghoul, 4)) ItemSet.Ghoul.LifestealPct else 0L

  /** Шанс (в %), что обычная атака, прошедшая в HP, вызовет кровотечение (порог 6). */
  def bleedOnHitChancePct: Long = if (has(ItemSet.Ghoul, 6)) ItemSet.Ghoul.BleedChancePct else 0L

  /** Сила накладываемого набором кровотечения, в % макс. HP цели. */
  def bleedPct: Int = ItemSet.Ghoul.BleedPct

  /** Лечит ли героя урон от кровотечения врага (порог 10). */
  def healsFromBleed: Boolean = has(ItemSet.Ghoul, 10)

  /** Всегда ли активные умения накладывают кровотечение (порог 12). */
  def skillsAlwaysBleed: Boolean = has(ItemSet.Ghoul, 12)

  /** Восстановление при убийстве врага — «жуткий пир» (порог 12). */
  def feastsOnKill: Boolean = has(ItemSet.Ghoul, 12)
  def feastHpPct: Long      = ItemSet.Ghoul.KillHpRestorePct
  def feastArmorPct: Long   = ItemSet.Ghoul.KillArmorRestorePct
}

object HeroSets {
  val empty: HeroSets = HeroSets(Map.empty)
}
