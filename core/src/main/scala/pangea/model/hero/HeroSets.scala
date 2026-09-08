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

  /** % нанесённого по HP урона, возвращаемого в лечение (порог 4) — как и
   *  вампиризм черепа: удар, поглощённый бронёй, крови не даёт. */
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

  // ── «Каменный страж» ────────────────────────────────────────────────────────

  /** Множитель ЛЮБОГО стихийного урона по герою (порог 4). Стихийным считается
   *  весь урон элементаля: он и есть чистая стихия — и удар, и всплеск, и смерч,
   *  и шипы, и горение. */
  def elementalDamageTakenMult: Double =
    if (has(ItemSet.StoneGuard, 4)) (100L - ItemSet.StoneGuard.ElementalTakenCutPct) / 100.0 else 1.0

  /** Множитель урона, снимаемого с БРОНИ героя (порог 6): броня тает медленнее.
   *  Урон по HP при этом не меняется — режется именно расход брони. */
  def armorDamageTakenMult: Double =
    if (has(ItemSet.StoneGuard, 6)) (100L - ItemSet.StoneGuard.ArmorDamageCutPct) / 100.0 else 1.0

  /** Сколько брони реально уйдёт, когда она поглотила `absorbed` урона. Броня
   *  прикрывает HP на всю поглощённую величину — порог 6 удешевляет только её
   *  собственный расход. */
  def armorSpent(absorbed: Long): Long =
    if (absorbed <= 0L) absorbed else (absorbed * armorDamageTakenMult).toLong.max(1L)

  /** На сколько п.п. снижен шанс поджечь героя (порог 10). */
  def igniteResistPct: Long =
    if (has(ItemSet.StoneGuard, 10)) ItemSet.StoneGuard.IgniteResistPct else 0L

  /** Спасает ли броня на низком HP (порог 12) и её ставки. */
  def rescuesOnLowHp: Boolean  = has(ItemSet.StoneGuard, 12)
  def lowHpThresholdPct: Long  = ItemSet.StoneGuard.LowHpThresholdPct
  def rescueArmorPct: Long     = ItemSet.StoneGuard.ArmorRestorePct

  // ── «Дикое пламя» ───────────────────────────────────────────────────────────

  /** Прибавка к граням урона огнём, в долях (порог 4). Применяется только если в
   *  оружии есть Огонь — см. `BattleState.splitElementalDamage`. */
  def fireDamageBonus: Double =
    if (has(ItemSet.WildFlame, 4)) ItemSet.WildFlame.FireDamageBonusPct / 100.0 else 0.0

  /** Во сколько раз быстрее растёт горение за раунд (порог 6). */
  def burnGrowthMult: Long =
    if (has(ItemSet.WildFlame, 6)) ItemSet.WildFlame.BurnGrowthMult else 1L

  /** Прибавка к шансу прока Огня, в п.п. (порог 10). */
  def igniteChanceBonusPct: Long =
    if (has(ItemSet.WildFlame, 10)) ItemSet.WildFlame.IgniteChanceBonusPct else 0L

  /** Поджигают ли активные умения врага всегда, и режет ли горение его защиту
   *  на столько же процентов, сколько горит (порог 12). */
  def skillsAlwaysIgnite: Boolean = has(ItemSet.WildFlame, 12)

  // ── «Охотник» ───────────────────────────────────────────────────────────────

  /** Шанс (в %) повторить атаку после промаха, не чаще раза за раунд (порог 6). */
  def repeatOnMissChancePct: Long =
    if (has(ItemSet.Hunter, 6)) ItemSet.Hunter.RepeatChancePct else 0L

  /** Отменяется ли первая за бой вражеская способность, наносящая урон (порог 10). */
  def cancelsFirstEnemySkill: Boolean = has(ItemSet.Hunter, 10)

  /** Удваивает ли первая за бой способность героя свой урон (порог 12). */
  def doublesFirstSkill: Boolean = has(ItemSet.Hunter, 12)
}

object HeroSets {
  val empty: HeroSets = HeroSets(Map.empty)
}
