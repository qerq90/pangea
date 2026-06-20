package pangea.model.hero

import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.trauma.{Trauma, TraumaPenalties}
import pangea.model.user.UserId

case class Hero(
  id: HeroId,
  userId: UserId,
  state: StateType,
  lvl: Long,
  exp: Long,
  upgradePoints: Long,
  race: Race,
  baseStats: BaseStats,
  fightStats: FightStats,
  equipment: Equipment,
  dungeonLevel: Int,
  gold: Long,
  traumaUntil: Option[Long],
  traumaNames: List[String]
) {
  def maxArmor: Long = equipment.allArmor * fightStats.defence.max(1L)

  def traumaActive(nowMs: Long): Boolean = traumaUntil.exists(_ > nowMs)

  def activeTraumas(nowMs: Long): List[Trauma] =
    if (!traumaActive(nowMs)) Nil
    else traumaNames.flatMap(Trauma.byName)

  def combinedPenalties(nowMs: Long): TraumaPenalties =
    activeTraumas(nowMs).foldLeft(TraumaPenalties.none)(_ + _.penalties)

  /** Боевые статы с заданными штрафами. Атака/защита/точность/уклонение зажаты
   *  снизу единицей (не могут упасть ниже 1); броня может быть 0 (без экипировки). */
  private def fightStatsWith(p: TraumaPenalties): FightStats = {
    val rf = race.factor
    fightStats.copy(
      atk           = (fightStats.atk * rf.attackFactor * (1.0 - p.atkPct)).toLong.max(1L),
      defence       = (fightStats.defence * rf.defenceFactor * (1.0 - p.defPct)).toLong.max(1L),
      accuracy      = (fightStats.accuracy * rf.accuracyFactor * (1.0 - p.accPct)).toLong.max(1L),
      evasion       = (fightStats.evasion * rf.evasionFactor * (1.0 - p.evasionPct)).toLong.max(1L),
      concentration = ((fightStats.concentration + baseStats.int * (1.0 - p.intPct)) * rf.concentrationFactor * (1.0 - p.concPct)).toLong.max(1L),
      armor         = (fightStats.armor * (1.0 - p.armorPct)).toLong.max(0L)
    )
  }

  /** Текущие боевые статы — с учётом активных травм. */
  def effectiveFightStats(nowMs: Long): FightStats = fightStatsWith(combinedPenalties(nowMs))

  /** Боевые статы без травм — «потолок», к которому стат вернётся после снятия травм. */
  def maxFightStats: FightStats = fightStatsWith(TraumaPenalties.none)

  /** Базовые характеристики с учётом травм (зажаты снизу единицей). Ловкость не
   *  имеет штрафа от травм. */
  def effectiveBaseStats(nowMs: Long): BaseStats = {
    val p = combinedPenalties(nowMs)
    BaseStats(
      agi = baseStats.agi,
      vit = (baseStats.vit * (1.0 - p.vitPct)).toLong.max(1L),
      str = (baseStats.str * (1.0 - p.strPct)).toLong.max(1L),
      int = (baseStats.int * (1.0 - p.intPct)).toLong.max(1L)
    )
  }

  def effectiveMaxHp(nowMs: Long): Long = {
    val p    = combinedPenalties(nowMs)
    val base = baseStats.vit * 16L
    (base * race.factor.hpFactor * (1.0 - p.vitPct) * (1.0 - p.hpPct)).toLong.max(1L)
  }

  def traumaRemainingText(nowMs: Long): Option[String] =
    traumaUntil.filter(_ > nowMs).map { until =>
      val secs    = (until - nowMs) / 1000L
      val hours   = secs / 3600
      val minutes = (secs % 3600) / 60
      s"${hours}ч ${minutes}мин"
    }

  // «текущее/максимум», если травма уронила стат ниже потолка; иначе просто число
  private def withMax(cur: Long, max: Long): String =
    if (cur < max) s"$cur/$max" else cur.toString

  def getInfo(nowMs: Long): String = {
    val effB  = effectiveBaseStats(nowMs)
    val eff   = effectiveFightStats(nowMs)
    val maxFS = maxFightStats
    val maxHp = effectiveMaxHp(nowMs)
    s"""${race.toString}, Уровень $lvl
       | $getLvlExp/$getNeededExp опыта
       |
       | 💪 СИЛ ${withMax(effB.str, baseStats.str)}  ТЕЛО ${withMax(effB.vit, baseStats.vit)}
       | 🏃 ЛОВ ${withMax(effB.agi, baseStats.agi)}  ИНТ ${withMax(effB.int, baseStats.int)}
       |
       | ⚔ Атк ${withMax(eff.atk, maxFS.atk)}  🎯 Точн ${withMax(eff.accuracy, maxFS.accuracy)}
       | 🛡 Защ ${withMax(eff.defence, maxFS.defence)}  👁 Укл ${withMax(eff.evasion, maxFS.evasion)}
       | ❤ ${fightStats.hp}/$maxHp
       |
       | Свободных очков: $upgradePoints
       |""".stripMargin
  }

  def getNeededExp: Long = lvl * 100L
  def getLvlExp: Long    = exp
}
