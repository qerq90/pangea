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

  def effectiveFightStats(nowMs: Long): FightStats = {
    val rf = race.factor
    val p  = combinedPenalties(nowMs)
    fightStats.copy(
      atk           = (fightStats.atk * rf.attackFactor * (1.0 - p.atkPct)).toLong.max(1L),
      defence       = (fightStats.defence * rf.defenceFactor * (1.0 - p.defPct)).toLong,
      accuracy      = (fightStats.accuracy * rf.accuracyFactor * (1.0 - p.accPct)).toLong,
      evasion       = (fightStats.evasion * rf.evasionFactor * (1.0 - p.evasionPct)).toLong,
      concentration = ((fightStats.concentration + baseStats.int * (1.0 - p.intPct)) * rf.concentrationFactor * (1.0 - p.concPct)).toLong,
      armor         = (fightStats.armor * (1.0 - p.armorPct)).toLong.max(0L)
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

  def getInfo(nowMs: Long): String = {
    val eff   = effectiveFightStats(nowMs)
    val maxHp = effectiveMaxHp(nowMs)
    s"""${race.toString}, Уровень $lvl
       | $getLvlExp/$getNeededExp опыта
       |
       | 💪 СИЛ ${baseStats.str}  ТЕЛО ${baseStats.vit}
       | 🏃 ЛОВ ${baseStats.agi}  ИНТ ${baseStats.int}
       |
       | ⚔ Атк ${eff.atk}  🎯 Точн ${eff.accuracy}
       | 🛡 Защ ${eff.defence}  👁 Укл ${eff.evasion}
       | ❤ ${fightStats.hp}/$maxHp
       |
       | Свободных очков: $upgradePoints
       |""".stripMargin
  }

  def getNeededExp: Long = lvl * 100L
  def getLvlExp: Long    = exp
}
