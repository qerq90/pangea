package pangea.model.hero

import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.trauma.Trauma
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
  traumaName: Option[String]
) {
  // Effective armor pool = Armor × Defence (min 1 so base equipment armor always counts)
  def maxArmor: Long = equipment.allArmor * fightStats.defence.max(1L)

  def traumaActive(nowMs: Long): Boolean = traumaUntil.exists(_ > nowMs)

  def effectiveFightStats(nowMs: Long): FightStats = {
    val rf        = race.factor
    val traumaMod = if (traumaActive(nowMs)) traumaModifier else 1.0
    fightStats.copy(
      atk           = (fightStats.atk * rf.attackFactor * traumaMod).toLong.max(1L),
      defence       = (fightStats.defence * rf.defenceFactor * traumaMod).toLong,
      accuracy      = (fightStats.accuracy * rf.accuracyFactor * traumaMod).toLong,
      evasion       = (fightStats.evasion * rf.evasionFactor * traumaMod).toLong,
      concentration = ((fightStats.concentration + baseStats.int) * rf.concentrationFactor * traumaMod).toLong
    )
  }

  def effectiveMaxHp(nowMs: Long): Long = {
    val base     = baseStats.vit * 16L
    val withRace = (base * race.factor.hpFactor).toLong.max(1L)
    if (traumaActive(nowMs)) (withRace * traumaModifier).toLong.max(1L) else withRace
  }

  // Stat multiplier from active trauma (1.0 if no known trauma → fallback -20%)
  private def traumaModifier: Double =
    traumaName.flatMap(Trauma.byName).map(_.modifier).getOrElse(0.8)

  // Remaining trauma in "Xh Ym" format, or None
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

