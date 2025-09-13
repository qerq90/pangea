package pangea.model.hero

import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.stats.{BaseStats, FightStats}
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
  equipment: Equipment
) {
  def getInfo: String =
    s"""Уровень $lvl
       | $getLvlExp/$getNeededExp очков опыта
       |
       | 👊 - ${baseStats.str} очков силы
       | 💪 - ${baseStats.vit} очков телосложения
       | 👣 - ${baseStats.agi} - очков ловкости
       | ❤ ${fightStats.hp}/64 🧥 ${fightStats.armor}/100
       |
       | Свободных очков характеристик: ${upgradePoints}
       |""".stripMargin

  def getNeededExp: Long = {
    var neededExp = 100
    var heroExp   = exp
    while (heroExp > neededExp) {
      heroExp -= neededExp
      neededExp += 100
    }

    neededExp
  }

  def getLvlExp: Long = {
    var neededExp = 100
    var heroExp   = exp
    while (heroExp > neededExp) {
      heroExp -= neededExp
      neededExp += 100
    }

    heroExp
  }
}
