package pangea.model.monster

import pangea.model.stats.{BaseStats, FightStats}

case class Monster(
  id: Long,
  lvl: Long,
  race: Race,
  rarity: Rarity,
  baseStats: BaseStats,
  fightStats: FightStats
)
