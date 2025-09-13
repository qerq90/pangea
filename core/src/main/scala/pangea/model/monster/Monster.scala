package pangea.model.monster

import pangea.model.stats.FightStats

case class Monster(
  id: Long,
  lvl: Long,
  race: Race,
  rarity: Rarity,
  fightStats: FightStats
)
