package pangea.model.hero

import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.user.UserId

case class Hero(
  id: HeroId,
  userId: UserId,
  state: StateType,
  race: Race,
  baseStats: BaseStats,
  fightStats: FightStats,
  equipment: Equipment
)
