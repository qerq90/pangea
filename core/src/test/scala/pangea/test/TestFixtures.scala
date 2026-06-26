package pangea.test

import pangea.model.hero.{Equipment, Hero, HeroId}
import pangea.model.item.Item
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.user.UserId

object TestFixtures {
  val noItem: Item = Item.NoItem
  val emptyEquipment: Equipment = Equipment(
    noItem, noItem, noItem, noItem, noItem, noItem, noItem,
    noItem, noItem, noItem, noItem, noItem, noItem, noItem
  )

  def hero(userId: UserId, state: StateType = StateType.Dungeon, dungeonLevel: Int = 1, maxDungeonLevel: Int = 150): Hero = Hero(
    id           = HeroId(1L),
    userId       = userId,
    state        = state,
    lvl          = 1L,
    exp          = 0L,
    upgradePoints = 0L,
    race         = Race.Human,
    baseStats    = BaseStats(agi = 10, vit = 10, str = 10, int = 10),
    fightStats   = FightStats(atk = 10, hp = 100, armor = 0, defence = 0, evasion = 5, accuracy = 10, concentration = 5),
    equipment    = emptyEquipment,
    dungeonLevel    = dungeonLevel,
    maxDungeonLevel = maxDungeonLevel,
    gold         = 0L,
    traumaUntil  = None,
    traumaNames  = Nil,
    guildReputation = 0L
  )
}
