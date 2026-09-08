package pangea.test

import pangea.model.hero.{Equipment, Hero, HeroId, MasterHornBoosts}
import pangea.model.item.{Item, ItemSet, ItemType, Rarity}
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

  /** Пустой предмет набора: только принадлежность, без единого стата — чтобы
   *  тест мерил бонусы набора, а не броню надетых железок. */
  private def setPiece(id: Long, itemType: ItemType, set: ItemSet): Item =
    Item(id, "Предмет", 1L, Rarity.Blue, itemType,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, set = Some(set))

  /** Экипировка, где первые `n` сетовых слотов заняты предметами набора `set`
   *  (порядок — как в `Equipment.setSlots`). */
  def wearingSet(set: ItemSet, n: Int): Equipment = {
    val slots = List(
      ItemType.Helmet, ItemType.ShoulderPads, ItemType.ChestPlate, ItemType.Bracelets,
      ItemType.Gloves, ItemType.Pants, ItemType.Boots, ItemType.Amulet,
      ItemType.Ring, ItemType.Ring, ItemType.Belt, ItemType.Weapon)
    slots.take(n).zipWithIndex.foldLeft(emptyEquipment) { case (acc, (t, i)) =>
      val it = setPiece(i.toLong, t, set)
      i match {
        case 0  => acc.copy(helmet = it)
        case 1  => acc.copy(shoulderPads = it)
        case 2  => acc.copy(chestPlate = it)
        case 3  => acc.copy(bracelets = it)
        case 4  => acc.copy(gloves = it)
        case 5  => acc.copy(pants = it)
        case 6  => acc.copy(boots = it)
        case 7  => acc.copy(amulet = it)
        case 8  => acc.copy(firstRing = it)
        case 9  => acc.copy(secondRing = it)
        case 10 => acc.copy(belt = it)
        case _  => acc.copy(weapon = it)
      }
    }
  }

  def hero(userId: UserId, state: StateType = StateType.Dungeon, dungeonLevel: Int = 1, maxDungeonLevel: Int = 150): Hero = Hero(
    id           = HeroId(1L),
    userId       = userId,
    state        = state,
    lvl          = 1L,
    exp          = 0L,
    upgradePoints = 0L,
    race         = Race.Human,
    baseStats    = BaseStats(agi = 10, vit = 10, str = 10, int = 10),
    fightStats   = FightStats(atk = 10, hp = 100, armor = 0, defence = 0, evasion = 5, accuracy = 10, energy = 5),
    equipment    = emptyEquipment,
    dungeonLevel    = dungeonLevel,
    maxDungeonLevel = maxDungeonLevel,
    silver       = 0L,
    traumaUntil  = None,
    traumaNames  = Nil,
    guildReputation = 0L,
    masterHornBoosts = MasterHornBoosts.empty,
    doubloons    = 0L,
    statBoosts   = pangea.model.stats.StatBoosts.none
  )
}
