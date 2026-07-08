package pangea.repository.hero

import pangea.dao.hero.HeroDao
import pangea.dao.inventory.InventoryDao
import pangea.model.hero.{Equipment, Hero, HeroId, MasterHornBoosts}
import pangea.model.item.Item.NoItem
import pangea.model.monster.Race.Human
import pangea.model.state.StateType
import pangea.model.state.StateType.Registration
import pangea.model.stats.{BaseStats, FightStats}
import pangea.model.user.UserId
import pangea.repository.hero.HeroRepositoryLive.newHero
import zio.Task

case class HeroRepositoryLive(heroDao: HeroDao, inventoryDao: InventoryDao)
    extends HeroRepository {

  override def registerNewHero(userId: UserId): Task[Hero] = {
    val hero = newHero(userId)
    for {
      hero <- heroDao
        .insertHero(hero)
        .map(heroId => hero.copy(id = heroId))
      _ <- inventoryDao.create(hero.id)
    } yield hero
  }

  override def getHero(userId: UserId): Task[Option[Hero]] =
    heroDao.getHeroByUserId(userId)

  override def updateState(userId: UserId, newState: StateType): Task[Unit] =
    heroDao.updateState(userId, newState)
}

object HeroRepositoryLive {
  private def newHero(id: UserId): Hero = {
    val hero = Hero(
      id = HeroId(-1),
      userId = id,
      state = Registration,
      lvl = 1,
      race = Human,
      exp = 0,
      upgradePoints = 0,
      baseStats = BaseStats(4, 4, 4, 4),
      fightStats = FightStats(
        atk = 22,
        hp = 0, // выставляется ниже из effectiveMaxHp (единый источник — формула от тела)
        armor = 0,
        defence = 8,
        evasion = 22,
        accuracy = 20,
        energy = 0 // выставляется ниже из maxEnergy (единый источник — формула)
      ),
      equipment = Equipment(
        NoItem,
        NoItem,
        NoItem,
        NoItem,
        NoItem,
        NoItem,
        NoItem,
        NoItem,
        NoItem,
        NoItem,
        NoItem,
        NoItem,
        NoItem,
        NoItem
      ),
      dungeonLevel = 1,
      maxDungeonLevel = 1,
      gold = 0L,
      traumaUntil = None,
      traumaNames = Nil,
      guildReputation = 0L,
      masterHornBoosts = MasterHornBoosts.empty,
      doubloons = 0L,
      statBoosts = pangea.model.stats.StatBoosts.none
    )
    // Новичок без травм/бустов/экипировки → effectiveMaxHp = applyVit(vit) * 24,
    // maxEnergy = 5·int + 2·agi. Стартуем с полными HP и Энергией.
    hero.copy(fightStats = hero.fightStats.copy(
      hp     = hero.effectiveMaxHp(0L),
      energy = hero.maxEnergy(0L)))
  }

}
