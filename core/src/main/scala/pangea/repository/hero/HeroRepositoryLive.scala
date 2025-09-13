package pangea.repository.hero

import pangea.dao.hero.HeroDao
import pangea.dao.inventory.InventoryDao
import pangea.model.hero.{Equipment, Hero, HeroId}
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
  private def newHero(id: UserId): Hero =
    Hero(
      id = HeroId(-1),
      userId = id,
      state = Registration,
      lvl = 1,
      race = Human,
      exp = 0,
      upgradePoints = 0,
      baseStats = BaseStats(4, 4, 4, 4),
      fightStats = FightStats(4, 64, 0, 0),
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
      )
    )

}
