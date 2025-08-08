package pangea.repository.hero

import pangea.dao.hero.HeroDao
import pangea.dao.inventory.InventoryDao
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.UserId
import zio.{Task, ZLayer}

trait HeroRepository {
  def registerNewHero(userId: UserId): Task[Hero]
  def getHero(userId: UserId): Task[Option[Hero]]
  def updateState(userId: UserId, potentiallyNewState: StateType): Task[Unit]
}

object HeroRepository {
  val live: ZLayer[HeroDao with InventoryDao, Nothing, HeroRepository] =
    ZLayer.fromFunction(new HeroRepositoryLive(_, _))
}
