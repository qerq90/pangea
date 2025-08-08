package pangea.dao.inventory

import doobie.util.transactor
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory
import zio.{Task, ZLayer}

trait InventoryDao {
  def create(heroId: HeroId): Task[Unit]
  def get(heroId: HeroId): Task[Inventory]
  def update(inventory: Inventory): Task[Unit]
}

object InventoryDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, InventoryDao] =
    ZLayer.fromFunction(new InventoryDaoLive(_))
}
