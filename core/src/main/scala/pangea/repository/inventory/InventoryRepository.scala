package pangea.repository.inventory

import pangea.dao.inventory.InventoryDao
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory
import pangea.model.item.Item
import zio.{IO, Task, ZLayer}

trait InventoryRepository {
  def get(heroId: HeroId): IO[InventoryRepoError, Inventory]
  def addItem(heroId: HeroId, item: Item): IO[InventoryRepoError, Unit]
  def removeItem(itemId: Long, heroId: HeroId): IO[InventoryRepoError, Unit]
  def removeItems(itemIds: Set[Long], heroId: HeroId): IO[InventoryRepoError, Unit]
  def refillFlasks(heroId: HeroId): IO[InventoryRepoError, Unit]
  def increaseCapacity(heroId: HeroId, delta: Long): IO[InventoryRepoError, Unit]
}

object InventoryRepository {
  val live: ZLayer[InventoryDao, Nothing, InventoryRepository] =
    ZLayer.fromFunction(new InventoryRepositoryLive(_))
}
