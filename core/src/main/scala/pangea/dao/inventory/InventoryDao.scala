package pangea.dao.inventory

import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory
import zio.Task

trait InventoryDao {
  def create(heroId: HeroId): Task[Unit]
  def get(heroId: HeroId): Task[Inventory]
  def update(inventory: Inventory): Task[Unit]
}
