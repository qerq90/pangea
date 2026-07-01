package pangea.test

import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory
import pangea.model.item.Item
import pangea.repository.inventory.{InventoryRepoError, InventoryRepository}
import zio.{IO, ZIO}

class TestInventoryRepository(canAdd: Boolean, private var items: List[Item] = Nil)
    extends InventoryRepository {

  private var capacity: Long = 20L

  def get(heroId: HeroId): IO[InventoryRepoError, Inventory] =
    ZIO.succeed(Inventory(1L, heroId, capacity, Inventory.Items(items)))

  def increaseCapacity(heroId: HeroId, delta: Long): IO[InventoryRepoError, Unit] =
    ZIO.succeed { capacity += delta }

  def addItem(heroId: HeroId, item: Item): IO[InventoryRepoError, Unit] =
    if (canAdd) ZIO.succeed { items = items :+ item }
    else ZIO.fail(InventoryRepoError.NoMorePlaceForItems)

  def removeItem(itemId: Long, heroId: HeroId): IO[InventoryRepoError, Unit] =
    ZIO.succeed { items = items.filterNot(_.id == itemId) }

  def removeItems(itemIds: Set[Long], heroId: HeroId): IO[InventoryRepoError, Unit] =
    ZIO.succeed { items = items.filterNot(i => itemIds.contains(i.id)) }

  def refillFlasks(heroId: HeroId): IO[InventoryRepoError, Unit] =
    ZIO.succeed {
      items = items.map(item =>
        if (item.itemType == pangea.model.item.ItemType.Flask && item.maxCharges.isDefined)
          item.copy(charges = item.maxCharges)
        else item)
    }

  def snapshot: List[Item] = items
}

object TestInventoryRepository {
  def accepting: TestInventoryRepository = new TestInventoryRepository(canAdd = true)
  def full: TestInventoryRepository      = new TestInventoryRepository(canAdd = false)
  def withItems(items: List[Item]): TestInventoryRepository =
    new TestInventoryRepository(canAdd = true, items = items)
}
