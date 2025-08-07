package pangea.repository.inventory

import pangea.dao.inventory.InventoryDao
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory
import pangea.model.item.Item
import zio.{IO, Task, ZIO}

final class InventoryRepositoryLive(inventoryDao: InventoryDao)
    extends InventoryRepository {
  def get(heroId: HeroId): IO[InventoryRepoError, Inventory] =
    inventoryDao
      .get(heroId)
      .orElseFail(InventoryRepoError.CantFindInventory)

  def addItem(heroId: HeroId, item: Item): IO[InventoryRepoError, Unit] =
    for {
      inventory <- inventoryDao
        .get(heroId)
        .orElseFail(InventoryRepoError.CantFindInventory)
      updatedInventory <-
        if (inventory.maxItems >= inventory.items.length)
          ZIO.fail(InventoryRepoError.NoMorePlaceForItems)
        else ZIO.succeed(inventory.addItem(item))
      _ <- inventoryDao
        .update(updatedInventory)
        .orElseFail(InventoryRepoError.CantUpdateInventory)
    } yield ()

  def removeItem(itemId: Long, heroId: HeroId): IO[InventoryRepoError, Unit] =
    for {
      inventory <- inventoryDao
        .get(heroId)
        .orElseFail(InventoryRepoError.CantFindInventory)
      itemsWithoutOne = inventory.items.filter(_.id != itemId)
      _ <- ZIO.when(itemsWithoutOne.length == inventory.items.length)(
        ZIO.fail(InventoryRepoError.CantFindItemToRemove)
      )
      _ <- inventoryDao
        .update(inventory.copy(items = itemsWithoutOne))
        .orElseFail(InventoryRepoError.CantUpdateInventory)
    } yield ()
}
