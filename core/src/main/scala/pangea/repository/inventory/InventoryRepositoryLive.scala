package pangea.repository.inventory

import pangea.dao.inventory.InventoryDao
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory
import pangea.model.item.Item
import zio.{IO, ZIO}

final class InventoryRepositoryLive(inventoryDao: InventoryDao)
    extends InventoryRepository {
  def get(heroId: HeroId): IO[InventoryRepoError, Inventory] =
    inventoryDao
      .get(heroId)
      .orElseFail(InventoryRepoError.CantFindInventory)

  def addItem(heroId: HeroId, item: Item): IO[InventoryRepoError, Unit] =
    (for {
      inventory <- inventoryDao
        .get(heroId)
        .tapError(err => ZIO.logError(s"Error occurred: ${err.getMessage}"))
        .orElseFail(InventoryRepoError.CantFindInventory)
      updatedInventory <-
        if (inventory.maxItems <= inventory.items.data.length)
          ZIO.fail(InventoryRepoError.NoMorePlaceForItems)
        else ZIO.succeed(inventory.addItem(item))
      _ <- inventoryDao
        .update(updatedInventory)
        .orElseFail(InventoryRepoError.CantUpdateInventory)
    } yield ())
      .tapError(err => ZIO.logError(s"Error occurred: $err"))

  def refillFlasks(heroId: HeroId): IO[InventoryRepoError, Unit] =
    for {
      inventory <- inventoryDao.get(heroId).orElseFail(InventoryRepoError.CantFindInventory)
      refilled   = inventory.items.data.map(item =>
                     if (item.itemType == pangea.model.item.ItemType.Flask && item.maxCharges.isDefined)
                       item.copy(charges = item.maxCharges)
                     else item)
      _         <- inventoryDao.update(inventory.withItems(refilled)).orElseFail(InventoryRepoError.CantUpdateInventory)
    } yield ()

  def removeItem(itemId: Long, heroId: HeroId): IO[InventoryRepoError, Unit] =
    for {
      inventory <- inventoryDao
        .get(heroId)
        .orElseFail(InventoryRepoError.CantFindInventory)
      itemsWithoutOne = inventory.items.data.filter(_.id != itemId)
      _ <- ZIO.when(itemsWithoutOne.length == inventory.items.data.length)(
        ZIO.fail(InventoryRepoError.CantFindItemToRemove)
      )
      _ <- inventoryDao
        .update(inventory.withItems(itemsWithoutOne))
        .orElseFail(InventoryRepoError.CantUpdateInventory)
    } yield ()
}
