package pangea.repository.inventory

sealed trait InventoryRepoError

object InventoryRepoError {
  case object NoMorePlaceForItems  extends InventoryRepoError
  case object CantFindInventory    extends InventoryRepoError
  case object CantUpdateInventory  extends InventoryRepoError
  case object CantFindItemToRemove extends InventoryRepoError
}
