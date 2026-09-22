package pangea.repository.inventory

sealed trait InventoryRepoError

object InventoryRepoError {
  case object NoMorePlaceForItems  extends InventoryRepoError
  /** Горстей этой пыли уже [[pangea.model.item.MaterialKind.MaxDustPerKind]] —
    * места она не занимает, но и сверх предела не ложится. */
  case object DustLimitReached     extends InventoryRepoError
  case object CantFindInventory    extends InventoryRepoError
  case object CantUpdateInventory  extends InventoryRepoError
  case object CantFindItemToRemove extends InventoryRepoError
}
