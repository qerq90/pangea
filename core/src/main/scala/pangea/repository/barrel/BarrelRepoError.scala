package pangea.repository.barrel

sealed trait BarrelRepoError

object BarrelRepoError {
  case object BarrelFull           extends BarrelRepoError
  case object GoldOverflow         extends BarrelRepoError
  case object NotEnoughGold        extends BarrelRepoError
  case object NonPositiveAmount    extends BarrelRepoError
  case object CantFindItemToRemove extends BarrelRepoError
  case object CantUpdateBarrel     extends BarrelRepoError
  case object CantFindBarrel       extends BarrelRepoError
}
