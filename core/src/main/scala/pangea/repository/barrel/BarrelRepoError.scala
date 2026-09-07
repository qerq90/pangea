package pangea.repository.barrel

sealed trait BarrelRepoError

object BarrelRepoError {
  case object BarrelFull           extends BarrelRepoError
  case object SilverOverflow       extends BarrelRepoError
  case object NotEnoughSilver      extends BarrelRepoError
  case object NonPositiveAmount    extends BarrelRepoError
  case object CantFindItemToRemove extends BarrelRepoError
  case object CantUpdateBarrel     extends BarrelRepoError
  case object CantFindBarrel       extends BarrelRepoError
}
