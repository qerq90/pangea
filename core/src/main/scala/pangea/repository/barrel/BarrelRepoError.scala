package pangea.repository.barrel

sealed trait BarrelRepoError

object BarrelRepoError {
  case object BarrelFull           extends BarrelRepoError
  /** Горстей этой пыли в бочке уже сотня: места она не занимает, но и сверх
    * предела не ложится (см. `MaterialKind.MaxDustPerKind`). */
  case object DustLimitReached     extends BarrelRepoError
  case object SilverOverflow       extends BarrelRepoError
  case object NotEnoughSilver      extends BarrelRepoError
  case object NonPositiveAmount    extends BarrelRepoError
  case object CantFindItemToRemove extends BarrelRepoError
  case object CantUpdateBarrel     extends BarrelRepoError
  case object CantFindBarrel       extends BarrelRepoError
}
