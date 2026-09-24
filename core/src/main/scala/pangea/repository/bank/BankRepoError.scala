package pangea.repository.bank

sealed trait BankRepoError

object BankRepoError {
  /** Ячеек не куплено — хранилища у героя ещё нет. */
  case object NoVault              extends BankRepoError
  case object VaultFull            extends BankRepoError
  /** Невесомого добра этого вида в хранилище уже сотня (см. `Item.HoardLimit`). */
  case object DustLimitReached     extends BankRepoError
  case object SilverOverflow       extends BankRepoError
  case object NotEnoughSilver      extends BankRepoError
  case object NonPositiveAmount    extends BankRepoError
  case object CantFindItemToRemove extends BankRepoError
  case object CantUpdateVault      extends BankRepoError
  case object CantFindVault        extends BankRepoError
}
