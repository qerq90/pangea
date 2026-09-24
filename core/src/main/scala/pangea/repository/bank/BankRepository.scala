package pangea.repository.bank

import pangea.dao.bank.BankVaultDao
import pangea.model.bank.BankVault
import pangea.model.hero.HeroId
import pangea.model.item.Item
import zio.{IO, ZLayer}

/** Хранилище в Торговом доме: то же, что бочка, плюс покупка ячеек и трата
 *  серебра «после своего» (см. `Purse`). */
trait BankRepository {
  def get(heroId: HeroId): IO[BankRepoError, BankVault]
  /** Купить ещё одну ячейку (серебро списывает вызывающий). */
  def buyCell(heroId: HeroId): IO[BankRepoError, BankVault]
  def deposit(heroId: HeroId, item: Item): IO[BankRepoError, Unit]
  def withdraw(heroId: HeroId, itemId: Long): IO[BankRepoError, Item]
  def depositSilver(heroId: HeroId, amount: Long): IO[BankRepoError, Unit]
  def withdrawSilver(heroId: HeroId, amount: Long): IO[BankRepoError, Unit]
}

object BankRepository {
  val live: ZLayer[BankVaultDao, Nothing, BankRepository] =
    ZLayer.fromFunction(new BankRepositoryLive(_))
}
