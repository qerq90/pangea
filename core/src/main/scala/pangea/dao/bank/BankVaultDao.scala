package pangea.dao.bank

import doobie.util.transactor
import pangea.model.bank.BankVault
import pangea.model.hero.HeroId
import zio.{Task, ZLayer}

trait BankVaultDao {
  /** Гарантирует наличие записи хранилища для героя и возвращает её. */
  def getOrCreate(heroId: HeroId): Task[BankVault]
  def update(vault: BankVault): Task[Unit]
}

object BankVaultDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, BankVaultDao] =
    ZLayer.fromFunction(new BankVaultDaoLive(_))
}
