package pangea.dao.bank

import doobie.implicits._
import doobie.postgres.circe.json.implicits._
import doobie.util.transactor.Transactor
import io.circe.syntax.EncoderOps
import pangea.model.bank.BankVault
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.{meta, Items}
import pangea.model.item.Item
import zio.Task
import zio.interop.catz._

class BankVaultDaoLive(xa: Transactor[Task]) extends BankVaultDao {

  override def getOrCreate(heroId: HeroId): Task[BankVault] =
    (for {
      _ <- sql"insert into bank_vaults(hero_id, cells, items, silver) values($heroId, 0, ${Items(List.empty[Item])}, 0) on conflict (hero_id) do nothing".update.run
      v <- sql"select id, hero_id, cells, items, silver from bank_vaults where hero_id = $heroId".query[BankVault].unique
    } yield v).transact(xa)

  override def update(vault: BankVault): Task[Unit] =
    sql"update bank_vaults set cells = ${vault.cells}, items = ${vault.items.asJson}, silver = ${vault.silver} where id = ${vault.id}".update.run
      .transact(xa)
      .unit
}
