package pangea.dao.inventory

import doobie.postgres.circe.json.implicits._
import doobie.implicits._
import doobie.util.transactor.Transactor
import io.circe.syntax.EncoderOps
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory
import pangea.model.item.Item
import zio.Task
import zio.interop.catz._

import pangea.model.inventory.Inventory.meta

class InventoryDaoLive(xa: Transactor[Task]) extends InventoryDao {

  override def create(heroId: HeroId): Task[Unit] =
    sql"insert into inventories(hero_id, max_items, items) values($heroId, 30, ${List.empty[Item].asJson})".update.run
      .transact(xa)
      .unit

  override def get(heroId: HeroId): Task[Inventory] =
    sql"select * from inventories where hero_id = $heroId"
      .query[Inventory]
      .unique
      .transact(xa)

  override def update(inventory: Inventory): Task[Unit] =
    sql"update inventories set max_items = ${inventory.maxItems}, items = ${inventory.items.asJson} where id = ${inventory.id}".update.run
      .transact(xa)
      .unit
}
