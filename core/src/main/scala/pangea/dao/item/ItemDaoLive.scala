package pangea.dao.item

import doobie.implicits.toSqlInterpolator
import doobie.util.transactor.Transactor
import doobie.implicits._
import pangea.model.item.{Item, ItemType}
import zio.Task
import zio.interop.catz._

class ItemDaoLive(xa: Transactor[Task]) extends ItemDao {

  override def getItemById(id: Long): Task[Option[Item]] =
    sql"select * from items where id = $id"
      .query[Item]
      .option
      .transact(xa)

  override def insertItem(item: Item): Task[Unit] =
    sql"insert into items(id, item_type, attack, accuracy, armor, defence, evasion) values(${item.id}, ${item.itemType}, ${item.attack},${item.accuracy},${item.armor},${item.defence},${item.evasion})".update.run
      .transact(xa)
      .unit
}
