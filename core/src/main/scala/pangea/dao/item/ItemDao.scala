package pangea.dao.item

import doobie.util.transactor
import pangea.model.item.Item
import zio.{Task, ZLayer}

trait ItemDao {
  def getItemById(id: Long): Task[Option[Item]]
  def insertItem(item: Item): Task[Unit]
}

object ItemDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, ItemDaoLive] =
    ZLayer.fromFunction(new ItemDaoLive(_))
}
