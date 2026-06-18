package pangea.dao.item

import doobie.util.transactor
import pangea.model.item.Item
import zio.{Task, ZLayer}

trait ItemDao {
  def insert(heroId: Long, item: Item): Task[Long]
}

object ItemDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, ItemDao] =
    ZLayer.fromFunction(new ItemDaoLive(_))
}
