package pangea.dao.event

import doobie.util.transactor
import pangea.model.event.{DungeonEvent, DungeonEventStatus}
import pangea.model.user.UserId
import zio.{Task, ZLayer}

trait EventDao {
  def insertEvent(event: DungeonEvent): Task[Int]
  def findEvent(userId: UserId, eventType: String): Task[DungeonEvent]
  def changeStatus(
      userId: UserId,
      eventType: String,
      status: DungeonEventStatus
  ): Task[Unit]
}

object EventDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, EventDao] =
    ZLayer.fromFunction(new EventDaoLive(_))
}
