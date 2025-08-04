package pangea.dao.event

import doobie.implicits._
import doobie.postgres.circe.json.implicits._
import doobie.postgres.implicits._
import doobie.util.transactor.Transactor
import pangea.model.event.{DungeonEvent, DungeonEventStatus}
import pangea.model.user.UserId
import zio.interop.catz._
import zio.Task

class EventDaoLive(transactor: Transactor[Task]) extends EventDao {

  override def insertEvent(event: DungeonEvent): Task[Int] =
    sql"insert into events (user_id, event_type, status, data, created_at) values (${event.userId.value}, ${event.eventType}, ${event.status}, ${event.data}, ${event.createdAt})".update
      .withUniqueGeneratedKeys[Int]("id")
      .transact(transactor)

  override def findEvent(
      userId: UserId,
      eventType: String
  ): Task[DungeonEvent] = {
    val status: DungeonEventStatus = DungeonEventStatus.InProgress
    sql"select * from events where user_id = ${userId.value} and status = $status and event_type = $eventType"
      .query[DungeonEvent]
      .unique
      .transact(transactor)
  }

  override def changeStatus(
      userId: UserId,
      eventType: String,
      status: DungeonEventStatus
  ): Task[Unit] =
    sql"update events set status = $status where user_id = $userId and event_type = $eventType".update.run
      .transact(transactor)
      .unit
}
