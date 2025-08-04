package pangea.repository.event

import io.circe.Json
import pangea.dao.event.EventDao
import pangea.model.event.DungeonEvent
import pangea.model.user.UserId
import zio.{Task, ZLayer}

trait EventRepository {
  def startEvent(
      userId: UserId,
      eventType: String,
      data: Json
  ): Task[DungeonEvent]
  def getEvent(userId: UserId, eventType: String): Task[DungeonEvent]
  def endEvent(userId: UserId, eventType: String): Task[Unit]
}

object EventRepository {
  val live: ZLayer[EventDao, Nothing, EventRepository] =
    ZLayer.fromFunction(new EventRepositoryLive(_))
}
