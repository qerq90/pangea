package pangea.repository.event

import io.circe.Json
import pangea.dao.event.EventDao
import pangea.model.event.DungeonEvent
import pangea.model.event.DungeonEventStatus.Ended
import pangea.model.user.UserId
import zio.Task

class EventRepositoryLive(eventDao: EventDao) extends EventRepository {

  override def startEvent(
      userId: UserId,
      eventType: String,
      data: Json
  ): Task[DungeonEvent] =
    eventDao
      .insertEvent(DungeonEvent.make(userId, eventType, data))
      .map(id => DungeonEvent.make(userId, eventType, data).copy(id = id))

  override def getEvent(userId: UserId, eventType: String): Task[DungeonEvent] =
    eventDao.findEvent(userId, eventType)

  override def endEvent(userId: UserId, eventType: String): Task[Unit] =
    eventDao.changeStatus(userId, eventType, Ended)
}
