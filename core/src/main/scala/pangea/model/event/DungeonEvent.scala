package pangea.model.event

import io.circe.Json
import pangea.model.user.UserId

import java.time.LocalDateTime

case class DungeonEvent(
  id: Long,
  userId: UserId,
  eventType: String,
  status: DungeonEventStatus,
  data: Json,
  createdAt: LocalDateTime
)

object DungeonEvent {
  def make(userId: UserId, eventType: String, data: Json): DungeonEvent =
    DungeonEvent(
      -1,
      userId,
      eventType,
      DungeonEventStatus.InProgress,
      data,
      LocalDateTime.now()
    )
}
