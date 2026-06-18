package pangea.model

import io.circe.Json
import pangea.model.user.UserId

import java.util.UUID

case class GameEvent(
  userId:    UserId,
  eventType: String,
  payload:   Json,
  traceId:   Option[UUID] = None
)
