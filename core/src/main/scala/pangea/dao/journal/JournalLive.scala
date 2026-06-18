package pangea.dao.journal

import doobie.implicits._
import doobie.postgres.circe.json.implicits._
import doobie.postgres.implicits._
import doobie.util.transactor.Transactor
import pangea.engine.{Journal, PartitionManager}
import pangea.model.GameEvent
import zio.interop.catz._
import zio.{Task, ZIO, ZLayer}

class JournalLive(xa: Transactor[Task]) extends Journal {
  override def append(event: GameEvent): Task[Unit] =
    sql"""INSERT INTO event_log (user_id, event_type, payload, trace_id)
          VALUES (${event.userId.value}, ${event.eventType}, ${event.payload}, ${event.traceId})"""
      .update.run.transact(xa).unit
}

object JournalLive {
  val live: ZLayer[Transactor[Task], Throwable, Journal] =
    ZLayer.fromZIO(
      ZIO.serviceWithZIO[Transactor[Task]] { xa =>
        PartitionManager.ensurePartitions(xa).as(new JournalLive(xa))
      }
    )
}
