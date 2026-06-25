package pangea.service.schedule

import pangea.dao.schedule.ScheduledTaskDao
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.UserId
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

class SchedulerLive(dao: ScheduledTaskDao) extends Scheduler {

  override def schedule(
    userId:        UserId,
    fireAt:        Long,
    kind:          TaskKind,
    expectedState: StateType,
    action:        String
  ): Task[Long] =
    for {
      now <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      _   <- dao.cancel(userId, kind)
      id  <- dao.insert(userId, fireAt, kind, expectedState, action, now)
    } yield id

  override def cancel(userId: UserId, kind: TaskKind): Task[Unit] =
    dao.cancel(userId, kind).unit
}
