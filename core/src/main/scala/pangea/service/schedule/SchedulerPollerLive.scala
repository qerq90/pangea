package pangea.service.schedule

import pangea.dao.schedule.ScheduledTaskDao
import pangea.model.schedule.ScheduledTask
import pangea.service.state.{StateHandler, UserAction}
import zio.{Duration, Task, ZIO}

import java.util.concurrent.TimeUnit

class SchedulerPollerLive(dao: ScheduledTaskDao, handler: StateHandler) extends SchedulerPoller {
  import SchedulerPollerLive._

  override def poll: Task[Unit] =
    for {
      now <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      due <- dao.claimDue(now, BatchLimit)
      _   <- ZIO.foreachDiscard(due)(process)
    } yield ()

  override def start: Task[Unit] =
    (poll.catchAllCause(c => ZIO.logErrorCause("Scheduler poll failed", c)) *> ZIO.sleep(PollInterval)).forever

  /** Исполнить одну задачу и проставить итоговый статус. */
  private def process(task: ScheduledTask): Task[Unit] = {
    val ua = UserAction(text = "", payload = Some(task.action))
    handler
      .runScheduled(task.userId, task.expectedState, ua)
      .foldZIO(
        err =>
          dao.bumpAttempts(task.id).flatMap { attempts =>
            ZIO.logError(s"Scheduled task ${task.id} (${task.kind}) failed, attempt $attempts: ${err.getMessage}") *>
              ZIO.when(attempts >= MaxAttempts)(dao.markDead(task.id)).unit
          },
        _ => dao.markDone(task.id)
      )
  }
}

object SchedulerPollerLive {
  /** Сколько задач забирать за тик. */
  val BatchLimit: Int = 64
  /** После скольких неудачных попыток задача помечается `Dead`. */
  val MaxAttempts: Int = 5
  /** Период опроса очереди. */
  val PollInterval: Duration = Duration.fromSeconds(5)
}
