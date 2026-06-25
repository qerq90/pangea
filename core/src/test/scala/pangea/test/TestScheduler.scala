package pangea.test

import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.UserId
import pangea.service.schedule.Scheduler
import zio.{Ref, Task, UIO}

/** Записывающий стаб планировщика для тестов состояний. */
class TestScheduler(
  scheduledRef: Ref[List[TestScheduler.Scheduled]],
  cancelledRef: Ref[List[(UserId, TaskKind)]]
) extends Scheduler {

  override def schedule(userId: UserId, fireAt: Long, kind: TaskKind, expectedState: StateType, action: String): Task[Long] =
    scheduledRef.update(_ :+ TestScheduler.Scheduled(userId, fireAt, kind, expectedState, action)).as(0L)

  override def cancel(userId: UserId, kind: TaskKind): Task[Unit] =
    cancelledRef.update(_ :+ (userId -> kind)).unit

  def scheduled: UIO[List[TestScheduler.Scheduled]] = scheduledRef.get
  def cancelled: UIO[List[(UserId, TaskKind)]]       = cancelledRef.get
}

object TestScheduler {
  final case class Scheduled(userId: UserId, fireAt: Long, kind: TaskKind, expectedState: StateType, action: String)

  def make: UIO[TestScheduler] =
    for {
      sched <- Ref.make(List.empty[Scheduled])
      canc  <- Ref.make(List.empty[(UserId, TaskKind)])
    } yield new TestScheduler(sched, canc)
}
