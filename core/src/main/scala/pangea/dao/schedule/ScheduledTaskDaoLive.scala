package pangea.dao.schedule

import doobie.implicits._
import doobie.util.transactor.Transactor
import pangea.model.schedule.{ScheduledTask, TaskKind, TaskStatus}
import pangea.model.state.StateType
import pangea.model.user.UserId
import zio.Task
import zio.interop.catz._

class ScheduledTaskDaoLive(xa: Transactor[Task]) extends ScheduledTaskDao {

  // Литералы статусов приводим к базовому типу: doobie ищет Meta[TaskStatus],
  // а не для singleton-подтипа `Pending.type`.
  private val pending:   TaskStatus = TaskStatus.Pending
  private val done:      TaskStatus = TaskStatus.Done
  private val dead:      TaskStatus = TaskStatus.Dead
  private val cancelled: TaskStatus = TaskStatus.Cancelled

  override def insert(
    userId:        UserId,
    fireAt:        Long,
    kind:          TaskKind,
    expectedState: StateType,
    action:        String,
    createdAt:     Long
  ): Task[Long] =
    sql"""insert into scheduled_tasks(user_id, fire_at, kind, expected_state, action, status, attempts, created_at)
          values(${userId.value}, $fireAt, $kind, $expectedState, $action, $pending, 0, $createdAt)"""
      .update
      .withUniqueGeneratedKeys[Long]("id")
      .transact(xa)

  override def claimDue(now: Long, limit: Int): Task[List[ScheduledTask]] =
    sql"""select id, user_id, fire_at, kind, expected_state, action, status, attempts, created_at
          from scheduled_tasks
          where status = $pending and fire_at <= $now
          order by fire_at
          limit $limit"""
      .query[ScheduledTask]
      .to[List]
      .transact(xa)

  override def markDone(id: Long): Task[Unit] =
    sql"update scheduled_tasks set status = $done where id = $id"
      .update.run.transact(xa).unit

  override def markDead(id: Long): Task[Unit] =
    sql"update scheduled_tasks set status = $dead where id = $id"
      .update.run.transact(xa).unit

  override def bumpAttempts(id: Long): Task[Int] =
    sql"update scheduled_tasks set attempts = attempts + 1 where id = $id returning attempts"
      .query[Int].unique.transact(xa)

  override def cancel(userId: UserId, kind: TaskKind): Task[Int] =
    sql"""update scheduled_tasks set status = $cancelled
          where user_id = ${userId.value} and kind = $kind and status = $pending"""
      .update.run.transact(xa)
}
