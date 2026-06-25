package pangea.service.schedule

import pangea.dao.schedule.ScheduledTaskDao
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.UserId
import zio.{Task, ZLayer}

/**
 * Write-сторона планировщика: кладёт/снимает durable-задачи в `scheduled_tasks`.
 * Зависит только от DAO — поэтому состояния могут планировать действия, не утягивая
 * за собой `StateHandler` (исполнение делает [[SchedulerPoller]]).
 */
trait Scheduler {

  /**
   * Запланировать действие. Перед вставкой снимает прежние `Pending`-задачи того же
   * `kind` у игрока — чтобы повторный вход в состояние не плодил дубликаты.
   *
   * @param fireAt        момент срабатывания (epoch ms)
   * @param expectedState состояние, в котором задача актуальна
   * @param action        payload синтетического `UserAction`, напр. `{"action":"Revive"}`
   * @return id задачи
   */
  def schedule(userId: UserId, fireAt: Long, kind: TaskKind, expectedState: StateType, action: String): Task[Long]

  /** Снять все ожидающие задачи игрока данного типа. */
  def cancel(userId: UserId, kind: TaskKind): Task[Unit]
}

object Scheduler {
  val live: ZLayer[ScheduledTaskDao, Nothing, Scheduler] =
    ZLayer.fromFunction(new SchedulerLive(_))
}
