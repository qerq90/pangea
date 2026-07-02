package pangea.model.schedule

import enumeratum._
import pangea.model.state.StateType
import pangea.model.user.UserId

/**
 * Отложенное действие игрока (durable, таблица `scheduled_tasks`). По `fireAt`
 * поллер исполняет `action` (синтетический `UserAction`) под локом игрока, если
 * текущее состояние героя совпадает с `expectedState`. См. сервис `Scheduler`.
 *
 * @param fireAt        момент срабатывания (epoch ms)
 * @param kind          тип задачи (для массовой отмены и логов)
 * @param expectedState состояние, в котором задача имеет смысл; иначе — тихо `Done`
 * @param action        payload синтетического действия, напр. `{"action":"Revive"}`
 */
final case class ScheduledTask(
  id:            Long,
  userId:        UserId,
  fireAt:        Long,
  kind:          TaskKind,
  expectedState: StateType,
  action:        String,
  status:        TaskStatus,
  attempts:      Int,
  createdAt:     Long
)

sealed trait TaskKind extends EnumEntry
object TaskKind extends Enum[TaskKind] with DoobieEnum[TaskKind] {
  val values = findValues
  // Пробуждение из отдыха (Rest → Dungeon) по истечении таймера.
  case object Revive extends TaskKind
  // Исцеление травм снятой комнатой таверны спустя 3 часа.
  case object TavernHeal extends TaskKind
  // Завершение добычи на Золотой жиле спустя 15 минут — выдача золота.
  case object Harvest extends TaskKind
  // Завершение работы на стройке (1/4/8 часов) — выдача золота.
  case object Construction extends TaskKind
  // Завершение раскопок прикопанного схрона (~10 минут) — выдача добычи/могилы.
  case object SchronDig extends TaskKind
  // Завершение похода за сокровищем по карте клада (~10 минут) — выдача добычи.
  case object TreasureHunt extends TaskKind
}

sealed trait TaskStatus extends EnumEntry
object TaskStatus extends Enum[TaskStatus] with DoobieEnum[TaskStatus] {
  val values = findValues
  case object Pending   extends TaskStatus
  case object Done      extends TaskStatus
  case object Cancelled extends TaskStatus
  case object Dead      extends TaskStatus
}
