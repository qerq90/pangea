package pangea.dao.user

import doobie.util.fragment.Fragment
import doobie.util.transactor.Transactor
import zio.interop.catz._
import doobie.implicits._
import pangea.model.user.{TelegramId, User, UserId, VkId}
import zio.Task

class UserDaoLive(xa: Transactor[Task]) extends UserDao {

  private val columns = Fragment.const(UserDaoLive.Columns)

  override def getUserById(userId: UserId): Task[Option[User]] =
    (fr"select" ++ columns ++ fr"from users where id = ${userId.value}")
      .query[User]
      .option
      .transact(xa)

  override def getUserByVkId(vkId: VkId): Task[Option[User]] =
    (fr"select" ++ columns ++ fr"from users where vk_id = ${vkId.value}")
      .query[User]
      .option
      .transact(xa)

  override def getUserByTelegramId(telegramId: TelegramId): Task[Option[User]] =
    (fr"select" ++ columns ++ fr"from users where telegram_id = ${telegramId.value}")
      .query[User]
      .option
      .transact(xa)

  override def insertUser(user: User): Task[UserId] =
    sql"insert into users(vk_id, telegram_id) values(${user.vkId}, ${user.telegramId})".update
      .withUniqueGeneratedKeys[UserId]("id")
      .transact(xa)

  override def checkAndRecordEvent(userId: UserId, eventId: Long): Task[Boolean] =
    // Заодно отмечаем, что игрок был здесь: по этой метке админ-панель считает,
    // сколько народу заходило за сутки и за неделю.
    sql"""UPDATE users SET last_event_id = $eventId, last_seen_at = now()
          WHERE id = ${userId.value} AND (last_event_id IS NULL OR last_event_id <> $eventId)"""
      .update.run.transact(xa).map(_ > 0)

  override def updateReceiptEmail(userId: UserId, email: String): Task[Unit] =
    sql"update users set receipt_email = $email where id = ${userId.value}"
      .update.run.transact(xa).unit
}

object UserDaoLive {

  /** Колонки `users` в порядке полей [[User]]: `Read[User]` читает их по
    * позиции, поэтому новое поле без колонки здесь падает не на компиляции, а
    * уже в рантайме. На соответствие стоит тест (`UserDaoSpec`). */
  val Columns: String = "id, vk_id, telegram_id, receipt_email"
}
