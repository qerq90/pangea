package pangea.dao.user

import doobie.implicits.toSqlInterpolator
import doobie.util.transactor.Transactor
import zio.interop.catz._
import doobie.implicits._
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import zio.Task

class UserDaoLive(xa: Transactor[Task]) extends UserDao {

  override def getUserById(userId: UserId): Task[Option[User]] =
    sql"select id, vk_id, telegram_id from users where id = ${userId.value}"
      .query[User]
      .option
      .transact(xa)

  override def getUserByVkId(vkId: VkId): Task[Option[User]] =
    sql"select id, vk_id, telegram_id from users where vk_id = ${vkId.value}"
      .query[User]
      .option
      .transact(xa)

  override def getUserByTelegramId(telegramId: TelegramId): Task[Option[User]] =
    sql"select id, vk_id, telegram_id from users where telegram_id = ${telegramId.value}"
      .query[User]
      .option
      .transact(xa)

  override def insertUser(user: User): Task[UserId] =
    sql"insert into users(vk_id, telegram_id) values(${user.vkId}, ${user.telegramId})".update
      .withUniqueGeneratedKeys[UserId]("id")
      .transact(xa)

  override def checkAndRecordEvent(userId: UserId, eventId: Long): Task[Boolean] =
    sql"""UPDATE users SET last_event_id = $eventId
          WHERE id = ${userId.value} AND (last_event_id IS NULL OR last_event_id <> $eventId)"""
      .update.run.transact(xa).map(_ > 0)
}
