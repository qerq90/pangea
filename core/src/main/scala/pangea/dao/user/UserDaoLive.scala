package pangea.dao.user

import doobie.implicits.toSqlInterpolator
import doobie.util.transactor.Transactor
import zio.interop.catz._
import doobie.implicits._
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import zio.Task

class UserDaoLive(xa: Transactor[Task]) extends UserDao {

  override def getUserByVkId(vkId: VkId): Task[Option[User]] =
    sql"select * from users where vk_id = ${vkId.value}"
      .query[User]
      .option
      .transact(xa)

  override def getUserByTelegramId(telegramId: TelegramId): Task[Option[User]] =
    sql"select * from users where telegram_id = ${telegramId.value}"
      .query[User]
      .option
      .transact(xa)

  override def insertUser(user: User): Task[UserId] =
    sql"insert into users(vk_id, telegram_id) values(${user.vkId}, ${user.telegramId})".update
      .withUniqueGeneratedKeys[UserId]("id")
      .transact(xa)

  override def updateState(userId: UserId, stateType: StateType): Task[Unit] =
    sql"update users set state = $stateType where id = $userId".update.run
      .transact(xa)
      .unit
}
