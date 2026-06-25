package pangea.dao.user

import doobie.util.transactor
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import zio.{Task, ZLayer}

trait UserDao {
  def getUserById(userId: UserId): Task[Option[User]]
  def getUserByVkId(vkId: VkId): Task[Option[User]]
  def getUserByTelegramId(telegramId: TelegramId): Task[Option[User]]
  def insertUser(user: User): Task[UserId]
  def checkAndRecordEvent(userId: UserId, eventId: Long): Task[Boolean]
}

object UserDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, UserDaoLive] =
    ZLayer.fromFunction(new UserDaoLive(_))
}
