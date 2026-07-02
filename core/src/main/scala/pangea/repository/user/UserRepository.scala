package pangea.repository.user

import pangea.dao.user.UserDao
import pangea.model.user.{TelegramId, User, UserId, VkId}
import zio.{Task, ZLayer}

trait UserRepository {
  def insertUserByVk(vkId: VkId): Task[User]
  def insertUserByTelegramId(telegramId: TelegramId): Task[User]
  def getUserById(userId: UserId): Task[Option[User]]
  def getUserByVkId(vkId: VkId): Task[Option[User]]
  def getUserByTelegramId(telegramId: TelegramId): Task[Option[User]]
  def checkAndRecordEvent(userId: UserId, eventId: Long): Task[Boolean]
}

object UserRepository {
  val live: ZLayer[UserDao, Nothing, UserRepository] =
    ZLayer.fromFunction(new UserRepositoryLive(_))
}
