package pangea.repository.user

import pangea.dao.user.UserDao
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.repository.user.UserRepositoryLive.{
  newTelegramUser,
  newVkUser,
  nullTelegramId,
  nullVkId
}
import zio.Task

case class UserRepositoryLive(userDao: UserDao) extends UserRepository {

  override def insertUserByVk(vkId: VkId): Task[User] =
    userDao
      .insertUser(newVkUser(vkId))
      .map(userId => User(userId, vkId, nullTelegramId))

  override def insertUserByTelegramId(telegramId: TelegramId): Task[User] =
    userDao
      .insertUser(newTelegramUser(telegramId))
      .map(userId => User(userId, nullVkId, telegramId))

  override def getUserByVkId(vkId: VkId): Task[Option[User]] =
    userDao.getUserByVkId(vkId)

  override def getUserByTelegramId(telegramId: TelegramId): Task[Option[User]] =
    userDao.getUserByTelegramId(telegramId)

  override def updateState(userId: UserId, newState: StateType): Task[Unit] =
    userDao.updateState(userId, newState)
}

object UserRepositoryLive {
  private def newVkUser(vkId: VkId): User =
    User(UserId(-1), vkId, nullTelegramId)

  private def newTelegramUser(telegramId: TelegramId): User =
    User(UserId(-1), nullVkId, telegramId)

  private val nullTelegramId = TelegramId("-1")
  private val nullVkId       = VkId("-1")
}
