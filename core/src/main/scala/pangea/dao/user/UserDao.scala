package pangea.dao.user

import doobie.util.transactor
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import zio.{Task, ZLayer}

trait UserDao {
  def getUserByVkId(vkId: VkId): Task[Option[User]]
  def getUserByTelegramId(telegramId: TelegramId): Task[Option[User]]
  def insertUser(user: User): Task[Unit]
  def updateState(userId: UserId, stateType: StateType): Task[Unit]
}

object UserDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, UserDaoLive] =
    ZLayer.fromFunction(new UserDaoLive(_))
}
