package pangea.test

import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.repository.user.UserRepository
import zio.{Ref, Task, ZIO}

class TestUserRepository(usersRef: Ref[Map[UserId, User]], eventsRef: Ref[Set[(UserId, Long)]]) extends UserRepository {

  def insertUserByVk(vkId: VkId): Task[User] =
    ZIO.fail(new Throwable("TestUserRepository: insertUserByVk not supported, seed a user via TestUserRepository.withUser"))

  def insertUserByTelegramId(telegramId: TelegramId): Task[User] =
    ZIO.fail(new Throwable("TestUserRepository: insertUserByTelegramId not supported, seed a user via TestUserRepository.withUser"))

  def getUserById(userId: UserId): Task[Option[User]] = usersRef.get.map(_.get(userId))

  def getUserByVkId(vkId: VkId): Task[Option[User]] = usersRef.get.map(_.values.find(_.vkId == vkId))

  def getUserByTelegramId(telegramId: TelegramId): Task[Option[User]] =
    usersRef.get.map(_.values.find(_.telegramId == telegramId))

  /** Как реальный `checkAndRecordEvent`: true, только если этот `eventId` для
   *  этого пользователя ещё не встречался. */
  def checkAndRecordEvent(userId: UserId, eventId: Long): Task[Boolean] =
    eventsRef.modify { seen =>
      val key = (userId, eventId)
      if (seen.contains(key)) (false, seen) else (true, seen + key)
    }
}

object TestUserRepository {
  def withUser(user: User): Task[TestUserRepository] =
    for {
      usersRef  <- Ref.make(Map(user.userId -> user))
      eventsRef <- Ref.make(Set.empty[(UserId, Long)])
    } yield new TestUserRepository(usersRef, eventsRef)
}
