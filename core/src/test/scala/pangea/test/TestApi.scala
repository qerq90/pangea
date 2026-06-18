package pangea.test

import pangea.model.user.User
import pangea.model.vk.Attachment
import pangea.model.vk.keyboard.Keyboard
import pangea.model.vk.model.{UserResponse, User => VkUser}
import pangea.service.sender.Api
import zio.{Ref, Task, ZIO}

class TestApi(messagesRef: Ref[List[(String, Option[Keyboard])]]) extends Api {

  def getName(user: User): Task[UserResponse] =
    ZIO.succeed(UserResponse(List(VkUser("Иван", "Иванов"))))

  def sendMessage(
      user: User,
      message: String,
      attachments: List[Attachment],
      keyboard: Option[Keyboard]
  ): Task[Unit] =
    messagesRef.update(_ :+ (message, keyboard))

  def sentMessages: Task[List[(String, Option[Keyboard])]] = messagesRef.get
}

object TestApi {
  def make: Task[TestApi] =
    Ref.make(List.empty[(String, Option[Keyboard])]).map(new TestApi(_))
}
