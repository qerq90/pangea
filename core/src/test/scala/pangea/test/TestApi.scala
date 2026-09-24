package pangea.test

import pangea.model.user.User
import pangea.model.vk.Attachment
import pangea.model.vk.keyboard.Keyboard
import pangea.model.vk.model.{UserResponse, User => VkUser}
import pangea.service.sender.Api
import zio.{Ref, Task, ZIO}

class TestApi(messagesRef: Ref[List[(String, Option[Keyboard])]], chatRef: Ref[List[String]]) extends Api {

  def getName(user: User): Task[UserResponse] =
    ZIO.succeed(UserResponse(List(VkUser("Иван", "Иванов"))))

  def sendMessage(
      user: User,
      message: String,
      attachments: List[Attachment],
      keyboard: Option[Keyboard]
  ): Task[Unit] =
    messagesRef.update(_ :+ (message, keyboard))

  def sendToChat(message: String): Task[Unit] = chatRef.update(_ :+ message)

  def sentMessages: Task[List[(String, Option[Keyboard])]] = messagesRef.get

  def chatMessages: Task[List[String]] = chatRef.get
}

object TestApi {
  def make: Task[TestApi] =
    for {
      messages <- Ref.make(List.empty[(String, Option[Keyboard])])
      chat     <- Ref.make(List.empty[String])
    } yield new TestApi(messages, chat)
}
