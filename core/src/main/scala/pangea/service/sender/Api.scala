package pangea.service.sender

import pangea.model.user.User
import pangea.model.vk.Attachment
import pangea.model.vk.keyboard.Keyboard
import pangea.model.vk.model.UserResponse
import pangea.service.sender.vk.VkApi
import pangea.service.sender.vk.config.VkConfig
import zio.{Task, ZIO, ZLayer}

trait Api {
  def getName(user: User): Task[UserResponse]

  def sendMessage(
      user: User,
      message: String,
      attachments: List[Attachment],
      keyboard: Option[Keyboard]
  ): Task[Unit]
}

object Api {
  val vk: ZLayer[VkConfig, Throwable, VkApi] = ZLayer.fromZIO(for {
    config <- ZIO.service[VkConfig]
    client <- pangea.client.Client.make
  } yield new VkApi(client, config))
}
