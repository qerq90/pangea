package pangea.service.sender

import pangea.model.user.User
import pangea.model.vk.Attachment
import pangea.service.sender.vk.VkSender
import pangea.service.sender.vk.config.VkConfig
import zio.{Task, ZIO, ZLayer}

trait Sender {
  def sendMessage(
      user: User,
      message: String,
      attachments: List[Attachment]
  ): Task[Unit]
}

object Sender {
  val vk: ZLayer[VkConfig, Throwable, VkSender] = ZLayer.fromZIO(for {
    config <- ZIO.service[VkConfig]
    client <- pangea.client.Client.make
  } yield new VkSender(client, config))
}
