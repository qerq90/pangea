package pangea.service.sender.vk.config

import pureconfig.ConfigSource
import pureconfig.generic.auto._
import zio.{ZIO, ZLayer}

/** @param chatPeerId `peer_id` общей беседы Пангеи (2000000000 + chat_id):
  *                    туда уходят объявления аукциона. Не задан — не пишем. */
case class VkConfig(token: String, chatPeerId: Option[Long] = None)

object VkConfig {
  private val loadConfig =
    ZIO.attempt(ConfigSource.default.at("vk").loadOrThrow[VkConfig])

  val live: ZLayer[Any, Throwable, VkConfig] =
    ZLayer.fromZIO(loadConfig)

}
