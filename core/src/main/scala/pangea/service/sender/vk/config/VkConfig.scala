package pangea.service.sender.vk.config

import pureconfig.ConfigSource
import pureconfig.generic.auto._
import zio.{ZIO, ZLayer}

case class VkConfig(token: String)

object VkConfig {
  private val loadConfig =
    ZIO.attempt(ConfigSource.default.at("vk").loadOrThrow[VkConfig])

  val live: ZLayer[Any, Throwable, VkConfig] =
    ZLayer.fromZIO(loadConfig)

}
