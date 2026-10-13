package server.model

import com.comcast.ip4s._
import pureconfig.module.ip4s._
import pureconfig._
import pureconfig.generic.auto._
import zio._

/** @param vkSecret секретный ключ Callback API группы (`VK_SECRET`). Без него
  *                 события конкурса активности не принимаются: их легко
  *                 подделать, а за них дают дублоны. */
case class ServerConfig(host: Host, port: Port, vkSecret: Option[String] = None)

object ServerConfig {
  private val loadConfig =
    ZIO.attempt(ConfigSource.default.at("server").loadOrThrow[ServerConfig])

  val live: ZLayer[Any, Throwable, ServerConfig] =
    ZLayer.fromZIO(loadConfig)
}
