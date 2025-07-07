package app

import pangea.dao.Transactor
import pangea.dao.config.PostgresConfig
import pangea.dao.monster.MonsterDao
import pangea.service.sender.Sender
import pangea.service.sender.vk.config.VkConfig
import server.Server
import server.model.ServerConfig
import zio._
import zio.logging.backend.SLF4J

object Main extends ZIOAppDefault {

  override val bootstrap: ZLayer[ZIOAppArgs, Nothing, Unit] =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j

  private type Env = Server

  private val env =
    ZLayer.make[Env](
      VkConfig.live,
      PostgresConfig.live,
      Transactor.live,
      Sender.vk,
      MonsterDao.live,
      ServerConfig.live,
      Server.live
    )

  private val program =
    ZIO.serviceWithZIO[Server](_.run())

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    program.provideLayer(env)
}
