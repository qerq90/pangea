package server

import pangea.service.admin.AdminConfig
import pangea.service.state.StateHandler
import server.model.ServerConfig
import zio._

trait Server {
  def run(): UIO[Unit]
}

object Server {
  val live: ZLayer[ServerConfig with StateHandler with AdminConfig, Nothing, Server] =
    ZLayer {
      for {
        config       <- ZIO.service[ServerConfig]
        stateHandler <- ZIO.service[StateHandler]
        adminConfig  <- ZIO.service[AdminConfig]
      } yield new ServerLive(config, stateHandler, adminConfig)
    }
}
