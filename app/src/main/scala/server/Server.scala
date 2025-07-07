package server

import pangea.dao.monster.MonsterDao
import pangea.service.sender.vk.VkSender
import server.model.ServerConfig
import zio._

trait Server {
  def run(): UIO[Unit]
}

object Server {
  val live
      : ZLayer[ServerConfig with VkSender with MonsterDao, Nothing, Server] =
    ZLayer {
      for {
        config     <- ZIO.service[ServerConfig]
        vkClient   <- ZIO.service[VkSender]
        monsterDao <- ZIO.service[MonsterDao]
      } yield new ServerLive(config, vkClient, monsterDao)
    }
}
