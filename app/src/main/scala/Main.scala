package app

import pangea.dao.Transactor
import pangea.dao.config.PostgresConfig
import pangea.dao.event.EventDao
import pangea.dao.hero.HeroDao
import pangea.dao.inventory.InventoryDao
import pangea.dao.monster.MonsterDao
import pangea.dao.user.UserDao
import pangea.repository.event.EventRepository
import pangea.repository.hero.HeroRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.user.{UserRepository, UserRepositoryLive}
import pangea.service.sender.Api
import pangea.service.sender.vk.config.VkConfig
import pangea.service.state.StateHandler
import pangea.service.state.states.StatesMap
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
      Api.vk,
      MonsterDao.live,
      HeroDao.live,
      UserDao.live,
      EventDao.live,
      InventoryDao.live,
      StatesMap.live,
      HeroRepository.live,
      UserRepository.live,
      EventRepository.live,
      InventoryRepository.live,
      StateHandler.live,
      ServerConfig.live,
      Server.live
    )

  private val program =
    ZIO.serviceWithZIO[Server](_.run())

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    program.provideLayer(env)
}
