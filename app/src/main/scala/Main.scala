package app

import pangea.dao.Transactor
import pangea.dao.config.PostgresConfig
import pangea.dao.artifact.ArtifactDao
import pangea.dao.auction.AuctionDao
import pangea.dao.payout.PayoutDao
import pangea.dao.bank.BankVaultDao
import pangea.dao.barrel.BarrelDao
import pangea.dao.hero.HeroDao
import pangea.dao.inventory.InventoryDao
import pangea.dao.item.ItemDao
import pangea.dao.journal.JournalLive
import pangea.dao.schedule.ScheduledTaskDao
import pangea.dao.sendfailure.SendFailureDao
import pangea.dao.user.UserDao
import pangea.engine.{Players, SceneContent}
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.auction.AuctionRepository
import pangea.repository.bank.BankRepository
import pangea.repository.barrel.BarrelRepository
import pangea.repository.hero.HeroRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.repository.user.UserRepository
import pangea.service.payout.Payouts
import pangea.service.schedule.{Scheduler, SchedulerPoller}
import pangea.service.sender.Api
import pangea.service.sender.vk.config.VkConfig
import pangea.service.sender.vk.VkPlayers
import pangea.service.state.StateHandler
import pangea.service.state.states.StatesMap
import server.Server
import server.model.ServerConfig
import zio._
import zio.logging.backend.SLF4J

object Main extends ZIOAppDefault {

  override val bootstrap: ZLayer[ZIOAppArgs, Nothing, Unit] =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j

  private type Env = Server with SchedulerPoller

  private val env =
    ZLayer.make[Env](
      VkConfig.live,
      PostgresConfig.live,
      Transactor.live,
      Api.vk,
      ZLayer.fromZIO(ZIO.service[Api].map(VkPlayers(_))).asInstanceOf[ZLayer[Api, Nothing, Players]],
      HeroDao.live,
      UserDao.live,
      InventoryDao.live,
      BarrelDao.live,
      BankVaultDao.live,
      AuctionDao.live,
      ArtifactDao.live,
      PayoutDao.live,
      ItemDao.live,
      ScheduledTaskDao.live,
      SendFailureDao.live,
      ItemRepository.live,
      JournalLive.live,
      SceneContent.live,
      StatesMap.live,
      HeroRepository.live,
      UserRepository.live,
      InventoryRepository.live,
      BarrelRepository.live,
      BankRepository.live,
      AuctionRepository.live,
      ArtifactRepository.live,
      Payouts.live,
      StateHandler.live,
      Scheduler.live,
      SchedulerPoller.live,
      ServerConfig.live,
      Server.live
    )

  private val program =
    for {
      poller <- ZIO.service[SchedulerPoller]
      _      <- poller.start.forkDaemon
      _      <- ZIO.serviceWithZIO[Server](_.run())
    } yield ()

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    program.provideLayer(env)
}
