package server

import zio.interop.catz._
import org.http4s.{HttpApp, HttpRoutes}
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.circe.CirceEntityCodec._
import org.http4s.implicits._
import pangea.dao.monster.MonsterDao
import pangea.model.user.UserId
import pangea.service.sender.vk.VkSender
import server.model.{ServerConfig, VkChecking}
import zio.{Task, UIO}

final class ServerLive(
  config: ServerConfig,
  vkSender: VkSender,
  monsterDao: MonsterDao
) extends Server {

  private val dsl = Http4sDsl[Task]

  import dsl._

  private val routes: HttpRoutes[Task] = HttpRoutes.of[Task] {
    case req @ POST -> Root =>
      for {
        vkChecking <- req.as[VkChecking].option
        resp <- vkChecking match {
          case Some(_) => Ok("33ebc838")
          case None    => Ok("ok")
        }
      } yield resp
  }

  private val httpApp: HttpApp[Task] = routes.orNotFound

  override def run(): UIO[Unit] =
    EmberServerBuilder
      .default[Task]
      .withHost(config.host)
      .withPort(config.port)
      .withHttpApp(httpApp)
      .build
      .useForever
      .orDie

}
