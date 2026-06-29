package server

import cats.syntax.semigroupk._
import io.circe.Json
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.{HttpApp, HttpRoutes}
import pangea.model.user.VkId
import pangea.service.state.{StateHandler, UserAction}
import server.model.{ServerConfig, VkEvent}
import zio.interop.catz._
import zio.{Task, UIO, ZIO}

final class ServerLive(
  config: ServerConfig,
  stateHandler: StateHandler
) extends Server {

  private val dsl = Http4sDsl[Task]

  import dsl._

  private val routes: HttpRoutes[Task] = HttpRoutes.of[Task] {
    case req @ POST -> Root =>
      for {
//        vkChecking <- req.as[VkChecking].option
//        resp <- vkChecking match {
//          case Some(_) =>
//            Ok(
//              "b874fbde",
//              `Content-Type`(MediaType.text.plain)
//            )
//          case None => Ok("ok")
//        }

        event <- req.as[VkEvent].option
        json  <- req.as[Json]
        _ <- (event match {
          case Some(value) =>
            val msg = value.`object`.message
            ZIO.logInfo(s"peer=${msg.peerId} text=${msg.text} payload=${msg.payload.getOrElse("")}") *>
              stateHandler
                .makeActionVK(
                  VkId(msg.peerId.toString),
                  msg.id,
                  UserAction(msg.text, msg.payload)
                )
                .catchAll(err => ZIO.logError(err.getMessage))
          case None => ZIO.attempt(println(json.noSpaces))
        }).catchAll { err =>
          ZIO.logError(s"error occurred: $err")
        }
        resp <- Ok("ok")
      } yield resp
  }

  private val httpApp: HttpApp[Task] = (routes <+> LogsRoutes.routes).orNotFound

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
