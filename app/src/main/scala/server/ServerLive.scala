package server

import cats.syntax.semigroupk._
import fs2.io.net.Network
import io.circe.Json
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.dsl.Http4sDsl
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.{HttpApp, HttpRoutes}
import pangea.model.user.VkId
import pangea.service.admin.{AdminConfig, AdminPanel}
import pangea.service.donation.Donations
import pangea.service.state.{StateHandler, UserAction}
import pangea.service.chat.ChatCommand
import server.model.{ServerConfig, VkEvent}
import zio.interop.catz._
import zio.{Task, UIO, ZIO}

final class ServerLive(
  config: ServerConfig,
  stateHandler: StateHandler,
  adminConfig: AdminConfig,
  donations: Donations
) extends Server {

  private val payments = new PaymentRoutes(donations)

  private val logs = new LogsRoutes(adminConfig)

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
            // Пароль админ-панели приходит обычным сообщением, а лог отдаётся
            // наружу — в логе от него остаётся только метка.
            val logged = AdminPanel.maskSecrets(msg.text, adminConfig.password)
            ZIO.logInfo(s"peer=${msg.peerId} from=${msg.fromId.getOrElse(0L)} id=${msg.id} cmid=${msg.conversationMessageId.getOrElse(0L)} text=$logged payload=${msg.payload.getOrElse("")}") *>
              // Беседа — не игрок: её peer_id за героя принимать нельзя, оттуда
              // мы слушаем только команды вроде «Передать».
              (if (msg.fromChat) handleChat(msg)
               else
                 stateHandler
                   .makeActionVK(
                     VkId(msg.peerId.toString),
                     msg.id,
                     UserAction(msg.text, msg.payload)
                   ))
                .catchAll(err => ZIO.logError(err.getMessage))
          case None => ZIO.attempt(println(json.noSpaces))
        }).catchAll { err =>
          ZIO.logError(s"error occurred: $err")
        }
        resp <- Ok("ok")
      } yield resp
  }

  /** Что умеет общая беседа: «Передать» в ответ (или пересылкой) на
    * сообщение того, кому передают, и две команды о себе — «Мой профиль» и
    * «Моё снаряжение», ответ на которые уходит в саму беседу. Всё прочее там нас
    * не касается. */
  private def handleChat(msg: VkEvent.Message): Task[Unit] =
    msg.fromId.filter(_ > 0L) match {
      case None => ZIO.unit
      case Some(from) =>
        ChatCommand.selfCommand(msg.text) match {
          case Some(command) => stateHandler.selfToChat(VkId(from.toString), command, msg.chatEventKey)
          case None =>
            (msg.quotedAuthor, ChatCommand.transferQuery(msg.text)) match {
              case (Some(to), Some(query)) if to > 0L =>
                stateHandler.transferFromChat(VkId(from.toString), VkId(to.toString), query, msg.chatEventKey)
              case _ => ZIO.unit
            }
        }
    }

  private val httpApp: HttpApp[Task] = (routes <+> payments.routes <+> logs.routes).orNotFound

  override def run(): UIO[Unit] = {
    // Явный Network[Task] вместо устаревшего неявного implicitForAsync (http4s 3.7.0).
    implicit val network: Network[Task] = Network.forAsync[Task]
    EmberServerBuilder
      .default[Task]
      .withHost(config.host)
      .withPort(config.port)
      .withHttpApp(httpApp)
      .build
      .useForever
      .orDie
  }
}
