package pangea.service.sender.vk

import io.circe.parser.parse
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.client.Client
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.{Method, Request}
import pangea.dao.sendfailure.SendFailureDao
import pangea.model.user.User
import pangea.model.vk.Attachment
import pangea.model.vk.keyboard.Keyboard
import pangea.model.vk.model.UserResponse
import pangea.service.sender.Api
import pangea.service.sender.vk.config.VkConfig
import zio.interop.catz._
import zio.{Ref, Schedule, Task, ZIO, durationInt}

import java.util.concurrent.TimeUnit
import scala.util.Random

class VkApi(client: Client[Task], config: VkConfig, failures: SendFailureDao) extends Api {

  private val uri = uri"https://api.vk.com/method/"
  private val baseQueryParams: Map[String, List[String]] = Map(
    "access_token" -> List(config.token),
    "v"            -> List("5.199")
  )

  // Транзиентные сбои VK/сети — лечим повтором. До 3 попыток с задержкой
  // 250 → 500 → 1000 мс. После — пишем инцидент в send_failures и
  // пробрасываем ошибку, чтобы catchAll в StateHandler мог отработать.
  private val sendRetry: Schedule[Any, Any, Any] =
    Schedule.recurs(2) && Schedule.exponential(250.millis)

  override def sendMessage(
      user: User,
      message: String,
      attachments: List[Attachment],
      keyboard: Option[Keyboard]
  ): Task[Unit] = {
    var queryParams = Map.empty[String, List[String]]
    keyboard.foreach(keyboard =>
      queryParams ++= Map[String, List[String]](
        "keyboard" -> List(keyboard.getJson)
      )
    )

    if (attachments.nonEmpty) {
      queryParams ++= Map(
        "message"    -> List(message),
        "user_id"    -> List(user.vkId.value),
        "random_id"  -> List(randomId.toString),
        "attachment" -> List(attachments.mkString(","))
      ) ++ baseQueryParams
    } else {
      queryParams ++= Map(
        "message"   -> List(message),
        "user_id"   -> List(user.vkId.value),
        "random_id" -> List(randomId.toString)
      ) ++ baseQueryParams
    }

    val request = Request[Task](
      method = Method.GET,
      uri = uri / "messages.send" =? queryParams
    )

    // Счётчик попыток для записи в журнал инцидентов.
    for {
      attempts <- Ref.make(0)
      send      = client.run(request).use { res =>
        for {
          _    <- attempts.update(_ + 1)
          body <- res.bodyText.compile.toList.map(_.mkString(""))
          _    <- ZIO.log(body)
          _    <- VkApi.failOnApiError(body)
        } yield ()
      }
      _ <- send.retry(sendRetry).tapError(err =>
        recordFailure(user, message, keyboard.map(_.getJson), err, attempts).orElse(ZIO.unit)
      )
    } yield ()
  }

  private def recordFailure(
    user:         User,
    messageText:  String,
    keyboardJson: Option[String],
    err:          Throwable,
    attemptsRef:  Ref[Int]
  ): Task[Unit] =
    for {
      now      <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      attempts <- attemptsRef.get
      (code, msg) = err match {
        case VkApi.VkApiError(c, m) => (Some(c), m)
        case other                  => (None, Option(other.getMessage).getOrElse(other.toString))
      }
      _ <- failures.record(
             userId       = Option(user.userId).filter(_.value > 0L),
             vkId         = user.vkId.value,
             messageText  = messageText,
             keyboardJson = keyboardJson,
             errorCode    = code,
             errorMessage = msg,
             attempts     = attempts,
             createdAt    = now
           )
    } yield ()

  override def getName(user: User): Task[UserResponse] = {
    val queryParams = baseQueryParams ++ Map(
      "user_ids" -> List(user.vkId.value)
    )

    val request = Request[Task](
      method = Method.GET,
      uri = uri / "users.get" =? queryParams
    )

    client.expect[UserResponse](request)
  }

  private def randomId = Random.between(Int.MinValue, Int.MaxValue)
}

object VkApi {

  /** Ошибка API VK: тело ответа содержит объект `error`. */
  final case class VkApiError(code: Int, message: String) extends RuntimeException(s"VK API error $code: $message")

  /**
   * Парсит тело ответа VK API. Если в JSON есть `error` — фейлит ZIO с
   * `VkApiError`, иначе — успех. Невалидный JSON игнорируется (VK иногда
   * отвечает текстом при проблемах транспорта — лог тела уже сделан выше).
   */
  def failOnApiError(body: String): Task[Unit] =
    parse(body) match {
      case Left(_) => ZIO.unit
      case Right(json) =>
        json.hcursor.downField("error").focus match {
          case None => ZIO.unit
          case Some(err) =>
            val code = err.hcursor.get[Int]("error_code").getOrElse(-1)
            val msg  = err.hcursor.get[String]("error_msg").getOrElse(err.noSpaces)
            ZIO.fail(VkApiError(code, msg))
        }
    }
}
