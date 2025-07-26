package pangea.service.sender.vk

import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.client.Client
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.{Method, Request}
import pangea.model.user.User
import pangea.model.vk.Attachment
import pangea.model.vk.keyboard.Keyboard
import pangea.model.vk.model.UserResponse
import pangea.service.sender.Api
import pangea.service.sender.vk.config.VkConfig
import zio.interop.catz._
import zio.{Task, ZIO}

import scala.util.Random

class VkApi(client: Client[Task], config: VkConfig) extends Api {

  private val uri = uri"https://api.vk.com/method/"
  private val baseQueryParams: Map[String, List[String]] = Map(
    "access_token" -> List(config.token),
    "v"            -> List("5.199")
  )

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

    client
      .run(request)
      .use(res =>
        res.bodyText.compile.toList.map(_.mkString("")).tap(ZIO.log(_)).unit
      )
  }

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
