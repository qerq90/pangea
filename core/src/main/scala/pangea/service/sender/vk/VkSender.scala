package pangea.service.sender.vk

import org.http4s.client.Client
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.{Method, Request}
import pangea.model.user.UserId
import pangea.model.vk.Attachment
import pangea.service.sender.Sender
import pangea.service.sender.vk.config.VkConfig
import zio.interop.catz._
import zio.{Task, ZIO}

import scala.util.Random

class VkSender(client: Client[Task], config: VkConfig) extends Sender {

  private val uri = uri"https://api.vk.com/method/"
  private val baseQueryParams: Map[String, List[String]] = Map(
    "access_token" -> List(config.token),
    "v"            -> List("5.199")
  )

  override def sendMessage(
      id: UserId,
      message: String,
      attachments: List[Attachment]
  ): Task[Unit] = {
    var queryParams = Map.empty[String, List[String]]
    if (attachments.nonEmpty) {
      queryParams ++= Map(
        "message"    -> List(message),
        "user_id"    -> List(id.value.toString),
        "random_id"  -> List(randomId.toString),
        "attachment" -> List(attachments.mkString(","))
      ) ++ baseQueryParams
    } else {
      queryParams ++= Map(
        "message"   -> List(message),
        "user_id"   -> List(id.value.toString),
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

  private def randomId = Random.between(Int.MinValue, Int.MaxValue)

}
