package pangea.service.sender.vk

import org.http4s.client.Client
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.{Method, Request}
import pangea.model.user.UserId
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

  override def sendMessage(id: UserId, message: String): Task[Unit] = {
    val queryParams = Map(
      "message"   -> List(message),
      "user_id"   -> List(id.value.toString),
      "random_id" -> List(randomId.toString)
    ) ++ baseQueryParams

    val request = Request[Task](
      method = Method.GET,
      uri = uri / "messages.send" =? queryParams
    )

    client
      .run(request)
      .use(_ => ZIO.unit)
  }

  private def randomId = Random.between(Int.MinValue, Int.MaxValue)

}
