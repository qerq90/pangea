package pangea.client.robokassa

import fs2.io.net.Network
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.{Method, Request, Uri}
import zio.interop.catz._
import zio.{Duration, Schedule, Task, ZIO, ZLayer}

import scala.concurrent.duration.DurationInt

/** Касса Робокассы. Знает про провод и про подписи; про дублоны и игроков — нет. */
trait RobokassaClient {

  /** Ссылка на оплату. Чистая функция: касса о заказе узнаёт, только когда
    * игрок её откроет. */
  def paymentUrl(request: PaymentRequest): String

  /** Состояние операции по номеру счёта. */
  def opState(invId: Long): Task[OpState]

  /** Подпись уведомления ResultURL. Без этой проверки «оплатить» мог бы любой,
    * кто знает адрес вебхука. */
  def verify(params: Map[String, String]): Boolean
}

class RobokassaClientLive(client: Client[Task], config: RobokassaConfig) extends RobokassaClient {

  /** `OpStateExt` только читает, поэтому повторять его безопасно — но только
    * когда касса не ответила вовсе: ответ с телом второй раз не изменится. */
  private val retry: Schedule[Any, Throwable, Any] =
    (Schedule.recurs(2) && Schedule.exponential(Duration.fromMillis(250)))
      .whileInput[Throwable](_.isInstanceOf[RobokassaError.Transport])

  override def paymentUrl(request: PaymentRequest): String = RobokassaApi.paymentUrl(config, request)

  override def opState(invId: Long): Task[OpState] = {
    val url = RobokassaApi.opStateUrl(config, invId)
    for {
      uri <- ZIO
               .fromEither(Uri.fromString(url))
               .mapError(err => RobokassaError.Protocol(s"плохой адрес: ${err.message}"))
      raw   <- send(Request[Task](method = Method.GET, uri = uri)).retry(retry)
      state <- ZIO.fromEither(RobokassaApi.opState(raw)).mapError(RobokassaError.Protocol(_))
    } yield state
  }

  override def verify(params: Map[String, String]): Boolean =
    RobokassaSignature.verifyResult(config.hash, params, config.password2)

  private def send(request: Request[Task]): Task[String] =
    client
      .run(request)
      .use { response =>
        response.bodyText.compile.toList.map(_.mkString).flatMap { raw =>
          if (response.status.code >= 500)
            ZIO.fail(RobokassaError.Transport(s"${response.status.code}: ${RobokassaError.snippet(raw)}"))
          else if (!response.status.isSuccess)
            ZIO.fail(RobokassaError.Protocol(s"${response.status.code}: ${RobokassaError.snippet(raw)}"))
          else ZIO.succeed(raw)
        }
      }
      // Сетевые сбои приходят обычными исключениями — всё, что не наша
      // собственная ошибка, считаем транспортным и даём повтору сработать.
      .mapError {
        case known: RobokassaError => known
        case other                 => RobokassaError.Transport(Option(other.getMessage).getOrElse(other.toString))
      }
}

/** Ошибки кассы, разделённые по одному признаку: есть ли смысл повторить. */
sealed abstract class RobokassaError(message: String) extends RuntimeException(message)

object RobokassaError {

  /** Касса не ответила или ответила 5xx — повтор может помочь. */
  final case class Transport(reason: String) extends RobokassaError(s"касса недоступна: $reason")

  /** Касса ответила, но не так, как мы умеем читать. */
  final case class Protocol(reason: String) extends RobokassaError(s"касса ответила неожиданно: $reason")

  private val BodySnippetLimit = 300

  /** Тела ошибок бывают HTML-страницами на десятки килобайт, а едут они в лог. */
  def snippet(body: String): String =
    if (body.length <= BodySnippetLimit) body else body.take(BodySnippetLimit) + "…"
}

object RobokassaClient {

  /** Клиент живёт весь срок приложения: ресурс привязан к области видимости
    * слоя, а не закрывается сразу после создания. */
  val live: ZLayer[RobokassaConfig, Throwable, RobokassaClient] =
    ZLayer.scoped {
      implicit val network: Network[Task] = Network.forAsync[Task]
      for {
        config <- ZIO.service[RobokassaConfig]
        client <- EmberClientBuilder
                    .default[Task]
                    .withTimeout(15.seconds)
                    .build
                    .toScopedZIO
      } yield new RobokassaClientLive(client, config): RobokassaClient
    }
}
