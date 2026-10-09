package pangea.client.tbank

import fs2.io.net.Network
import io.circe.parser.parse
import io.circe.{Json, JsonObject}
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.{Method, Request, Uri}
import zio.interop.catz._
import zio.{Duration, Schedule, Task, ZIO, ZLayer}

import scala.concurrent.duration.DurationInt

/** Касса Т-Банка. Знает про провод и про подпись; про дублоны и игроков — нет. */
trait TBankClient {

  def init(request: InitRequest, dueAtMs: Long): Task[InitResponse]

  def getState(paymentId: String): Task[GetStateResponse]

  /** Подпись входящей нотификации. Без этой проверки «оплатить» мог бы любой,
    * кто знает адрес вебхука. */
  def verify(body: JsonObject): Boolean
}

class TBankClientLive(client: Client[Task], config: TBankConfig) extends TBankClient {

  /** Повторяем только то, чего банк заведомо не обработал: обрыв связи и 5xx.
    * Ответ с телом — это обработанный запрос, второй раз его слать нельзя. */
  private val retry: Schedule[Any, Throwable, Any] =
    (Schedule.recurs(2) && Schedule.exponential(Duration.fromMillis(250)))
      .whileInput[Throwable](_.isInstanceOf[TBankError.Transport])

  override def init(request: InitRequest, dueAtMs: Long): Task[InitResponse] =
    post(config.initUrl, TBankApi.initBody(config, request, dueAtMs)).map(TBankApi.initResponse)

  override def getState(paymentId: String): Task[GetStateResponse] =
    post(config.getStateUrl, TBankApi.getStateBody(config, paymentId)).map(TBankApi.getStateResponse)

  override def verify(body: JsonObject): Boolean =
    TBankToken.verify(body, config.password)

  private def post(url: String, body: JsonObject): Task[Json] =
    for {
      uri <- ZIO
               .fromEither(Uri.fromString(url))
               .mapError(err => TBankError.Protocol(s"плохой адрес '$url': ${err.message}"))
      request = Request[Task](method = Method.POST, uri = uri).withEntity(Json.fromJsonObject(body))
      json <- send(request).retry(retry)
    } yield json

  private def send(request: Request[Task]): Task[Json] =
    client
      .run(request)
      .use { response =>
        response.bodyText.compile.toList.map(_.mkString).flatMap { raw =>
          if (response.status.code >= 500)
            ZIO.fail(TBankError.Transport(s"${response.status.code}: ${TBankError.snippet(raw)}"))
          else if (!response.status.isSuccess)
            ZIO.fail(TBankError.Protocol(s"${response.status.code}: ${TBankError.snippet(raw)}"))
          else
            ZIO
              .fromEither(parse(raw))
              .mapError(_ => TBankError.Protocol(s"ответ не JSON: ${TBankError.snippet(raw)}"))
        }
      }
      // Сетевые сбои приходят обычными исключениями — всё, что не наша
      // собственная ошибка, считаем транспортным и даём повтору сработать.
      .mapError {
        case known: TBankError => known
        case other             => TBankError.Transport(Option(other.getMessage).getOrElse(other.toString))
      }
}

/** Ошибки кассы, разделённые по одному признаку: можно ли повторить запрос,
  * не рискуя вторым списанием. */
sealed abstract class TBankError(message: String) extends RuntimeException(message)

object TBankError {

  /** Запрос до банка не дошёл или не был обработан — повтор безопасен. */
  final case class Transport(reason: String) extends TBankError(s"касса недоступна: $reason")

  /** Банк ответил, но не так, как мы умеем читать. Повторять нельзя. */
  final case class Protocol(reason: String) extends TBankError(s"касса ответила неожиданно: $reason")

  private val BodySnippetLimit = 300

  /** Тела ошибок бывают HTML-страницами на десятки килобайт, а едут они в лог. */
  def snippet(body: String): String =
    if (body.length <= BodySnippetLimit) body else body.take(BodySnippetLimit) + "…"
}

object TBankClient {

  /** Клиент живёт весь срок приложения: в отличие от [[pangea.client.Client]]
    * ресурс привязан к области видимости слоя, а не закрывается сразу после
    * создания. */
  val live: ZLayer[TBankConfig, Throwable, TBankClient] =
    ZLayer.scoped {
      implicit val network: Network[Task] = Network.forAsync[Task]
      for {
        config <- ZIO.service[TBankConfig]
        client <- EmberClientBuilder
                    .default[Task]
                    .withTimeout(15.seconds)
                    .build
                    .toScopedZIO
      } yield new TBankClientLive(client, config): TBankClient
    }
}
