package server

import io.circe.Json
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.dsl.Http4sDsl
import pangea.service.donation.Donations
import zio.interop.catz._
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Касса снаружи: вебхук со статусами платежей.
  *
  * Живёт на своём пути, потому что `POST /` занят колбэком ВК: тот разбирает
  * тело как событие ВК, а всё непонятное молча кладёт в лог и отвечает `ok` —
  * нотификация банка там бы просто растворилась.
  *
  * Страниц возврата у нас нет: `SuccessURL` и `FailURL` ведут прямо в диалог с
  * сообществом, так что после оплаты игрок попадает туда, где игра и
  * продолжается, а бот через секунду пишет ему о зачислении. */
final class PaymentRoutes(donations: Donations) {

  private val dsl = Http4sDsl[Task]
  import dsl._

  val routes: HttpRoutes[Task] = HttpRoutes.of[Task] {

    case req @ POST -> Root / "pay" / "tbank" / "notify" =>
      (for {
        json   <- req.as[Json]
        now    <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
        result <- donations.notified(json, now)
        resp <- result match {
          case Donations.Notified.BadSignature =>
            Forbidden("bad token")

          case Donations.Notified.Granted(payment) =>
            // Письмо игроку не должно задерживать ответ банку: он ждёт его 10
            // секунд, а дублоны к этому моменту уже начислены.
            donations.announce(payment).forkDaemon *> accepted

          // Заказ не найден, сумма не та, тело не объект — повтор это не
          // вылечит, а слать его банк будет месяц. Отвечаем `OK` и шумим в лог.
          case Donations.Notified.Ignored(_) => accepted
          case Donations.Notified.Accepted   => accepted
        }
      } yield resp).catchAll { err =>
        // Своя поломка (упала база, например) — единственный случай, когда
        // повтор нотификации действительно нужен.
        ZIO.logError(s"donation: нотификация не обработана: ${err.getMessage}") *>
          InternalServerError("retry later")
      }
  }

  /** Ответ, которого ждёт банк: HTTP 200 и тело ровно `OK`, заглавными, без
    * разметки. Иначе нотификация считается недоставленной, и банк будет
    * переотправлять её сутки, а потом месяц раз в день. */
  private def accepted = Ok("OK")
}
