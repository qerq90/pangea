package server

import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, UrlForm}
import pangea.service.donation.Donations
import zio.interop.catz._
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Касса снаружи: уведомление ResultURL об оплате.
  *
  * Живёт на своём пути, потому что `POST /` занят колбэком ВК: тот разбирает
  * тело как событие ВК, а всё непонятное молча кладёт в лог и отвечает `ok` —
  * уведомление кассы там бы просто растворилось.
  *
  * Адрес и метод (POST) задаются в технических настройках магазина в кабинете
  * Робокассы. Страниц возврата у нас нет: SuccessURL и FailURL там же ведут
  * прямо в сообщество, а бот через секунду пишет игроку о зачислении. */
final class PaymentRoutes(donations: Donations) {

  private val dsl = Http4sDsl[Task]
  import dsl._

  val routes: HttpRoutes[Task] = HttpRoutes.of[Task] {

    case req @ POST -> Root / "pay" / "robokassa" / "result" =>
      (for {
        form  <- req.as[UrlForm]
        params = form.values.flatMap { case (key, values) => values.headOption.map(key -> _) }
        now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
        result <- donations.notified(params, now)
        resp <- result match {
          case Donations.Notified.BadSignature =>
            BadRequest("bad sign")

          case Donations.Notified.Granted(payment) =>
            // Письмо игроку не должно задерживать ответ кассе: дублоны к этому
            // моменту уже начислены.
            donations.announce(payment).forkDaemon *> accepted(params)

          // Заказ не найден, сумма не та — повтор это не вылечит. Отвечаем, как
          // ждёт касса, и шумим в лог.
          case Donations.Notified.Ignored(_) => accepted(params)
          case Donations.Notified.Accepted   => accepted(params)
        }
      } yield resp).catchAll { err =>
        // Своя поломка (упала база, например) — единственный случай, когда
        // повтор уведомления действительно нужен.
        ZIO.logError(s"donation: уведомление не обработано: ${err.getMessage}") *>
          InternalServerError("retry later")
      }
  }

  /** Ответ, которого ждёт касса: `OK` и номер счёта вплотную, например `OK42`.
    * Иначе уведомление считается недоставленным. */
  private def accepted(params: Map[String, String]) = Ok(s"OK${params.getOrElse("InvId", "")}")
}
