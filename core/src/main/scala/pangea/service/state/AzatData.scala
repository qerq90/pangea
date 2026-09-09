package pangea.service.state

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.model.hero.AzatState
import pangea.model.user.UserId
import zio.{Task, ZIO}

/**
 * Единая точка чтения и записи `azat_data`. Через неё же идёт ЛЕНИВОЕ суточное
 * начисление мгновенных отдыхов благословения: считать полуночи фоном не на чем
 * (планировщик умеет только задачи, привязанные к конкретному состоянию героя),
 * поэтому недостающие порции доначисляются при первом же обращении — и сразу
 * сохраняются, чтобы не начислиться повторно.
 *
 * Все состояния читают состояние храма отсюда: тогда отдыхи «дозреют» в любом
 * экране, где благословение вообще как-то участвует.
 */
object AzatData {

  /** Прочитать состояние храма, доначислив всё, что задолжало благословение. */
  def load(heroDao: HeroDao, userId: UserId, nowMs: Long): Task[AzatState] =
    for {
      stored  <- read(heroDao, userId)
      updated  = stored.withDailyRests(nowMs)
      _       <- ZIO.when(updated != stored)(save(heroDao, userId, updated))
    } yield updated

  /** Прочитать как есть, без начислений — для мест, где важен голый снимок. */
  def read(heroDao: HeroDao, userId: UserId): Task[AzatState] =
    heroDao.readAzatData(userId).map(_.flatMap(_.as[AzatState].toOption).getOrElse(AzatState.empty))

  def save(heroDao: HeroDao, userId: UserId, azat: AzatState): Task[Unit] =
    heroDao.writeAzatData(userId, azat.asJson)
}
