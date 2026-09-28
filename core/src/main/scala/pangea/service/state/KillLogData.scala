package pangea.service.state

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.model.hero.KillLog
import pangea.model.user.UserId
import zio.Task

/** Журнал убийств героя: кого и сколько он положил и кому за это уже
  * прилетело. Лежит в `heroes.kill_log`; читают его бой (пополняет) и
  * лабиринт (смотрит, не пора ли расе прийти за расплатой). */
object KillLogData {

  /** Через сколько убитых одной расы она приходит мстить. Счёт не
    * обнуляется: следующая расплата — ещё через столько же. */
  val RevengeEvery: Long = 250L

  def read(heroDao: HeroDao, userId: UserId): Task[KillLog] =
    heroDao.readKillLog(userId).map(_.flatMap(_.as[KillLog].toOption).getOrElse(KillLog.empty))

  def write(heroDao: HeroDao, userId: UserId, log: KillLog): Task[Unit] =
    heroDao.writeKillLog(userId, log.asJson)

  /** Записать павших этого боя. */
  def add(heroDao: HeroDao, userId: UserId, races: List[String]): Task[Unit] =
    read(heroDao, userId).flatMap(log => write(heroDao, userId, log.add(races)))
}
