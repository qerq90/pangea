package pangea.dao.admin

import doobie.implicits._
import doobie.util.transactor
import pangea.model.hero.Equipment
import pangea.model.item.ItemSet
import zio.interop.catz._
import zio.{Task, ZLayer}

/** Сводка по серверу для админ-панели.
  *
  * @param heroes      сколько героев заведено всего
  * @param active24h   сколько игроков заходило за сутки (см. `users.last_seen_at`)
  * @param active7d    то же за неделю
  * @param heroSilver  серебро на руках у героев
  * @param vaultSilver серебро, лежащее в банковских ячейках
  * @param doubloons   дублоны на руках
  * @param sets        кто в наборах: набор, сколько его предметов надето, сколько таких героев
  */
final case class AdminStats(
  heroes:      Long,
  active24h:   Long,
  active7d:    Long,
  heroSilver:  Long,
  vaultSilver: Long,
  doubloons:   Long,
  sets:        List[AdminStats.SetRow]
) {
  def silverTotal: Long = heroSilver + vaultSilver
}

object AdminStats {
  /** «54 игрока держат набор Охотника на шести предметах». */
  final case class SetRow(set: ItemSet, worn: Int, heroes: Long)

  val empty: AdminStats = AdminStats(0L, 0L, 0L, 0L, 0L, 0L, Nil)

  /** Кто сколько носит: по каждому набору — сколько героев держат столько-то
    * его предметов. Один предмет набором не считается: это ещё не
    * «придерживаться». Считаем в памяти, а не в SQL: экипировка лежит в JSONB
    * одним объектом, и разбирать её запросом было бы куда сложнее, чем взять
    * готовую [[Equipment.setCounts]]. */
  def setRows(all: List[Equipment], minWorn: Int): List[SetRow] =
    all.flatMap(_.setCounts.filter(_._2 >= minWorn))
      .groupBy(identity)
      .map { case ((set, worn), xs) => SetRow(set, worn, xs.size.toLong) }
      .toList
      .sortBy(r => (r.set.entryName, -r.worn))
}

trait AdminDao {
  def stats: Task[AdminStats]
}

/** Запросы админ-панели. Героев на сервере немного, а панель открывают руками
  * и редко, поэтому экипировку читаем целиком и считаем наборы в памяти
  * (см. [[AdminStats.setRows]]). */
class AdminDaoLive(xa: transactor.Transactor[Task]) extends AdminDao {

  override def stats: Task[AdminStats] =
    for {
      heroes    <- sql"select count(*) from heroes".query[Long].unique.transact(xa)
      day       <- activeSince("24 hours")
      week      <- activeSince("7 days")
      money     <- sql"select coalesce(sum(silver), 0), coalesce(sum(doubloons), 0) from heroes"
                     .query[(Long, Long)].unique.transact(xa)
      inVaults  <- sql"select coalesce(sum(silver), 0) from bank_vaults".query[Long].unique.transact(xa)
      equipment <- sql"select equipment from heroes".query[Equipment].to[List].transact(xa)
    } yield AdminStats(
      heroes      = heroes,
      active24h   = day,
      active7d    = week,
      heroSilver  = money._1,
      vaultSilver = inVaults,
      doubloons   = money._2,
      sets        = AdminStats.setRows(equipment, AdminDao.MinWorn))

  /** Сколько игроков заходило за этот срок. `interval` подставляется не
    * параметром, а строкой — но строку задаём здесь сами, снаружи она не
    * приходит. */
  private def activeSince(interval: String): Task[Long] =
    doobie.Fragment.const(s"select count(*) from users where last_seen_at > now() - interval '$interval'")
      .query[Long].unique.transact(xa)

}

object AdminDao {
  /** Со скольких надетых предметов считаем, что герой держит набор. */
  val MinWorn: Int = 2

  val live: ZLayer[transactor.Transactor[Task], Nothing, AdminDao] =
    ZLayer.fromFunction(new AdminDaoLive(_))
}
