package pangea.dao.arena

import doobie.implicits._
import doobie.postgres.circe.json.implicits._
import doobie.util.transactor.Transactor
import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.model.arena.{ArenaFight, ArenaStatus}
import pangea.model.user.UserId
import zio.Task
import zio.interop.catz._

class ArenaDaoLive(xa: Transactor[Task]) extends ArenaDao {

  private val waitingStatus: ArenaStatus = ArenaStatus.Waiting

  /** Строка без разбора: id и код берём из колонок, остальное — из data,
    * чтобы новое поле боя не требовало миграции. */
  private def parse(id: Long, code: String, data: Json): Option[ArenaFight] =
    data.as[ArenaFight].toOption.map(_.copy(id = id, code = code))

  override def create(fight: ArenaFight, now: Long): Task[Option[ArenaFight]] =
    sql"""insert into arena_fights(code, status, a_user_id, b_user_id, data, updated_at)
          values(${fight.code}, ${fight.status.entryName}, ${fight.a.userId.value},
                 ${fight.b.map(_.userId.value)}, ${fight.asJson}, $now)
          on conflict (code) do nothing"""
      .update
      .withGeneratedKeys[Long]("id")
      .compile
      .last
      .transact(xa)
      .map(_.map(id => fight.copy(id = id)))

  override def byId(id: Long): Task[Option[ArenaFight]] =
    sql"select id, code, data from arena_fights where id = $id"
      .query[(Long, String, Json)].option.transact(xa)
      .map(_.flatMap { case (i, c, d) => parse(i, c, d) })

  override def byCode(code: String): Task[Option[ArenaFight]] =
    sql"select id, code, data from arena_fights where code = $code"
      .query[(Long, String, Json)].option.transact(xa)
      .map(_.flatMap { case (i, c, d) => parse(i, c, d) })

  override def ofUser(userId: UserId): Task[Option[ArenaFight]] =
    sql"""select id, code, data from arena_fights
          where a_user_id = ${userId.value} or b_user_id = ${userId.value}
          order by updated_at desc limit 1"""
      .query[(Long, String, Json)].option.transact(xa)
      .map(_.flatMap { case (i, c, d) => parse(i, c, d) })

  override def waiting(limit: Int): Task[List[ArenaFight]] =
    sql"""select id, code, data from arena_fights
          where status = ${waitingStatus.entryName}
          order by updated_at limit $limit"""
      .query[(Long, String, Json)].to[List].transact(xa)
      .map(_.flatMap { case (i, c, d) => parse(i, c, d) })

  override def update(fight: ArenaFight, now: Long): Task[Unit] =
    sql"""update arena_fights
          set status = ${fight.status.entryName}, b_user_id = ${fight.b.map(_.userId.value)},
              data = ${fight.asJson}, updated_at = $now
          where id = ${fight.id}"""
      .update.run.transact(xa).unit

  override def delete(id: Long): Task[Unit] =
    sql"delete from arena_fights where id = $id".update.run.transact(xa).unit
}
