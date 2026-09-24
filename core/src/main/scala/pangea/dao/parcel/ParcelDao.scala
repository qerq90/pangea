package pangea.dao.parcel

import doobie.implicits._
import doobie.util.transactor
import doobie.util.transactor.Transactor
import pangea.model.hero.HeroId
import pangea.model.item.Item
import pangea.model.item.Item.meta
import pangea.model.parcel.Parcel
import zio.interop.catz._
import zio.{Task, ZLayer}

/** Посылки от игрока к игроку: лежат тут, пока получатель не заберёт их —
 *  сперва в банковскую ячейку сами, а что не влезло, руками с почты. */
trait ParcelDao {
  def add(heroId: HeroId, fromName: String, item: Item, now: Long): Task[Parcel]
  def list(heroId: HeroId, limit: Long): Task[List[Parcel]]
  def count(heroId: HeroId): Task[Long]
  /** Забрать посылку: `false` — её уже забрали. */
  def claim(id: Long, heroId: HeroId): Task[Boolean]
}

class ParcelDaoLive(xa: Transactor[Task]) extends ParcelDao {

  private val columns = fr"id, hero_id, from_name, item, sent_at"

  override def add(heroId: HeroId, fromName: String, item: Item, now: Long): Task[Parcel] =
    sql"""insert into parcels(hero_id, from_name, item, sent_at)
          values(${heroId.value}, $fromName, $item, $now)"""
      .update.withUniqueGeneratedKeys[Long]("id").transact(xa)
      .map(id => Parcel(id, heroId, fromName, item, now))

  override def list(heroId: HeroId, limit: Long): Task[List[Parcel]] =
    (fr"select" ++ columns ++ fr"from parcels where hero_id = ${heroId.value} order by id limit $limit")
      .query[Parcel].to[List].transact(xa)

  override def count(heroId: HeroId): Task[Long] =
    sql"select count(*) from parcels where hero_id = ${heroId.value}"
      .query[Long].unique.transact(xa)

  // Забираем удалением: одна и та же посылка не достанется дважды.
  override def claim(id: Long, heroId: HeroId): Task[Boolean] =
    sql"delete from parcels where id = $id and hero_id = ${heroId.value}"
      .update.run.transact(xa).map(_ == 1)
}

object ParcelDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, ParcelDao] =
    ZLayer.fromFunction(new ParcelDaoLive(_))
}
