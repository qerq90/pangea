package pangea.test

import pangea.dao.parcel.ParcelDao
import pangea.model.hero.HeroId
import pangea.model.item.Item
import pangea.model.parcel.Parcel
import zio.{Task, ZIO}

/** Посылки в памяти: те же правила, что в проде, включая «забрать можно один
  * раз» — второй `claim` той же посылки вернёт `false`. */
class TestParcelDao(private var parcels: List[Parcel] = Nil) extends ParcelDao {

  private var nextId: Long = parcels.map(_.id).maxOption.getOrElse(0L) + 1L

  def add(heroId: HeroId, fromName: String, item: Item, now: Long): Task[Parcel] =
    ZIO.succeed {
      val parcel = Parcel(nextId, heroId, fromName, item, now)
      nextId += 1L
      parcels = parcels :+ parcel
      parcel
    }

  def list(heroId: HeroId, limit: Long): Task[List[Parcel]] =
    ZIO.succeed(parcels.filter(_.heroId == heroId).sortBy(_.id).take(limit.toInt))

  def count(heroId: HeroId): Task[Long] = ZIO.succeed(parcels.count(_.heroId == heroId).toLong)

  def claim(id: Long, heroId: HeroId): Task[Boolean] =
    ZIO.succeed {
      val found = parcels.exists(p => p.id == id && p.heroId == heroId)
      if (found) parcels = parcels.filterNot(_.id == id)
      found
    }

  def snapshot: List[Parcel] = parcels
}

object TestParcelDao {
  def empty: TestParcelDao = new TestParcelDao()
}
