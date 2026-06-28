package pangea.dao.barrel

import doobie.util.transactor
import pangea.model.barrel.Barrel
import pangea.model.hero.HeroId
import zio.{Task, ZLayer}

trait BarrelDao {
  /** Гарантирует наличие записи бочки для героя и возвращает её. */
  def getOrCreate(heroId: HeroId): Task[Barrel]
  def update(barrel: Barrel): Task[Unit]
}

object BarrelDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, BarrelDao] =
    ZLayer.fromFunction(new BarrelDaoLive(_))
}
