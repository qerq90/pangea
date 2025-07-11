package pangea.dao.hero

import doobie.util.transactor
import pangea.model.hero.{Hero, HeroId}
import pangea.model.user.UserId
import zio.{Task, ZLayer}

trait HeroDao {
  def getHeroByUserId(userId: UserId): Task[Option[Hero]]
  def insertHero(hero: Hero): Task[HeroId]
}

object HeroDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, HeroDaoLive] =
    ZLayer.fromFunction(new HeroDaoLive(_))
}
