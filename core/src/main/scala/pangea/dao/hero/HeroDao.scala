package pangea.dao.hero

import doobie.util.transactor
import pangea.model.hero.{Hero, HeroId}
import pangea.model.monster.Race
import pangea.model.user.UserId
import zio.{Task, ZLayer}

trait HeroDao {
  def getHeroByUserId(userId: UserId): Task[Option[Hero]]
  def insertHero(hero: Hero): Task[HeroId]
  def updateRace(userId: UserId, race: Race): Task[Unit]
}

object HeroDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, HeroDaoLive] =
    ZLayer.fromFunction(new HeroDaoLive(_))
}
