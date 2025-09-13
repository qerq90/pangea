package pangea.dao.hero

import doobie.implicits.toSqlInterpolator
import doobie.util.transactor.Transactor
import doobie.implicits._
import doobie.postgres.circe.json.implicits._
import io.circe.syntax.EncoderOps
import pangea.dao.hero.queries.Queries
import pangea.model.hero.{Hero, HeroId}
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.user.UserId
import zio.Task
import zio.interop.catz._

class HeroDaoLive(xa: Transactor[Task]) extends HeroDao {

  override def getHeroByUserId(userId: UserId): Task[Option[Hero]] =
    Queries
      .getHeroByUserId(userId)
      .query[Hero]
      .option
      .transact(xa)

  override def insertHero(hero: Hero): Task[HeroId] =
    Queries
      .insert(hero)
      .update
      .withUniqueGeneratedKeys[HeroId]("id")
      .transact(xa)

  override def updateRace(userId: UserId, race: Race): Task[Unit] =
    Queries
      .updateRace(userId, race)
      .update
      .run
      .transact(xa)
      .unit

  override def updateState(
      userId: UserId,
      newState: StateType
  ): Task[Unit] =
    Queries
      .updateState(userId, newState)
      .update
      .run
      .transact(xa)
      .unit

}
