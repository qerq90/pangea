package pangea.dao.hero

import doobie.implicits.toSqlInterpolator
import doobie.util.transactor.Transactor
import doobie.implicits._
import doobie.postgres.circe.json.implicits._
import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.model.hero.Equipment
import pangea.model.stats.FightStats
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

  override def updateDungeonLevel(userId: UserId, level: Int): Task[Unit] =
    Queries
      .updateDungeonLevel(userId, level)
      .update
      .run
      .transact(xa)
      .unit

  override def updateFightStats(userId: UserId, stats: FightStats): Task[Unit] =
    Queries.updateFightStats(userId, stats).update.run.transact(xa).unit

  override def updateExpAndLevel(userId: UserId, exp: Long, lvl: Long, upgradePoints: Long): Task[Unit] =
    Queries.updateExpAndLevel(userId, exp, lvl, upgradePoints).update.run.transact(xa).unit

  override def updateGold(userId: UserId, gold: Long): Task[Unit] =
    Queries.updateGold(userId, gold).update.run.transact(xa).unit

  override def updateTrauma(userId: UserId, traumaUntil: Option[Long], traumaName: Option[String]): Task[Unit] =
    Queries.updateTrauma(userId, traumaUntil, traumaName).update.run.transact(xa).unit

  override def updateFlaskCharges(userId: UserId, charges: Int): Task[Unit] =
    Queries.updateFlaskCharges(userId, charges).update.run.transact(xa).unit

  override def updateBaseStats(userId: UserId, stats: pangea.model.stats.BaseStats): Task[Unit] =
    Queries.updateBaseStats(userId, stats).update.run.transact(xa).unit

  override def updateEquipmentAndFightStats(userId: UserId, eq: Equipment, stats: FightStats): Task[Unit] =
    Queries.updateEquipmentAndFightStats(userId, eq, stats).update.run.transact(xa).unit

  override def writeActiveBattle(userId: UserId, data: Json): Task[Unit] =
    Queries.writeActiveBattle(userId, data).update.run.transact(xa).unit

  override def readActiveBattle(userId: UserId): Task[Option[Json]] =
    Queries.readActiveBattle(userId).query[Option[Json]].unique.transact(xa)

  override def clearActiveBattle(userId: UserId): Task[Unit] =
    Queries.clearActiveBattle(userId).update.run.transact(xa).unit

  override def writeSceneData(userId: UserId, data: Json): Task[Unit] =
    Queries
      .writeSceneData(userId, data)
      .update
      .run
      .transact(xa)
      .unit

  override def readSceneData(userId: UserId): Task[Option[Json]] =
    Queries
      .readSceneData(userId)
      .query[Option[Json]]
      .unique
      .transact(xa)

}
