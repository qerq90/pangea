package pangea.dao.hero

import doobie.implicits.toSqlInterpolator
import doobie.util.transactor.Transactor
import doobie.implicits._
import doobie.postgres.circe.json.implicits._
import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.TraumaInstances._
import pangea.model.hero.{Equipment, MasterHornBoosts}
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

  override def updateMaxDungeonLevel(userId: UserId, level: Int): Task[Unit] =
    Queries
      .updateMaxDungeonLevel(userId, level)
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

  override def updateDoubloons(userId: UserId, doubloons: Long): Task[Unit] =
    Queries.updateDoubloons(userId, doubloons).update.run.transact(xa).unit

  override def updateGuildReputation(userId: UserId, value: Long): Task[Unit] =
    Queries.updateGuildReputation(userId, value).update.run.transact(xa).unit

  override def updateMasterHornBoosts(userId: UserId, boosts: MasterHornBoosts): Task[Unit] =
    Queries.updateMasterHornBoosts(userId, boosts).update.run.transact(xa).unit

  override def updateTrauma(userId: UserId, traumaUntil: Option[Long], traumaNames: List[String]): Task[Unit] =
    Queries.updateTrauma(userId, traumaUntil, traumaNames).update.run.transact(xa).unit

  override def updateStatBoosts(userId: UserId, boosts: pangea.model.stats.StatBoosts): Task[Unit] =
    Queries.updateStatBoosts(userId, boosts).update.run.transact(xa).unit

  override def updateBaseStats(userId: UserId, stats: pangea.model.stats.BaseStats): Task[Unit] =
    Queries.updateBaseStats(userId, stats).update.run.transact(xa).unit

  override def updateEquipment(userId: UserId, eq: Equipment): Task[Unit] =
    Queries.updateEquipment(userId, eq).update.run.transact(xa).unit

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

  override def writeMerchantData(userId: UserId, data: Json): Task[Unit] =
    Queries.writeMerchantData(userId, data).update.run.transact(xa).unit

  override def readMerchantData(userId: UserId): Task[Option[Json]] =
    Queries.readMerchantData(userId).query[Option[Json]].unique.transact(xa)

  override def writeQuestData(userId: UserId, data: Json): Task[Unit] =
    Queries.writeQuestData(userId, data).update.run.transact(xa).unit

  override def readQuestData(userId: UserId): Task[Option[Json]] =
    Queries.readQuestData(userId).query[Option[Json]].unique.transact(xa)

  override def writeGustavoData(userId: UserId, data: Json): Task[Unit] =
    Queries.writeGustavoData(userId, data).update.run.transact(xa).unit

  override def readGustavoData(userId: UserId): Task[Option[Json]] =
    Queries.readGustavoData(userId).query[Option[Json]].unique.transact(xa)

  override def writeReturnState(userId: UserId, state: Option[StateType]): Task[Unit] =
    Queries.writeReturnState(userId, state).update.run.transact(xa).unit

  override def readReturnState(userId: UserId): Task[Option[StateType]] =
    Queries.readReturnState(userId).query[Option[StateType]].unique.transact(xa)

}
