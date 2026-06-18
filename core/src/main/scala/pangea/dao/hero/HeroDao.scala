package pangea.dao.hero

import doobie.util.transactor
import io.circe.Json
import pangea.model.hero.{Equipment, Hero, HeroId}
import pangea.model.stats.FightStats
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.user.UserId
import zio.{Task, ZLayer}

trait HeroDao {
  def getHeroByUserId(userId: UserId): Task[Option[Hero]]
  def insertHero(hero: Hero): Task[HeroId]
  def updateRace(userId: UserId, race: Race): Task[Unit]
  def updateState(userId: UserId, potentiallyNewState: StateType): Task[Unit]

  def updateDungeonLevel(userId: UserId, level: Int): Task[Unit]
  def updateFightStats(userId: UserId, stats: FightStats): Task[Unit]
  def updateExpAndLevel(userId: UserId, exp: Long, lvl: Long, upgradePoints: Long): Task[Unit]
  def updateGold(userId: UserId, gold: Long): Task[Unit]
  def updateTrauma(userId: UserId, traumaUntil: Option[Long], traumaName: Option[String]): Task[Unit]
  def updateFlaskCharges(userId: UserId, charges: Int): Task[Unit]
  def updateBaseStats(userId: UserId, stats: pangea.model.stats.BaseStats): Task[Unit]
  def updateEquipmentAndFightStats(userId: UserId, eq: Equipment, stats: FightStats): Task[Unit]

  def writeActiveBattle(userId: UserId, data: Json): Task[Unit]
  def readActiveBattle(userId: UserId): Task[Option[Json]]
  def clearActiveBattle(userId: UserId): Task[Unit]

  // Live scene state: temporary scratchpad overwritten on each scene transition
  def writeSceneData(userId: UserId, data: Json): Task[Unit]
  def readSceneData(userId: UserId): Task[Option[Json]]
}

object HeroDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, HeroDaoLive] =
    ZLayer.fromFunction(new HeroDaoLive(_))
}
