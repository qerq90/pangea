package pangea.test

import io.circe.Json
import pangea.dao.hero.HeroDao
import pangea.model.hero.{Equipment, Hero, HeroId}
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.UserId
import zio.{Ref, Task, ZIO}

class TestHeroDao(
  raceRef:      Ref[Map[UserId, Race]],
  heroRef:      Ref[Map[UserId, Hero]],
  sceneDataRef: Ref[Map[UserId, Json]],
  battleRef:    Ref[Map[UserId, Json]],
  merchantRef:  Ref[Map[UserId, Json]],
  questRef:     Ref[Map[UserId, Json]],
  returnRef:    Ref[Map[UserId, StateType]]
) extends HeroDao {

  def getHeroByUserId(userId: UserId): Task[Option[Hero]] = heroRef.get.map(_.get(userId))

  def insertHero(hero: Hero): Task[HeroId] =
    heroRef.update(_.updated(hero.userId, hero)).as(hero.id)

  def updateRace(userId: UserId, race: Race): Task[Unit] =
    raceRef.update(_.updated(userId, race))

  def updateState(userId: UserId, state: StateType): Task[Unit] = ZIO.unit

  def updateDungeonLevel(userId: UserId, level: Int): Task[Unit] =
    heroRef.update(m => m.get(userId).fold(m)(h => m.updated(userId, h.copy(dungeonLevel = level))))

  def updateMaxDungeonLevel(userId: UserId, level: Int): Task[Unit] =
    heroRef.update(m => m.get(userId).fold(m)(h => m.updated(userId, h.copy(maxDungeonLevel = level))))

  def updateFightStats(userId: UserId, stats: FightStats): Task[Unit] =
    heroRef.update(m => m.get(userId).fold(m)(h => m.updated(userId, h.copy(fightStats = stats))))

  def updateExpAndLevel(userId: UserId, exp: Long, lvl: Long, upgradePoints: Long): Task[Unit] =
    heroRef.update(m => m.get(userId).fold(m)(h =>
      m.updated(userId, h.copy(exp = exp, lvl = lvl, upgradePoints = upgradePoints))))

  def updateGold(userId: UserId, gold: Long): Task[Unit] =
    heroRef.update(m => m.get(userId).fold(m)(h => m.updated(userId, h.copy(gold = gold))))

  def updateGuildReputation(userId: UserId, value: Long): Task[Unit] =
    heroRef.update(m => m.get(userId).fold(m)(h => m.updated(userId, h.copy(guildReputation = value))))

  def updateTrauma(userId: UserId, traumaUntil: Option[Long], traumaNames: List[String]): Task[Unit] =
    heroRef.update(m => m.get(userId).fold(m)(h => m.updated(userId, h.copy(traumaUntil = traumaUntil, traumaNames = traumaNames))))

  def updateBaseStats(userId: UserId, stats: pangea.model.stats.BaseStats): Task[Unit] =
    heroRef.update(m => m.get(userId).fold(m)(h => m.updated(userId, h.copy(baseStats = stats))))

  def updateEquipment(userId: UserId, eq: Equipment): Task[Unit] =
    heroRef.update(m => m.get(userId).fold(m)(h => m.updated(userId, h.copy(equipment = eq))))

  def updateEquipmentAndFightStats(userId: UserId, eq: Equipment, stats: FightStats): Task[Unit] =
    heroRef.update(m => m.get(userId).fold(m)(h => m.updated(userId, h.copy(equipment = eq, fightStats = stats))))

  def writeActiveBattle(userId: UserId, data: Json): Task[Unit] =
    battleRef.update(_.updated(userId, data))

  def readActiveBattle(userId: UserId): Task[Option[Json]] =
    battleRef.get.map(_.get(userId))

  def clearActiveBattle(userId: UserId): Task[Unit] =
    battleRef.update(_ - userId)

  def writeSceneData(userId: UserId, data: Json): Task[Unit] =
    sceneDataRef.update(_.updated(userId, data))

  def readSceneData(userId: UserId): Task[Option[Json]] =
    sceneDataRef.get.map(_.get(userId))

  def writeMerchantData(userId: UserId, data: Json): Task[Unit] =
    merchantRef.update(_.updated(userId, data))

  def readMerchantData(userId: UserId): Task[Option[Json]] =
    merchantRef.get.map(_.get(userId))

  def writeQuestData(userId: UserId, data: Json): Task[Unit] =
    questRef.update(_.updated(userId, data))

  def readQuestData(userId: UserId): Task[Option[Json]] =
    questRef.get.map(_.get(userId))

  def writeReturnState(userId: UserId, state: Option[StateType]): Task[Unit] =
    returnRef.update(m => state.fold(m - userId)(s => m.updated(userId, s)))

  def readReturnState(userId: UserId): Task[Option[StateType]] =
    returnRef.get.map(_.get(userId))

  def raceSnapshot: Task[Map[UserId, Race]] = raceRef.get
}

object TestHeroDao {
  def make: Task[TestHeroDao] =
    for {
      raceRef      <- Ref.make(Map.empty[UserId, Race])
      heroRef      <- Ref.make(Map.empty[UserId, Hero])
      sceneDataRef <- Ref.make(Map.empty[UserId, Json])
      battleRef    <- Ref.make(Map.empty[UserId, Json])
      merchantRef  <- Ref.make(Map.empty[UserId, Json])
      questRef     <- Ref.make(Map.empty[UserId, Json])
      returnRef    <- Ref.make(Map.empty[UserId, StateType])
    } yield new TestHeroDao(raceRef, heroRef, sceneDataRef, battleRef, merchantRef, questRef, returnRef)

  def withHero(userId: UserId, hero: Hero): Task[TestHeroDao] =
    for {
      raceRef      <- Ref.make(Map.empty[UserId, Race])
      heroRef      <- Ref.make(Map(userId -> hero))
      sceneDataRef <- Ref.make(Map.empty[UserId, Json])
      battleRef    <- Ref.make(Map.empty[UserId, Json])
      merchantRef  <- Ref.make(Map.empty[UserId, Json])
      questRef     <- Ref.make(Map.empty[UserId, Json])
      returnRef    <- Ref.make(Map.empty[UserId, StateType])
    } yield new TestHeroDao(raceRef, heroRef, sceneDataRef, battleRef, merchantRef, questRef, returnRef)
}
