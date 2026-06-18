package pangea.dao.hero.queries

import doobie.implicits.toSqlInterpolator
import doobie.util.fragment.Fragment
import doobie.implicits._
import doobie.postgres.circe.json.implicits._
import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.model.hero.{Equipment, Hero}
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.UserId

object Queries {
  private val fields: Fragment =
    sql"id, user_id, state, lvl, exp, upgrade_points, race, base_stats, fight_stats, equipment, dungeon_level, gold, trauma_until, trauma_name, flask_charges"
  private val fieldInsert: Fragment =
    sql"user_id, state, lvl, exp, upgrade_points, race, base_stats, fight_stats, equipment, dungeon_level, gold, trauma_until, trauma_name, flask_charges"
  private val tableName: Fragment = sql"heroes"

  private val selectAll = fr"select $fields from $tableName"

  def getHeroByUserId(userId: UserId): Fragment =
    selectAll ++ sql"where user_id = $userId"

  def insert(hero: Hero): Fragment =
    sql"insert into $tableName($fieldInsert) values(${hero.userId}, ${hero.state}, ${hero.lvl}, ${hero.exp}, ${hero.upgradePoints}, ${hero.race}, ${hero.baseStats.asJson}, ${hero.fightStats.asJson}, ${hero.equipment.asJson}, ${hero.dungeonLevel}, ${hero.gold}, ${hero.traumaUntil}, ${hero.traumaName}, ${hero.flaskCharges})"

  def updateGold(userId: UserId, gold: Long): Fragment =
    sql"update $tableName set gold = $gold where user_id = $userId"

  def updateBaseStats(userId: UserId, stats: pangea.model.stats.BaseStats): Fragment =
    sql"update $tableName set base_stats = ${stats.asJson} where user_id = $userId"

  def updateTrauma(userId: UserId, traumaUntil: Option[Long], traumaName: Option[String]): Fragment =
    sql"update $tableName set trauma_until = $traumaUntil, trauma_name = $traumaName where user_id = $userId"

  def updateFlaskCharges(userId: UserId, charges: Int): Fragment =
    sql"update $tableName set flask_charges = $charges where user_id = $userId"

  def updateDungeonLevel(userId: UserId, level: Int): Fragment =
    sql"update $tableName set dungeon_level = $level where user_id = $userId"

  def updateRace(userId: UserId, race: Race): Fragment =
    sql"update $tableName set race = $race where user_id = $userId"

  def updateState(userId: UserId, state: StateType): Fragment =
    sql"update $tableName set state = $state where user_id = $userId"

  def updateFightStats(userId: UserId, stats: FightStats): Fragment =
    sql"update $tableName set fight_stats = ${stats.asJson} where user_id = $userId"

  def updateExpAndLevel(userId: UserId, exp: Long, lvl: Long, upgradePoints: Long): Fragment =
    sql"update $tableName set exp = $exp, lvl = $lvl, upgrade_points = $upgradePoints where user_id = $userId"

  def updateEquipmentAndFightStats(userId: UserId, eq: Equipment, stats: FightStats): Fragment =
    sql"update $tableName set equipment = ${eq.asJson}, fight_stats = ${stats.asJson} where user_id = $userId"

  def writeActiveBattle(userId: UserId, data: Json): Fragment =
    sql"update $tableName set active_battle = $data where user_id = $userId"

  def readActiveBattle(userId: UserId): Fragment =
    sql"select active_battle from $tableName where user_id = $userId"

  def clearActiveBattle(userId: UserId): Fragment =
    sql"update $tableName set active_battle = null where user_id = $userId"

  def writeSceneData(userId: UserId, data: Json): Fragment =
    sql"update $tableName set scene_data = $data where user_id = $userId"

  def readSceneData(userId: UserId): Fragment =
    sql"select scene_data from $tableName where user_id = $userId"
}
