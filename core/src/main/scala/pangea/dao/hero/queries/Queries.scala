package pangea.dao.hero.queries

import doobie.implicits.toSqlInterpolator
import doobie.util.fragment.Fragment
import doobie.implicits._
import doobie.postgres.circe.json.implicits._
import io.circe.syntax.EncoderOps
import pangea.model.hero.Hero
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.user.UserId

object Queries {
  private val fields: Fragment =
    sql"id, user_id, state, lvl, exp, upgrade_points, race, base_stats, fight_stats, equipment"
  private val fieldInsert: Fragment =
    sql"user_id, state, lvl, exp, upgrade_points, race, base_stats, fight_stats, equipment"
  private val tableName: Fragment = sql"heroes"

  private val selectAll = fr"select $fields from $tableName"

  def getHeroByUserId(userId: UserId): Fragment =
    selectAll ++ sql"where user_id = $userId"

  def insert(hero: Hero): Fragment =
    sql"insert into $tableName($fieldInsert) values(${hero.userId}, ${hero.state}, ${hero.lvl}, ${hero.exp}, ${hero.upgradePoints}, ${hero.race}, ${hero.baseStats.asJson}, ${hero.fightStats.asJson}, ${hero.equipment.asJson})"

  def updateRace(userId: UserId, race: Race): Fragment =
    sql"update $tableName set race = $race where user_id = $userId"

  def updateState(userId: UserId, state: StateType): Fragment =
    sql"update $tableName set state = $state where user_id = $userId"
}
