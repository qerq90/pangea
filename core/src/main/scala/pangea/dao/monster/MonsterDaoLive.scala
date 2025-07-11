package pangea.dao.monster

import doobie.implicits.toSqlInterpolator
import doobie.util.transactor.Transactor
import doobie.implicits._
import doobie.postgres.circe.json.implicits._
import io.circe.syntax.EncoderOps
import pangea.model.monster.Monster
import zio.Task
import zio.interop.catz._

class MonsterDaoLive(xa: Transactor[Task]) extends MonsterDao {

  override def getMonsterById(id: Long): Task[Option[Monster]] =
    sql"select * from monsters where id = $id"
      .query[Monster]
      .option
      .transact(xa)

  override def insertMonster(monster: Monster): Task[Long] =
    sql"insert into monsters(lvl, race, rarity, fight_stats) values(${monster.lvl}, ${monster.race}, ${monster.rarity}, ${monster.fightStats.asJson})".update
      .withUniqueGeneratedKeys[Long]("id")
      .transact(xa)
}
