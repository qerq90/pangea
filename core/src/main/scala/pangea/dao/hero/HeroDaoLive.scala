package pangea.dao.hero

import doobie.implicits.toSqlInterpolator
import doobie.implicits._
import doobie.postgres.circe.json.implicits._
import io.circe.syntax.EncoderOps
import doobie.util.transactor.Transactor
import pangea.model.hero.{Hero, HeroId}
import pangea.model.user.UserId
import zio.Task
import zio.interop.catz._

class HeroDaoLive(xa: Transactor[Task]) extends HeroDao {

  override def getHeroByUserId(userId: UserId): Task[Option[Hero]] =
    sql"select * from heroes where user_id = ${userId.value}"
      .query[Hero]
      .option
      .transact(xa)

  override def insertHero(hero: Hero): Task[HeroId] =
    sql"insert into heroes(user_id, state, base_stats, fight_stats, equipment) values(${hero.userId}, ${hero.state}, ${hero.baseStats.asJson}, ${hero.fightStats.asJson}, ${hero.equipment.asJson})".update
      .withUniqueGeneratedKeys[HeroId]("id")
      .transact(xa)
}
