package pangea.dao.hero

import doobie.implicits._
import doobie.util.transactor.Transactor
import io.circe.syntax.EncoderOps
import pangea.model.hero.Hero
import pangea.model.user.UserId
import zio.Task
import zio.interop.catz._

class HeroDaoLive(xa: Transactor[Task]) extends HeroDao {

  override def getHeroByUserId(userId: UserId): Task[Option[Hero]] =
    sql"select * from heroes where user_id = ${userId.value}"
      .query[Hero]
      .option
      .transact(xa)

  override def insertHero(hero: Hero): Task[Hero] =
    sql"insert into heroes(user_id, state, base_stats, fight_stats, equipment) values(${hero.userId}, ${hero.state}, ${hero.baseStats.asJson.noSpaces}, ${hero.fightStats.asJson.noSpaces}, ${hero.equipment.asJson.noSpaces}) returning id, user_id, state, base_stats, fight_stats, equipment".update
      .withUniqueGeneratedKeys[Hero](
        "id",
        "user_id",
        "state",
        "base_stats",
        "fight_stats",
        "equipment"
      )
      .transact(xa)
}
