package pangea.dao.monster

import doobie.util.transactor
import pangea.model.monster.Monster
import zio.{Task, ZLayer}

trait MonsterDao {
  def getMonsterById(id: Long): Task[Option[Monster]]
  def insertMonster(monster: Monster): Task[Long]
}

object MonsterDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, MonsterDaoLive] =
    ZLayer.fromFunction(new MonsterDaoLive(_))
}
