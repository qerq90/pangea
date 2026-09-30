package pangea.dao.arena

import doobie.util.transactor
import pangea.model.arena.ArenaFight
import pangea.model.user.UserId
import zio.{Task, ZLayer}

/** Строки боёв арены. Одна строка — один бой: от записи до последнего удара. */
trait ArenaDao {

  /** Завести бой с одним бойцом и его кодом. Код занят — вернётся None: код
    * катает состояние, и повтор оно разыграет само. */
  def create(fight: ArenaFight, now: Long): Task[Option[ArenaFight]]

  def byId(id: Long): Task[Option[ArenaFight]]

  def byCode(code: String): Task[Option[ArenaFight]]

  /** Бой, в котором этот игрок сейчас участвует, — ждущий или идущий. */
  def ofUser(userId: UserId): Task[Option[ArenaFight]]

  /** Кто ждёт соперника, раньше записавшиеся первыми. */
  def waiting(limit: Int): Task[List[ArenaFight]]

  def update(fight: ArenaFight, now: Long): Task[Unit]

  def delete(id: Long): Task[Unit]
}

object ArenaDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, ArenaDao] =
    ZLayer.fromFunction(new ArenaDaoLive(_))
}
