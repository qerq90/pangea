package pangea.test

import pangea.dao.arena.ArenaDao
import pangea.model.arena.ArenaFight
import pangea.model.user.UserId
import zio.{Task, ZIO}

/** Строки боёв арены в памяти: те же правила, что в базе, — код уникален, а
  * игрок дерётся не больше чем в одном бою. */
class TestArenaDao(private var fights: List[ArenaFight] = Nil, private var nextId: Long = 1L) extends ArenaDao {

  override def create(fight: ArenaFight, now: Long): Task[Option[ArenaFight]] =
    ZIO.succeed {
      if (fights.exists(_.code == fight.code)) None
      else {
        val saved = fight.copy(id = nextId)
        nextId += 1
        fights = fights :+ saved
        Some(saved)
      }
    }

  override def byId(id: Long): Task[Option[ArenaFight]] = ZIO.succeed(fights.find(_.id == id))

  override def byCode(code: String): Task[Option[ArenaFight]] = ZIO.succeed(fights.find(_.code == code))

  override def ofUser(userId: UserId): Task[Option[ArenaFight]] = ZIO.succeed(fights.find(_.has(userId)))

  override def waiting(limit: Int): Task[List[ArenaFight]] =
    ZIO.succeed(fights.filter(f => f.waiting && f.open).take(limit))

  override def update(fight: ArenaFight, now: Long): Task[Unit] =
    ZIO.succeed { fights = fights.map(f => if (f.id == fight.id) fight else f) }

  override def delete(id: Long): Task[Unit] =
    ZIO.succeed { fights = fights.filterNot(_.id == id) }

  def snapshot: List[ArenaFight] = fights
}

object TestArenaDao {
  def empty: TestArenaDao = new TestArenaDao()
  def of(fights: ArenaFight*): TestArenaDao =
    new TestArenaDao(fights.toList, fights.map(_.id).foldLeft(1L)(_ max _ + 1))
}
