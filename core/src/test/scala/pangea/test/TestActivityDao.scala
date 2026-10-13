package pangea.test

import pangea.dao.activity.{ActivityDao, ActivityScore}
import pangea.model.hero.HeroId
import pangea.model.user.UserId
import zio.{Task, ZIO}

/** Конкурс активности в памяти. Выплаты победителям ложатся в тот же склад
  * отложенных выплат, что и в проде (`pending_payouts`), — здесь это
  * [[TestPayoutDao]]. */
class TestActivityDao(payouts: TestPayoutDao) extends ActivityDao {
  private var posts: Map[Long, Long]                               = Map.empty
  private var actions: Map[(UserId, Long, String), (Long, Long, Long)] = Map.empty  // (points, week, at)
  private var awardedWeeks: Set[Long]                              = Set.empty

  def recordPost(postId: Long, postedAt: Long): Task[Unit] =
    ZIO.succeed { if (!posts.contains(postId)) posts = posts.updated(postId, postedAt) }

  def postedAt(postId: Long): Task[Option[Long]] = ZIO.succeed(posts.get(postId))

  def recordAction(userId: UserId, postId: Long, kind: String, points: Long, week: Long, at: Long): Task[Boolean] =
    ZIO.succeed {
      val key = (userId, postId, kind)
      if (actions.contains(key)) false
      else { actions = actions.updated(key, (points, week, at)); true }
    }

  def top(week: Long, limit: Int): Task[List[ActivityScore]] =
    ZIO.succeed {
      actions.toList
        .collect { case ((user, _, _), (points, w, at)) if w == week => (user, points, at) }
        .groupBy(_._1).toList
        .map { case (user, rows) => ActivityScore(user, rows.map(_._2).sum, rows.map(_._3).max) }
        .sortBy(s => (-s.points, s.lastAt))
        .take(limit)
    }

  def awarded(week: Long): Task[Boolean] = ZIO.succeed(awardedWeeks.contains(week))

  def award(week: Long, prizes: List[(HeroId, Long)], now: Long): Task[Boolean] =
    if (awardedWeeks.contains(week)) ZIO.succeed(false)
    else
      ZIO.succeed { awardedWeeks += week } *>
        ZIO.foreachDiscard(prizes) { case (hero, doubloons) => payouts.add(hero, 0L, doubloons) }.as(true)

  def awardedSnapshot: Set[Long] = awardedWeeks
}
