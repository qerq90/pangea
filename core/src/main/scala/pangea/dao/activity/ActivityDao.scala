package pangea.dao.activity

import cats.syntax.foldable._
import cats.syntax.functor._
import doobie.free.{connection => FC}
import doobie.implicits._
import doobie.util.transactor
import doobie.util.transactor.Transactor
import pangea.model.hero.HeroId
import pangea.model.user.UserId
import zio.interop.catz._
import zio.{Task, ZLayer}

/** Баллы игрока за неделю: сколько набрал и когда получил последний — по нему
  * решается спор при равных баллах (кто набрал раньше, тот выше). */
final case class ActivityScore(userId: UserId, points: Long, lastAt: Long)

/** Конкурс активности: посты группы, засчитанные лайки и комментарии, подведённые
  * недели. */
trait ActivityDao {
  /** Запомнить вышедший пост. Повтор события ничего не портит. */
  def recordPost(postId: Long, postedAt: Long): Task[Unit]

  /** Когда вышел пост; `None` — такого поста мы не видели. */
  def postedAt(postId: Long): Task[Option[Long]]

  /** Засчитать действие. `false` — оно уже засчитано: один лайк и один
    * комментарий на пост от игрока, сколько бы раз он их ни повторял. */
  def recordAction(userId: UserId, postId: Long, kind: String, points: Long, week: Long, at: Long): Task[Boolean]

  /** Лучшие за неделю: по баллам, при равенстве — кто набрал раньше. */
  def top(week: Long, limit: Int): Task[List[ActivityScore]]

  /** Подведена ли уже неделя. */
  def awarded(week: Long): Task[Boolean]

  /** Наградить неделю одним заходом: отметка «подведена» и деньги победителям
    * (в отложенные выплаты, см. `Payouts`) — в одной транзакции. `false` —
    * неделю уже подвели, и никто ничего не получил второй раз. */
  def award(week: Long, prizes: List[(HeroId, Long)], now: Long): Task[Boolean]
}

class ActivityDaoLive(xa: Transactor[Task]) extends ActivityDao {

  override def recordPost(postId: Long, postedAt: Long): Task[Unit] =
    sql"""insert into group_posts(post_id, posted_at) values($postId, $postedAt)
          on conflict (post_id) do nothing"""
      .update.run.transact(xa).unit

  override def postedAt(postId: Long): Task[Option[Long]] =
    sql"select posted_at from group_posts where post_id = $postId"
      .query[Long].option.transact(xa)

  // Баллы лежат прямо в строке действия: сколько оно стоило в момент засчёта.
  override def recordAction(userId: UserId, postId: Long, kind: String, points: Long, week: Long, at: Long): Task[Boolean] =
    sql"""insert into activity_actions(user_id, post_id, kind, points, week, at)
          values(${userId.value}, $postId, $kind, $points, $week, $at)
          on conflict (user_id, post_id, kind) do nothing"""
      .update.run.transact(xa).map(_ == 1)

  override def top(week: Long, limit: Int): Task[List[ActivityScore]] =
    sql"""select user_id, sum(points), max(at) from activity_actions
          where week = $week
          group by user_id
          order by sum(points) desc, max(at) asc
          limit $limit"""
      .query[(Long, Long, Long)].to[List].transact(xa)
      .map(_.map { case (user, points, last) => ActivityScore(UserId(user), points, last) })

  override def awarded(week: Long): Task[Boolean] =
    sql"select count(*) from activity_prizes where week = $week"
      .query[Long].unique.transact(xa).map(_ > 0L)

  override def award(week: Long, prizes: List[(HeroId, Long)], now: Long): Task[Boolean] = {
    val mark =
      sql"""insert into activity_prizes(week, paid_at) values($week, $now)
            on conflict (week) do nothing""".update.run

    def pay(hero: HeroId, doubloons: Long) =
      sql"""insert into pending_payouts(hero_id, silver, doubloons)
            values(${hero.value}, 0, $doubloons)
            on conflict (hero_id) do update
            set doubloons = pending_payouts.doubloons + $doubloons""".update.run

    mark.flatMap {
      case 0 => FC.pure(false)
      case _ => prizes.traverse_ { case (hero, doubloons) => pay(hero, doubloons) }.as(true)
    }.transact(xa)
  }
}

object ActivityDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, ActivityDao] =
    ZLayer.fromFunction(new ActivityDaoLive(_))
}
