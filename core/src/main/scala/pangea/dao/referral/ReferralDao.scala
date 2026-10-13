package pangea.dao.referral

import doobie.implicits._
import doobie.util.transactor
import doobie.util.transactor.Transactor
import pangea.model.user.UserId
import zio.interop.catz._
import zio.{Task, ZLayer}

/** Код друга: кто чей код ввёл и заплачено ли владельцу кода за 10 уровень. */
trait ReferralDao {
  /** Чей код ввёл игрок; `None` — не вводил. */
  def referrerOf(userId: UserId): Task[Option[UserId]]

  /** Записать, что игрок ввёл чужой код. `false` — он уже вводил код: дважды
    * не получится даже двумя сообщениями разом. */
  def add(userId: UserId, referrer: UserId, now: Long): Task[Boolean]

  /** Отметить, что за 10 уровень игрока владельцу кода заплачено, — если ещё
    * нет. Возвращает владельца кода, когда платить надо сейчас; `None` — платить
    * некому или уже заплачено. */
  def claimBonus(userId: UserId): Task[Option[UserId]]
}

class ReferralDaoLive(xa: Transactor[Task]) extends ReferralDao {

  override def referrerOf(userId: UserId): Task[Option[UserId]] =
    sql"select referrer_id from referrals where user_id = ${userId.value}"
      .query[Long].option.transact(xa).map(_.map(UserId(_)))

  override def add(userId: UserId, referrer: UserId, now: Long): Task[Boolean] =
    sql"""insert into referrals(user_id, referrer_id, created_at)
          values(${userId.value}, ${referrer.value}, $now)
          on conflict (user_id) do nothing"""
      .update.run.transact(xa).map(_ == 1)

  // Отметка и ответ одним запросом: двое не выплатят одну и ту же награду.
  override def claimBonus(userId: UserId): Task[Option[UserId]] =
    sql"""update referrals set bonus_paid = true
          where user_id = ${userId.value} and not bonus_paid
          returning referrer_id"""
      .query[Long].option.transact(xa).map(_.map(UserId(_)))
}

object ReferralDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, ReferralDao] =
    ZLayer.fromFunction(new ReferralDaoLive(_))
}
