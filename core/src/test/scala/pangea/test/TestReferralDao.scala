package pangea.test

import pangea.dao.referral.ReferralDao
import pangea.model.user.UserId
import zio.{Task, ZIO}

/** Коды друзей в памяти: кто чей код вписал и заплачено ли за 10 уровень. */
class TestReferralDao extends ReferralDao {
  private var rows: Map[UserId, (UserId, Boolean)] = Map.empty

  def referrerOf(userId: UserId): Task[Option[UserId]] = ZIO.succeed(rows.get(userId).map(_._1))

  def add(userId: UserId, referrer: UserId, now: Long): Task[Boolean] =
    ZIO.succeed {
      if (rows.contains(userId)) false
      else { rows = rows.updated(userId, (referrer, false)); true }
    }

  def claimBonus(userId: UserId): Task[Option[UserId]] =
    ZIO.succeed {
      rows.get(userId) match {
        case Some((referrer, false)) => rows = rows.updated(userId, (referrer, true)); Some(referrer)
        case _                       => None
      }
    }

  def snapshot: Map[UserId, (UserId, Boolean)] = rows
}
