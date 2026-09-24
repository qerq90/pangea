package pangea.test

import pangea.dao.payout.PayoutDao
import pangea.model.hero.HeroId
import zio.{Task, ZIO}

/** Отложенные выплаты в памяти: прибавляем и забираем всё разом, как в проде. */
class TestPayoutDao(private var pending: Map[HeroId, (Long, Long)] = Map.empty) extends PayoutDao {

  def add(heroId: HeroId, silver: Long, doubloons: Long): Task[Unit] =
    ZIO.succeed {
      val (s, d) = pending.getOrElse(heroId, (0L, 0L))
      pending = pending.updated(heroId, (s + silver, d + doubloons))
    }

  def takeAll(heroId: HeroId): Task[(Long, Long)] =
    ZIO.succeed {
      val money = pending.getOrElse(heroId, (0L, 0L))
      pending = pending - heroId
      money
    }

  def snapshot: Map[HeroId, (Long, Long)] = pending
}

object TestPayoutDao {
  def empty: TestPayoutDao = new TestPayoutDao()
}
