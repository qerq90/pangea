package pangea.dao.payout

import doobie.implicits._
import doobie.util.transactor
import doobie.util.transactor.Transactor
import pangea.model.hero.HeroId
import zio.interop.catz._
import zio.{Task, ZLayer}

/** Деньги, которые ждут героя в городе: выручка с аукциона, не влезшая в
 *  банковскую ячейку. На руки они попадают только в городской сцене — чтобы
 *  игрок не потерял с ними половину, умерев в лабиринте. */
trait PayoutDao {
  /** Прибавить к отложенному. Считаем в самом UPDATE: герой в этот момент может
    * получать и другую выручку. */
  def add(heroId: HeroId, silver: Long, doubloons: Long): Task[Unit]

  /** Забрать всё отложенное одним разом: возвращает суммы и чистит запись. */
  def takeAll(heroId: HeroId): Task[(Long, Long)]
}

class PayoutDaoLive(xa: Transactor[Task]) extends PayoutDao {

  override def add(heroId: HeroId, silver: Long, doubloons: Long): Task[Unit] =
    sql"""insert into pending_payouts(hero_id, silver, doubloons)
          values(${heroId.value}, $silver, $doubloons)
          on conflict (hero_id) do update
          set silver    = pending_payouts.silver + $silver,
              doubloons = pending_payouts.doubloons + $doubloons"""
      .update.run.transact(xa).unit

  // delete ... returning: и прочитали, и забрали — двое одновременно не выдадут
  // одни и те же деньги дважды.
  override def takeAll(heroId: HeroId): Task[(Long, Long)] =
    sql"delete from pending_payouts where hero_id = ${heroId.value} returning silver, doubloons"
      .query[(Long, Long)].option.transact(xa).map(_.getOrElse((0L, 0L)))
}

object PayoutDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, PayoutDao] =
    ZLayer.fromFunction(new PayoutDaoLive(_))
}
