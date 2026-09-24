package pangea.dao.auction

import doobie.util.transactor
import pangea.model.auction.AuctionLot
import pangea.model.hero.HeroId
import zio.{Task, ZLayer}

trait AuctionDao {
  def insert(lot: AuctionLot): Task[AuctionLot]
  def get(id: Long): Task[Option[AuctionLot]]
  /** Витрина: лоты в продаже, самые свежие сверху. */
  def listOnSale(now: Long, offset: Long, limit: Long): Task[List[AuctionLot]]
  def countOnSale(now: Long): Task[Long]
  /** Лоты героя: и в продаже, и непроданные. */
  def listBySeller(sellerId: HeroId, limit: Long): Task[List[AuctionLot]]
  def countBySeller(sellerId: HeroId): Task[Long]

  /** Продать лот — только если он всё ещё в продаже. Строка удаляется: истории
    * продаж мы не держим. `false` означает, что лот успели купить или снять, и
    * покупателю ничего списывать нельзя. */
  def sold(id: Long, now: Long): Task[Boolean]

  /** Вернуть вещь хозяину: снятие с торгов или забор непроданного. */
  def returned(id: Long, sellerId: HeroId): Task[Boolean]
}

object AuctionDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, AuctionDao] =
    ZLayer.fromFunction(new AuctionDaoLive(_))
}
