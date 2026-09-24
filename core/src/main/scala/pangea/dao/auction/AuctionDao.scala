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
  /** Всё, что герой выставлял: активное, непроданное и проданное. */
  def listBySeller(sellerId: HeroId, limit: Long): Task[List[AuctionLot]]

  /** Закрыть лот покупкой — только если он всё ещё в продаже. `false` означает,
    * что лот успели купить или снять, и покупателю ничего списывать нельзя. */
  def markSold(id: Long, buyerId: HeroId, now: Long): Task[Boolean]

  /** Вернуть вещь хозяину: снятие с торгов или забор непроданного. */
  def markReturned(id: Long, sellerId: HeroId): Task[Boolean]
}

object AuctionDao {
  val live: ZLayer[transactor.Transactor[Task], Nothing, AuctionDao] =
    ZLayer.fromFunction(new AuctionDaoLive(_))
}
