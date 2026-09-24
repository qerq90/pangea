package pangea.dao.auction

import doobie.implicits._
import doobie.util.transactor.Transactor
import pangea.model.auction.{AuctionLot, LotStatus}
import pangea.model.hero.HeroId
import pangea.model.item.Item.meta
import zio.Task
import zio.interop.catz._

class AuctionDaoLive(xa: Transactor[Task]) extends AuctionDao {

  // Литералы статусов приводим к базовому типу: doobie ищет Meta[LotStatus],
  // а не Meta для singleton-подтипа `Active.type`.
  private val active:   LotStatus = LotStatus.Active
  private val sold:     LotStatus = LotStatus.Sold
  private val returned: LotStatus = LotStatus.Returned

  private val columns =
    fr"id, seller_id, item, price, currency, status, listed_at, expires_at, buyer_id"

  override def insert(lot: AuctionLot): Task[AuctionLot] =
    sql"""insert into auction_lots(seller_id, item, price, currency, status, listed_at, expires_at)
          values(${lot.sellerId.value}, ${lot.item}, ${lot.price}, ${lot.currency}, ${lot.status},
                 ${lot.listedAt}, ${lot.expiresAt})"""
      .update
      .withUniqueGeneratedKeys[Long]("id")
      .transact(xa)
      .map(id => lot.copy(id = id))

  override def get(id: Long): Task[Option[AuctionLot]] =
    (fr"select" ++ columns ++ fr"from auction_lots where id = $id")
      .query[AuctionLot].option.transact(xa)

  override def listOnSale(now: Long, offset: Long, limit: Long): Task[List[AuctionLot]] =
    (fr"select" ++ columns ++
      fr"""from auction_lots
           where status = $active and expires_at > $now
           order by id desc
           limit $limit offset $offset""")
      .query[AuctionLot].to[List].transact(xa)

  override def countOnSale(now: Long): Task[Long] =
    sql"select count(*) from auction_lots where status = $active and expires_at > $now"
      .query[Long].unique.transact(xa)

  override def listBySeller(sellerId: HeroId, limit: Long): Task[List[AuctionLot]] =
    (fr"select" ++ columns ++
      fr"""from auction_lots
           where seller_id = ${sellerId.value}
           order by id desc
           limit $limit""")
      .query[AuctionLot].to[List].transact(xa)

  // Гонка двух покупателей решается здесь: условие `status = active` стоит в
  // самом UPDATE, поэтому второй получит 0 изменённых строк и `false`.
  override def markSold(id: Long, buyerId: HeroId, now: Long): Task[Boolean] =
    sql"""update auction_lots
          set status = $sold, buyer_id = ${buyerId.value}
          where id = $id and status = $active and expires_at > $now"""
      .update.run.transact(xa).map(_ == 1)

  override def markReturned(id: Long, sellerId: HeroId): Task[Boolean] =
    sql"""update auction_lots
          set status = $returned
          where id = $id and seller_id = ${sellerId.value} and status = $active"""
      .update.run.transact(xa).map(_ == 1)
}
