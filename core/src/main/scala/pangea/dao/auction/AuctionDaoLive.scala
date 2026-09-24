package pangea.dao.auction

import doobie.implicits._
import doobie.util.transactor.Transactor
import pangea.model.auction.AuctionLot
import pangea.model.hero.HeroId
import pangea.model.item.Item.meta
import zio.Task
import zio.interop.catz._

class AuctionDaoLive(xa: Transactor[Task]) extends AuctionDao {

  private val columns = fr"id, seller_id, item, price, currency, listed_at, expires_at"

  override def insert(lot: AuctionLot): Task[AuctionLot] =
    sql"""insert into auction_lots(seller_id, item, price, currency, listed_at, expires_at)
          values(${lot.sellerId.value}, ${lot.item}, ${lot.price}, ${lot.currency},
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
           where expires_at > $now
           order by id desc
           limit $limit offset $offset""")
      .query[AuctionLot].to[List].transact(xa)

  override def countOnSale(now: Long): Task[Long] =
    sql"select count(*) from auction_lots where expires_at > $now"
      .query[Long].unique.transact(xa)

  override def listBySeller(sellerId: HeroId, limit: Long): Task[List[AuctionLot]] =
    (fr"select" ++ columns ++
      fr"""from auction_lots
           where seller_id = ${sellerId.value}
           order by id desc
           limit $limit""")
      .query[AuctionLot].to[List].transact(xa)

  override def countBySeller(sellerId: HeroId): Task[Long] =
    sql"select count(*) from auction_lots where seller_id = ${sellerId.value}"
      .query[Long].unique.transact(xa)

  // Гонка двух покупателей решается здесь: строка либо удалилась (значит, лот
  // был наш), либо её уже не было — и второй получит `false`.
  override def sold(id: Long, now: Long): Task[Boolean] =
    sql"delete from auction_lots where id = $id and expires_at > $now"
      .update.run.transact(xa).map(_ == 1)

  override def returned(id: Long, sellerId: HeroId): Task[Boolean] =
    sql"delete from auction_lots where id = $id and seller_id = ${sellerId.value}"
      .update.run.transact(xa).map(_ == 1)
}
