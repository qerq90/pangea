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

  // Номер лота — наименьший свободный: проданный или снятый лот освобождает
  // свой, и следующий продавец занимает дырку, а не берёт новое число. Искать
  // есть смысл только до «сколько лотов + 1»: среди стольких чисел свободное
  // найдётся всегда.
  private val freeId =
    sql"""select min(n)
          from generate_series(1, (select count(*) from auction_lots) + 1) as g(n)
          where not exists (select 1 from auction_lots a where a.id = g.n)"""
      .query[Long].unique

  // Замок на время транзакции: без него двое продавцов, зашедшие разом,
  // высчитают один и тот же свободный номер и один упрётся в первичный ключ.
  private val lockIds =
    sql"select pg_advisory_xact_lock(${AuctionDaoLive.LotIdLock})".query[Unit].unique

  override def insert(lot: AuctionLot): Task[AuctionLot] =
    (for {
      _  <- lockIds
      id <- freeId
      _  <- sql"""insert into auction_lots(id, seller_id, item, price, currency, listed_at, expires_at)
                  values($id, ${lot.sellerId.value}, ${lot.item}, ${lot.price}, ${lot.currency},
                         ${lot.listedAt}, ${lot.expiresAt})""".update.run
    } yield lot.copy(id = id)).transact(xa)

  override def get(id: Long): Task[Option[AuctionLot]] =
    (fr"select" ++ columns ++ fr"from auction_lots where id = $id")
      .query[AuctionLot].option.transact(xa)

  override def listOnSale(now: Long, offset: Long, limit: Long): Task[List[AuctionLot]] =
    (fr"select" ++ columns ++
      fr"""from auction_lots
           where expires_at > $now
           order by listed_at desc, id desc
           limit $limit offset $offset""")
      .query[AuctionLot].to[List].transact(xa)

  override def countOnSale(now: Long): Task[Long] =
    sql"select count(*) from auction_lots where expires_at > $now"
      .query[Long].unique.transact(xa)

  override def listBySeller(sellerId: HeroId, limit: Long): Task[List[AuctionLot]] =
    (fr"select" ++ columns ++
      fr"""from auction_lots
           where seller_id = ${sellerId.value}
           order by listed_at desc, id desc
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

object AuctionDaoLive {
  /** Ключ advisory-замка, под которым выдаются номера лотов. Число произвольное,
    * важно лишь, чтобы его не занял кто-то ещё. */
  val LotIdLock: Long = 770001L
}
