package pangea.test

import pangea.model.auction.{AuctionCurrency, AuctionLot, LotStatus}
import pangea.model.hero.HeroId
import pangea.model.item.Item
import pangea.repository.auction.{AuctionRepoError, AuctionRepository}
import zio.{IO, ZIO}

/** Аукцион в памяти: те же правила, что в проде, включая «кто первый, того и
  * лот» — покупка закрывает лот и второму достаётся отказ. */
class TestAuctionRepository(private var lots: List[AuctionLot] = Nil) extends AuctionRepository {

  private var nextId: Long = lots.map(_.id).maxOption.getOrElse(0L) + 1L

  def page(now: Long, page: Int, pageSize: Int): IO[AuctionRepoError, (List[AuctionLot], Int, Int)] = {
    val onSaleLots = lots.filter(_.onSale(now)).sortBy(-_.id)
    val pages      = ((onSaleLots.size + pageSize - 1) / pageSize).max(1)
    val p          = page.max(0).min(pages - 1)
    ZIO.succeed((onSaleLots.slice(p * pageSize, p * pageSize + pageSize), pages, p))
  }

  def onSale(now: Long): IO[AuctionRepoError, Long] = ZIO.succeed(lots.count(_.onSale(now)).toLong)

  def lot(id: Long): IO[AuctionRepoError, AuctionLot] =
    ZIO.fromOption(lots.find(_.id == id)).orElseFail(AuctionRepoError.LotNotFound)

  def mine(sellerId: HeroId, limit: Long): IO[AuctionRepoError, List[AuctionLot]] =
    ZIO.succeed(lots.filter(_.sellerId == sellerId).sortBy(-_.id).take(limit.toInt))

  def sell(sellerId: HeroId, item: Item, price: Long, currency: AuctionCurrency, now: Long): IO[AuctionRepoError, AuctionLot] =
    if (item.weightless)                  ZIO.fail(AuctionRepoError.Weightless)
    else if (price < AuctionLot.MinPrice) ZIO.fail(AuctionRepoError.PriceTooLow)
    else if (price > AuctionLot.MaxPrice) ZIO.fail(AuctionRepoError.PriceTooHigh)
    else ZIO.succeed {
      val lot = AuctionLot.fresh(sellerId, item, price, currency, now).copy(id = nextId)
      nextId += 1L
      lots = lots :+ lot
      lot
    }

  def buy(lotId: Long, buyerId: HeroId, now: Long): IO[AuctionRepoError, AuctionLot] =
    lots.find(_.id == lotId) match {
      case Some(lot) if lot.onSale(now) =>
        lots = lots.map(l => if (l.id == lotId) l.copy(status = LotStatus.Sold, buyerId = Some(buyerId.value)) else l)
        ZIO.succeed(lot)
      case Some(_) => ZIO.fail(AuctionRepoError.LotGone)
      case None    => ZIO.fail(AuctionRepoError.LotNotFound)
    }

  def reclaim(lotId: Long, sellerId: HeroId): IO[AuctionRepoError, AuctionLot] =
    lots.find(_.id == lotId) match {
      case Some(lot) if lot.status == LotStatus.Active && lot.sellerId == sellerId =>
        lots = lots.map(l => if (l.id == lotId) l.copy(status = LotStatus.Returned) else l)
        ZIO.succeed(lot)
      case Some(_) => ZIO.fail(AuctionRepoError.LotGone)
      case None    => ZIO.fail(AuctionRepoError.LotNotFound)
    }

  def snapshot: List[AuctionLot] = lots
}

object TestAuctionRepository {
  def empty: TestAuctionRepository = new TestAuctionRepository()
  def of(lots: AuctionLot*): TestAuctionRepository = new TestAuctionRepository(lots.toList)
}
