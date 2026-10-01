package pangea.test

import pangea.model.auction.{AuctionCurrency, AuctionLot}
import pangea.model.hero.HeroId
import pangea.model.item.Item
import pangea.repository.auction.{AuctionRepoError, AuctionRepository}
import zio.{IO, ZIO}

/** Аукцион в памяти: те же правила, что в проде, включая «кто первый, того и
  * лот» — покупка забирает лот, и второму достаётся отказ. Проданные и снятые
  * лоты не хранятся. */
class TestAuctionRepository(private var lots: List[AuctionLot] = Nil) extends AuctionRepository {

  /** Наименьший свободный номер — как в проде: проданный лот освобождает свой. */
  private def freeId: Long = Iterator.from(1).map(_.toLong).find(n => !lots.exists(_.id == n)).get

  /** Свежие сверху. По номеру сортировать нельзя: они переиспользуются. */
  private def newestFirst(xs: List[AuctionLot]): List[AuctionLot] =
    xs.sortBy(l => (-l.listedAt, -l.id))

  def page(now: Long, page: Int, pageSize: Int): IO[AuctionRepoError, (List[AuctionLot], Int, Int)] = {
    val onSaleLots = newestFirst(lots.filter(_.onSale(now)))
    val pages      = ((onSaleLots.size + pageSize - 1) / pageSize).max(1)
    val p          = page.max(0).min(pages - 1)
    ZIO.succeed((onSaleLots.slice(p * pageSize, p * pageSize + pageSize), pages, p))
  }

  def onSale(now: Long): IO[AuctionRepoError, Long] = ZIO.succeed(lots.count(_.onSale(now)).toLong)

  def lot(id: Long): IO[AuctionRepoError, AuctionLot] =
    ZIO.fromOption(lots.find(_.id == id)).orElseFail(AuctionRepoError.LotNotFound)

  def mine(sellerId: HeroId, limit: Long): IO[AuctionRepoError, List[AuctionLot]] =
    ZIO.succeed(newestFirst(lots.filter(_.sellerId == sellerId)).take(limit.toInt))

  def mineCount(sellerId: HeroId): IO[AuctionRepoError, Long] =
    ZIO.succeed(lots.count(_.sellerId == sellerId).toLong)

  def sell(sellerId: HeroId, item: Item, price: Long, currency: AuctionCurrency, now: Long): IO[AuctionRepoError, AuctionLot] =
    if (item.weightless)                  ZIO.fail(AuctionRepoError.Weightless)
    else if (price < AuctionLot.MinPrice) ZIO.fail(AuctionRepoError.PriceTooLow)
    else if (price > AuctionLot.MaxPrice) ZIO.fail(AuctionRepoError.PriceTooHigh)
    else if (lots.count(_.sellerId == sellerId) >= AuctionLot.MaxLots) ZIO.fail(AuctionRepoError.TooManyLots)
    else ZIO.succeed {
      val lot = AuctionLot.fresh(sellerId, item, price, currency, now).copy(id = freeId)
      lots = lots :+ lot
      lot
    }

  def buy(lotId: Long, now: Long): IO[AuctionRepoError, AuctionLot] =
    lots.find(_.id == lotId) match {
      case Some(lot) if lot.onSale(now) =>
        lots = lots.filterNot(_.id == lotId)
        ZIO.succeed(lot)
      case Some(_) => ZIO.fail(AuctionRepoError.LotGone)
      case None    => ZIO.fail(AuctionRepoError.LotNotFound)
    }

  def reclaim(lotId: Long, sellerId: HeroId): IO[AuctionRepoError, AuctionLot] =
    lots.find(_.id == lotId) match {
      case Some(lot) if lot.sellerId == sellerId =>
        lots = lots.filterNot(_.id == lotId)
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
