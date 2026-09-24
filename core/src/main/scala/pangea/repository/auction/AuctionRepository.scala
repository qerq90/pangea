package pangea.repository.auction

import pangea.dao.auction.AuctionDao
import pangea.model.auction.{AuctionCurrency, AuctionLot}
import pangea.model.hero.HeroId
import pangea.model.item.Item
import zio.{IO, ZLayer}

sealed trait AuctionRepoError
object AuctionRepoError {
  /** Дешевле [[AuctionLot.MinPrice]] не выставляют. */
  case object PriceTooLow  extends AuctionRepoError
  case object PriceTooHigh extends AuctionRepoError
  /** Пыль и малые руны места не занимают — их на торги не берут. */
  case object Weightless   extends AuctionRepoError
  case object LotNotFound  extends AuctionRepoError
  /** Лот только что купили, сняли или у него вышел срок. */
  case object LotGone      extends AuctionRepoError
  case object Failed       extends AuctionRepoError
}

/** Аукцион Торгового дома: витрина, выставление, покупка и возврат вещей.
 *  Деньги репозиторий не трогает — их считает состояние через `Purse`. */
trait AuctionRepository {
  /** Страница витрины: лоты, сколько всего страниц и какая показана. */
  def page(now: Long, page: Int, pageSize: Int): IO[AuctionRepoError, (List[AuctionLot], Int, Int)]
  /** Сколько лотов сейчас в продаже. */
  def onSale(now: Long): IO[AuctionRepoError, Long]
  def lot(id: Long): IO[AuctionRepoError, AuctionLot]
  def mine(sellerId: HeroId, limit: Long): IO[AuctionRepoError, List[AuctionLot]]

  /** Выставить вещь. Плату за выставление берёт вызывающий. */
  def sell(sellerId: HeroId, item: Item, price: Long, currency: AuctionCurrency, now: Long): IO[AuctionRepoError, AuctionLot]

  /** Купить: лот закрывается атомарно, поэтому второму покупателю придёт
    * [[AuctionRepoError.LotGone]] и списывать с него нечего. */
  def buy(lotId: Long, buyerId: HeroId, now: Long): IO[AuctionRepoError, AuctionLot]

  /** Снять свой лот с торгов или забрать непроданное: возвращает вещь. */
  def reclaim(lotId: Long, sellerId: HeroId): IO[AuctionRepoError, AuctionLot]
}

object AuctionRepository {
  val live: ZLayer[AuctionDao, Nothing, AuctionRepository] =
    ZLayer.fromFunction(new AuctionRepositoryLive(_))
}
