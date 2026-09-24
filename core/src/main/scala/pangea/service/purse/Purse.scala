package pangea.service.purse

import pangea.dao.hero.HeroDao
import pangea.model.hero.Hero
import pangea.model.user.UserId
import pangea.repository.bank.BankRepository
import zio.{Task, ZIO}

/** Что у героя на руках и что лежит в ячейке Торгового дома. Платим сперва
 *  своим, нехватку добираем из хранилища Рахадима. */
final case class Wallet(hand: Long, vault: Long) {
  def total: Long = hand + vault

  def canAfford(price: Long): Boolean = total >= price

  /** Сколько снять с рук и сколько из ячейки; None — денег не хватает. */
  def split(price: Long): Option[(Long, Long)] =
    Option.when(price >= 0 && canAfford(price)) {
      val fromHand = hand.min(price)
      (fromHand, price - fromHand)
    }
}

/** Общий кошель героя: своё серебро плюс серебро в ячейке Торгового дома.
 *  Все покупки и передачи идут через него, поэтому банковское серебро тратится
 *  само, как только кончилось своё.
 *
 *  [[Purse.heroOnly]] — кошель без банка (тесты и места, где ячейка ни при чём):
 *  ведёт себя ровно как прежний `heroDao.updateSilver`. */
final case class Purse(heroDao: HeroDao, bank: Option[BankRepository]) {

  def wallet(hero: Hero): Task[Wallet] =
    bank match {
      case None    => ZIO.succeed(Wallet(hero.silver, 0L))
      case Some(b) => b.get(hero.id).fold(_ => Wallet(hero.silver, 0L), v => Wallet(hero.silver, v.silver))
    }

  def total(hero: Hero): Task[Long] = wallet(hero).map(_.total)

  def canAfford(hero: Hero, price: Long): Task[Boolean] = wallet(hero).map(_.canAfford(price))

  /** Списывает цену: сперва с рук, остаток — из ячейки. Возвращает героя с
    * новым серебром на руках; `None` — не хватило, и тогда ничего не списано. */
  def charge(userId: UserId, hero: Hero, price: Long): Task[Option[Hero]] =
    for {
      w   <- wallet(hero)
      res <- w.split(price) match {
        case None => ZIO.none
        case Some((fromHand, fromVault)) =>
          ZIO.foreachDiscard(bank.filter(_ => fromVault > 0))(
            _.withdrawSilver(hero.id, fromVault).mapError(e => new Throwable(e.toString))
          ) *> heroDao.updateSilver(userId, hero.silver - fromHand)
            .as(Some(hero.copy(silver = hero.silver - fromHand)))
      }
    } yield res
}

object Purse {
  /** Кошель без банка: тратится только то, что у героя на руках. */
  def heroOnly(heroDao: HeroDao): Purse = Purse(heroDao, None)

  def live(heroDao: HeroDao, bank: BankRepository): Purse = Purse(heroDao, Some(bank))
}
