package pangea.repository.barrel

import pangea.dao.barrel.BarrelDao
import pangea.model.barrel.Barrel
import pangea.model.hero.HeroId
import pangea.model.item.Item
import zio.{IO, ZIO}

final class BarrelRepositoryLive(dao: BarrelDao) extends BarrelRepository {

  def get(heroId: HeroId): IO[BarrelRepoError, Barrel] =
    dao.getOrCreate(heroId).orElseFail(BarrelRepoError.CantFindBarrel)

  def deposit(heroId: HeroId, item: Item): IO[BarrelRepoError, Unit] =
    for {
      barrel <- get(heroId)
      _      <- ZIO.when(barrel.freeSlots <= 0)(ZIO.fail(BarrelRepoError.BarrelFull))
      _      <- dao.update(barrel.addItem(item)).orElseFail(BarrelRepoError.CantUpdateBarrel)
    } yield ()

  def withdraw(heroId: HeroId, itemId: Long): IO[BarrelRepoError, Item] =
    for {
      barrel <- get(heroId)
      item   <- ZIO.fromOption(barrel.items.data.find(_.id == itemId))
                  .orElseFail(BarrelRepoError.CantFindItemToRemove)
      rest    = barrel.items.data.filterNot(_.id == itemId)
      _      <- dao.update(barrel.withItems(rest)).orElseFail(BarrelRepoError.CantUpdateBarrel)
    } yield item

  def depositGold(heroId: HeroId, amount: Long): IO[BarrelRepoError, Unit] =
    for {
      _      <- ZIO.when(amount <= 0)(ZIO.fail(BarrelRepoError.NonPositiveAmount))
      barrel <- get(heroId)
      _      <- ZIO.when(amount > barrel.freeGoldSpace)(ZIO.fail(BarrelRepoError.GoldOverflow))
      _      <- dao.update(barrel.copy(gold = barrel.gold + amount)).orElseFail(BarrelRepoError.CantUpdateBarrel)
    } yield ()

  def withdrawGold(heroId: HeroId, amount: Long): IO[BarrelRepoError, Unit] =
    for {
      _      <- ZIO.when(amount <= 0)(ZIO.fail(BarrelRepoError.NonPositiveAmount))
      barrel <- get(heroId)
      _      <- ZIO.when(amount > barrel.gold)(ZIO.fail(BarrelRepoError.NotEnoughGold))
      _      <- dao.update(barrel.copy(gold = barrel.gold - amount)).orElseFail(BarrelRepoError.CantUpdateBarrel)
    } yield ()
}
