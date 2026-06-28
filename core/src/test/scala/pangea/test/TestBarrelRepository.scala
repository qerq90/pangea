package pangea.test

import pangea.model.barrel.Barrel
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.Items
import pangea.model.item.Item
import pangea.repository.barrel.{BarrelRepoError, BarrelRepository}
import zio.{IO, ZIO}

class TestBarrelRepository(private var items: List[Item] = Nil, private var gold: Long = 0L)
    extends BarrelRepository {

  private val heroId = HeroId(1L)
  private def barrel: Barrel = Barrel(1L, heroId, Items(items), gold)

  def get(heroId: HeroId): IO[BarrelRepoError, Barrel] = ZIO.succeed(barrel)

  def deposit(heroId: HeroId, item: Item): IO[BarrelRepoError, Unit] =
    if (items.length >= Barrel.MaxItems) ZIO.fail(BarrelRepoError.BarrelFull)
    else ZIO.succeed { items = items :+ item }

  def withdraw(heroId: HeroId, itemId: Long): IO[BarrelRepoError, Item] =
    items.find(_.id == itemId) match {
      case None => ZIO.fail(BarrelRepoError.CantFindItemToRemove)
      case Some(it) =>
        items = items.filterNot(_.id == itemId)
        ZIO.succeed(it)
    }

  def depositGold(heroId: HeroId, amount: Long): IO[BarrelRepoError, Unit] =
    if (amount <= 0)                         ZIO.fail(BarrelRepoError.NonPositiveAmount)
    else if (amount + gold > Barrel.MaxGold) ZIO.fail(BarrelRepoError.GoldOverflow)
    else                                     ZIO.succeed { gold += amount }

  def withdrawGold(heroId: HeroId, amount: Long): IO[BarrelRepoError, Unit] =
    if (amount <= 0)        ZIO.fail(BarrelRepoError.NonPositiveAmount)
    else if (amount > gold) ZIO.fail(BarrelRepoError.NotEnoughGold)
    else                    ZIO.succeed { gold -= amount }

  def itemsSnapshot: List[Item] = items
  def goldSnapshot:  Long       = gold
}

object TestBarrelRepository {
  def empty: TestBarrelRepository = new TestBarrelRepository()
  def withItems(items: List[Item]): TestBarrelRepository = new TestBarrelRepository(items = items)
  def withGold(gold: Long): TestBarrelRepository = new TestBarrelRepository(gold = gold)
  def of(items: List[Item], gold: Long): TestBarrelRepository = new TestBarrelRepository(items, gold)
}
