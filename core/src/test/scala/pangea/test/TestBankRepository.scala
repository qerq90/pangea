package pangea.test

import pangea.model.bank.BankVault
import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.Items
import pangea.model.item.Item
import pangea.repository.bank.{BankRepoError, BankRepository}
import zio.{IO, ZIO}

/** Хранилище Рахадима для тестов: та же логика ячеек, что в проде. */
class TestBankRepository(
  private var cells:  Int        = 0,
  private var items:  List[Item] = Nil,
  private var silver: Long       = 0L
) extends BankRepository {

  private val heroId = HeroId(1L)
  private def vault: BankVault = BankVault(1L, heroId, cells, Items(items), silver)

  def get(heroId: HeroId): IO[BankRepoError, BankVault] = ZIO.succeed(vault)

  def buyCell(heroId: HeroId): IO[BankRepoError, BankVault] =
    ZIO.succeed { cells += 1; vault }

  def deposit(heroId: HeroId, item: Item): IO[BankRepoError, Unit] =
    if (!vault.open)                                          ZIO.fail(BankRepoError.NoVault)
    else if (item.weightless && !vault.hasRoomForHoard(item))  ZIO.fail(BankRepoError.DustLimitReached)
    else if (!item.weightless && vault.freeSlots <= 0)         ZIO.fail(BankRepoError.VaultFull)
    else                                                       ZIO.succeed { items = items :+ item }

  def withdraw(heroId: HeroId, itemId: Long): IO[BankRepoError, Item] =
    items.find(_.id == itemId) match {
      case None => ZIO.fail(BankRepoError.CantFindItemToRemove)
      case Some(it) =>
        items = items.filterNot(_.id == itemId)
        ZIO.succeed(it)
    }

  def depositSilver(heroId: HeroId, amount: Long): IO[BankRepoError, Unit] =
    if (amount <= 0)                          ZIO.fail(BankRepoError.NonPositiveAmount)
    else if (!vault.open)                     ZIO.fail(BankRepoError.NoVault)
    else if (amount > vault.freeSilverSpace)  ZIO.fail(BankRepoError.SilverOverflow)
    else                                      ZIO.succeed { silver += amount }

  def withdrawSilver(heroId: HeroId, amount: Long): IO[BankRepoError, Unit] =
    if (amount <= 0)          ZIO.fail(BankRepoError.NonPositiveAmount)
    else if (amount > silver) ZIO.fail(BankRepoError.NotEnoughSilver)
    else                      ZIO.succeed { silver -= amount }

  def cellsSnapshot: Int         = cells
  def itemsSnapshot: List[Item]  = items
  def silverSnapshot: Long       = silver
}

object TestBankRepository {
  def empty: TestBankRepository = new TestBankRepository()
  def withCells(cells: Int): TestBankRepository = new TestBankRepository(cells = cells)
  def of(cells: Int, items: List[Item] = Nil, silver: Long = 0L): TestBankRepository =
    new TestBankRepository(cells, items, silver)
}
