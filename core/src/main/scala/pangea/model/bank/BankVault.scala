package pangea.model.bank

import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.Items
import pangea.model.item.Item

/** Ячейка в Торговом доме: хранилище героя у банкира Рахадима. Устроено как
 *  неприметная бочка, только вместимость растёт с числом выкупленных ячеек —
 *  каждая даёт [[BankVault.ItemsPerCell]] мест под вещи и
 *  [[BankVault.SilverPerCell]] места под серебро. Серебро отсюда идёт в дело
 *  после того, как кончится своё (см. `Purse`).
 *
 *  Пыль и малые руны места не занимают — то же правило, что в сумке и бочке. */
case class BankVault(
  id:     Long,
  heroId: HeroId,
  cells:  Int,
  items:  Items,
  silver: Long
) {
  def addItem(item: Item): BankVault = copy(items = items.copy(data = items.data.appended(item)))

  def withItems(items: List[Item]): BankVault = copy(items = Items(items))

  /** Куплена ли хоть одна ячейка: без них хранилища у героя нет. */
  def open: Boolean = cells > 0

  def maxItems: Long  = cells.toLong * BankVault.ItemsPerCell
  def maxSilver: Long = cells.toLong * BankVault.SilverPerCell

  /** Сколько мест занято: невесомое добро не в счёт. */
  def occupied: Long = items.data.count(!_.weightless).toLong

  def freeSlots: Long = (maxItems - occupied).max(0L)

  def freeSilverSpace: Long = (maxSilver - silver).max(0L)

  /** Сколько такого же невесомого добра уже лежит — предел у него вместо места. */
  def hoardCount(key: String): Int = items.data.count(_.hoardKey.contains(key))

  def hasRoomForHoard(item: Item): Boolean =
    item.hoardKey.forall(k => hoardCount(k) < Item.HoardLimit)

  /** Сколько стоит следующая ячейка: первая дешёвая, дальше по сто тысяч за
    * каждую уже купленную. */
  def nextCellPrice: Long = BankVault.cellPrice(cells)
}

object BankVault {
  /** Сколько мест под вещи и места под серебро даёт одна ячейка. */
  val ItemsPerCell: Long  = 100L
  val SilverPerCell: Long = 100000L

  /** Первая ячейка — чтобы завести дело; дальше цена растёт на сто тысяч. */
  val FirstCellPrice: Long = 10000L
  val CellPriceStep: Long  = 100000L

  def cellPrice(cells: Int): Long =
    if (cells <= 0) FirstCellPrice else CellPriceStep * cells.toLong

  def empty(heroId: HeroId): BankVault =
    BankVault(id = 0L, heroId = heroId, cells = 0, items = Items(Nil), silver = 0L)
}
