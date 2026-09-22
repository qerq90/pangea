package pangea.model.barrel

import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.Items
import pangea.model.item.{Item, MaterialKind}

/** Личная неприметная бочка игрока в Портовом квартале: хранилище до 100 предметов
 *  и до 100 000 серебра, независимое от инвентаря и кошелька героя.
 */
case class Barrel(
  id: Long,
  heroId: HeroId,
  items: Items,
  silver: Long
) {
  def addItem(item: Item): Barrel =
    copy(items = items.copy(data = items.data.appended(item)))

  def withItems(items: List[Item]): Barrel = copy(items = Items(items))

  /** Сколько в бочке лежит того, что занимает место: пыль не в счёт. */
  def occupied: Long = items.data.count(!_.isDust).toLong

  def freeSlots: Long = (Barrel.MaxItems - occupied).max(0L)

  /** Сколько горстей этой пыли уже в бочке — у неё свой предел вместо места. */
  def dustCount(kind: MaterialKind): Int = items.data.count(_.dustKind.contains(kind))

  def hasRoomForDust(kind: MaterialKind): Boolean = dustCount(kind) < MaterialKind.MaxDustPerKind

  def freeSilverSpace: Long = (Barrel.MaxSilver - silver).max(0L)
}

object Barrel {
  val MaxItems: Long  = 100L
  val MaxSilver: Long = 100000L

  def empty(heroId: HeroId): Barrel =
    Barrel(id = 0L, heroId = heroId, items = Items(Nil), silver = 0L)
}
