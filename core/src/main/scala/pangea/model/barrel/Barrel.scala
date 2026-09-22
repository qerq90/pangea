package pangea.model.barrel

import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.Items
import pangea.model.item.Item

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

  /** Сколько в бочке лежит того, что занимает место: пыль и малые руны не в счёт. */
  def occupied: Long = items.data.count(!_.weightless).toLong

  def freeSlots: Long = (Barrel.MaxItems - occupied).max(0L)

  /** Сколько такого же невесомого добра уже в бочке — у него свой предел вместо места. */
  def hoardCount(key: String): Int = items.data.count(_.hoardKey.contains(key))

  def hasRoomForHoard(item: Item): Boolean =
    item.hoardKey.forall(k => hoardCount(k) < Item.HoardLimit)

  def freeSilverSpace: Long = (Barrel.MaxSilver - silver).max(0L)
}

object Barrel {
  val MaxItems: Long  = 100L
  val MaxSilver: Long = 100000L

  def empty(heroId: HeroId): Barrel =
    Barrel(id = 0L, heroId = heroId, items = Items(Nil), silver = 0L)
}
