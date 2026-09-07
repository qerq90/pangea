package pangea.model.barrel

import pangea.model.hero.HeroId
import pangea.model.inventory.Inventory.Items
import pangea.model.item.Item

/** Личная неприметная бочка игрока в Портовом квартале: хранилище до 10 предметов
 *  и до 10 000 серебра, независимое от инвентаря и кошелька героя.
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

  def freeSlots: Long = (Barrel.MaxItems - items.data.length).max(0L)

  def freeSilverSpace: Long = (Barrel.MaxSilver - silver).max(0L)
}

object Barrel {
  val MaxItems: Long  = 10L
  val MaxSilver: Long = 10000L

  def empty(heroId: HeroId): Barrel =
    Barrel(id = 0L, heroId = heroId, items = Items(Nil), silver = 0L)
}
