package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.model.hero.{Equipment, Hero}
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.states.InventoryState
import zio.{Task, ZIO}

/** «Забрать и надеть»: только что полученная вещь сразу встаёт на героя, если её
  * слот пуст, — с добычи, с находки в лабиринте, с прилавка Ришелье. В сумку она
  * не заходит, так что надеть можно и при полной сумке. Занятый слот не трогаем:
  * замена со всеми её вопросами (какое кольцо снять, хватит ли места снятому) —
  * в инвентаре. */
object TakeAndEquip {

  /** Встаёт ли вещь в пустой слот героя: её можно надеть, уровень по силам, и
    * слот свободен — у кольца любой из двух, у одноручного и щита свободная рука
    * (и в другой не двуручное), у двуручного обе руки. Фляга идёт на пояс, роза —
    * в ячейку пояса: их не надевают. */
  def fits(hero: Hero, item: Item): Boolean =
    item.lvl <= hero.lvl && wearable(item) && emptySlotFor(hero.equipment, item)

  private def wearable(item: Item): Boolean =
    ItemType.equippable.contains(item.itemType) && item.itemType != ItemType.Flask && item.rose.isEmpty

  private def emptySlotFor(eq: Equipment, item: Item): Boolean = {
    val free = eq.equippedFor(item.itemType).count(_.itemType == ItemType.NoItem)
    item.itemType match {
      case ItemType.Weapon | ItemType.Shield =>
        if (item.twoHanded) free == 2 else free > 0 && !eq.leftBusy
      case _ => free > 0
    }
  }

  /** Что из добычи надеть: вещи, которым есть пустой слот, — лучшие первыми
    * (редкость, потом уровень). Два шлема — надевается более редкий, второй
    * уходит в сумку. Номера вещей в `items` — в том порядке, в каком надевать. */
  def pick(hero: Hero, items: List[Item]): List[Int] =
    items.zipWithIndex
      .sortBy { case (item, _) => (-Rarity.order(item.rarity), -item.lvl) }
      .foldLeft((hero, List.empty[Int])) { case ((h, picked), (item, i)) =>
        if (fits(h, item)) (put(h, item)._1, picked :+ i) else (h, picked)
      }._2

  /** Надеть только что полученные вещи (уже с id) — каждую в её пустой слот, по
    * порядку: снаряжение, статы и места сумки от «Тайника». Достижение за
    * собранный набор вызывающий выдаёт сам, после своей строки о получении
    * (`QuestSupport.grantFullSets`), — как обычное «Надеть». */
  def wear(heroDao: HeroDao, inventoryRepo: InventoryRepository, user: User, hero: Hero,
           items: List[Item]): Task[Hero] =
    if (items.isEmpty) ZIO.succeed(hero)
    else {
      val (worn, removed) = items.foldLeft((hero, List.empty[Item])) { case ((h, out), item) =>
        val (next, gone) = put(h, item)
        (next, out ++ gone)
      }
      val capDelta = InventoryState.equipmentStashDelta(hero.equipment, worn.equipment, hero.runes.brandedPassives.toSet)
      heroDao.updateEquipmentAndFightStats(user.userId, worn.equipment, worn.fightStats) *>
        // В пустой слот ничего не снимается; а если слот всё же успел занять
        // кто-то другой — снятое уходит в сумку, а не в никуда.
        ZIO.foreachDiscard(removed)(old => inventoryRepo.addItem(hero.id, old).ignore) *>
        ZIO.when(capDelta != 0)(inventoryRepo.increaseCapacity(hero.id, capDelta).orElse(ZIO.unit)) *>
        ZIO.succeed(worn)
    }

  private def put(hero: Hero, item: Item): (Hero, List[Item]) = {
    val (eq, fight, removed) = InventoryState.equip(hero, item, None)
    (hero.copy(equipment = eq, fightStats = fight), removed)
  }
}
