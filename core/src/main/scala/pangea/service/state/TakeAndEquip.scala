package pangea.service.state

import pangea.dao.hero.HeroDao
import pangea.engine.SceneContent
import pangea.model.hero.{Equipment, Hero}
import pangea.model.inventory.Inventory
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.states.InventoryState
import zio.{Task, ZIO}

/** «Забрать и надеть»: только что полученная вещь сразу встаёт на героя — с
  * добычи, с находки в лабиринте, с прилавка Ришелье. Сама она в сумку не
  * заходит, так что в пустой слот её можно надеть и при полной сумке. Вместо
  * надетой — тоже можно: снятое уходит в сумку, и место под него должно быть
  * (`noRoom`); какое из двух колец или какую руку освободить, спрашивает
  * [[SlotChoice]]. */
object TakeAndEquip {

  /** Что вышло из надевания: герой после него и снятое — оно ушло в сумку. */
  final case class Worn(hero: Hero, removed: List[Item])

  /** Встаёт ли вещь в пустой слот героя: её можно надеть, уровень по силам, и
    * слот свободен — у кольца любой из двух, у одноручного и щита свободная рука
    * (и в другой не двуручное), у двуручного обе руки. Фляга идёт на пояс, роза —
    * в ячейку пояса: их не надевают. */
  def fits(hero: Hero, item: Item): Boolean =
    canWear(hero, item) && emptySlotFor(hero.equipment, item)

  /** Встаёт ли вещь вместо надетой: надеть можно, а пустого слота под неё нет. */
  def replaces(hero: Hero, item: Item): Boolean =
    canWear(hero, item) && !emptySlotFor(hero.equipment, item)

  /** Можно ли надеть сразу — в пустой слот или вместо надетого. */
  def canWear(hero: Hero, item: Item): Boolean =
    item.lvl <= hero.lvl && ItemType.equippable.contains(item.itemType) &&
      item.itemType != ItemType.Flask && item.rose.isEmpty

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
        if (fits(h, item)) (put(h, item, None)._1, picked :+ i) else (h, picked)
      }._2

  /** Почему надеть вместо надетого нельзя — ключ текста, или None, если можно.
    * Новая вещь в сумку не заходит, а снятое ложится туда: место нужно на каждую
    * снятую вещь (двуручное поверх двух одноручных снимает обе). Замена, снимающая
    * последний «Тайник», сумку ещё и ужимает. `slot` — как в [[wear]]. */
  def noRoom(hero: Hero, inv: Inventory, item: Item, slot: Option[Int]): Option[String] = {
    val (worn, removed) = put(hero, item, slot)
    val capDelta        = stashDelta(hero, worn)
    if (InventoryState.fitsAfterCapacityChange(inv, capDelta, removed.size)) None
    else if (InventoryState.fitsAfterCapacityChange(inv, 0L, removed.size)) Some("equipment.stashBlocked")
    else Some("common.swapNoRoom")
  }

  /** Надеть только что полученные вещи (уже с id) по порядку: снаряжение, статы,
    * снятое — в сумку, места сумки от «Тайника». `slot` — какое из двух колец
    * или какую руку освободить, когда заняты обе (как в инвентаре: 1 и 2), None —
    * обычное правило слотов; он для одной вещи. Место под снятое проверяют до
    * этого (`noRoom`). Достижение за собранный набор вызывающий выдаёт сам, после
    * своей строки о получении (`QuestSupport.grantFullSets`), — как обычное
    * «Надеть». */
  def wear(heroDao: HeroDao, inventoryRepo: InventoryRepository, user: User, hero: Hero,
           items: List[Item], slot: Option[Int] = None): Task[Worn] =
    if (items.isEmpty) ZIO.succeed(Worn(hero, Nil))
    else {
      val (worn, removed) = items.foldLeft((hero, List.empty[Item])) { case ((h, out), item) =>
        val (next, gone) = put(h, item, slot)
        (next, out ++ gone)
      }
      val capDelta = stashDelta(hero, worn)
      heroDao.updateEquipmentAndFightStats(user.userId, worn.equipment, worn.fightStats) *>
        // Места от нового «Тайника» — до того, как снятое ляжет в сумку, а
        // потеря последнего — после.
        ZIO.when(capDelta > 0)(inventoryRepo.increaseCapacity(hero.id, capDelta).orElse(ZIO.unit)) *>
        ZIO.foreachDiscard(removed)(old => inventoryRepo.addItem(hero.id, old).ignore) *>
        ZIO.when(capDelta < 0)(inventoryRepo.increaseCapacity(hero.id, capDelta).orElse(ZIO.unit)) *>
        ZIO.succeed(Worn(worn, removed))
    }

  /** Строка «🎒 Снято в сумку: …» с переводом строки впереди; ничего не снято —
    * пусто. */
  def takenOffLine(content: SceneContent, worn: Worn): String =
    if (worn.removed.isEmpty) ""
    else "\n" + content.format("common.takenOff", "old" -> worn.removed.map(_.name).mkString(", "))

  private def put(hero: Hero, item: Item, slot: Option[Int]): (Hero, List[Item]) = {
    val (eq, fight, removed) = InventoryState.equip(hero, item, slot)
    (hero.copy(equipment = eq, fightStats = fight), removed)
  }

  private def stashDelta(before: Hero, after: Hero): Long =
    InventoryState.equipmentStashDelta(before.equipment, after.equipment, before.runes.brandedPassives.toSet)
}
