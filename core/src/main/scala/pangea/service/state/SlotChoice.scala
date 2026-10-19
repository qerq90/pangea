package pangea.service.state

import pangea.engine.{Choice, SceneContent, Screen}
import pangea.model.hero.Equipment
import pangea.model.item.{Item, ItemType}
import pangea.service.state.states.{EquipmentState, InventoryState}

/** Оба кольца или обе руки заняты: новая вещь встаёт только вместо одной из них,
  * и что снять, спрашиваем у игрока. Экран общий для инвентаря и «Забрать и
  * надеть»; куда ведут кнопки, решает вызывающий: `action` с его `data` и номером
  * слота (`slot`: 1 и 2 — кольцо 1 и 2, правая и левая рука). */
object SlotChoice {

  /** Нужно ли спрашивать: кольцо при двух надетых, одноручное или щит при двух
    * занятых руках. Двуручное снимает обе — спрашивать нечего. */
  def needed(eq: Equipment, item: Item): Boolean =
    (item.itemType == ItemType.Ring && InventoryState.ringSlotsFull(eq)) ||
      ((item.itemType == ItemType.Weapon || item.itemType == ItemType.Shield) && !item.twoHanded && eq.handsFull)

  /** Экран выбора: надетое в обоих слотах для сравнения, по кнопке на слот и
    * последней — `back`. */
  def screen(content: SceneContent, item: Item, eq: Equipment, action: String,
             data: Map[String, String], back: Choice): Screen =
    if (item.itemType == ItemType.Ring)
      build(content.format("inventory.ringChoice", "name" -> item.displayTitle),
        List(eq.firstRing -> "Слот 1", eq.secondRing -> "Слот 2"),
        (slot, worn) => content.format("inventory.ringSlot", "slot" -> slot.toString, "name" -> worn.displayTitle),
        action, data, back)
    else
      build(content.format("inventory.handChoice", "name" -> item.displayTitle),
        List(eq.weapon -> EquipmentState.RightHandName, eq.leftHand -> EquipmentState.LeftHandName),
        (slot, worn) => content.format("inventory.handSlot",
          "hand" -> (if (slot == 1) EquipmentState.RightHandName else EquipmentState.LeftHandName),
          "name" -> worn.displayTitle),
        action, data, back)

  private def build(header: String, slots: List[(Item, String)], label: (Int, Item) => String,
                    action: String, data: Map[String, String], back: Choice): Screen = {
    val worn    = slots.map { case (item, title) => item.equippedComparison(title) }
    val buttons = slots.zipWithIndex.map { case ((item, _), i) =>
      Choice(
        id    = action,
        label = ItemMenu.truncate(label(i + 1, item)),
        data  = data + ("slot" -> (i + 1).toString),
        row   = Some(i)
      )
    }
    Screen(header + "\n\n" + worn.mkString("\n" + Item.ComparisonSeparator + "\n"), buttons :+ back.copy(row = Some(2)))
  }
}
