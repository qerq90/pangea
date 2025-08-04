package pangea.generator.item

import pangea.model.item.{Item, ItemType, Rarity}
import pangea.util.ListOps
import scala.util.Random

object ItemGenerator {
  private val random = new Random()
  private val id     = -1

  private def getModifiedLvl(lvl: Long): Long = {
    var scaledLvl = lvl - random.between(0, 7) + 1
    if (scaledLvl > 150) scaledLvl = 150
    if (scaledLvl < 0) scaledLvl = 1

    scaledLvl
  }

  private def modifyParameter(param: Double): Long = {
    val randomPercent: Long = random.between(-10, 11)
    val percentValue        = (param / 100.0) * randomPercent
    (param + percentValue).toLong
  }

  def createItem(lvl: Long, rarity: Rarity): Item = {
    val itemLvl             = getModifiedLvl(lvl)
    val numberOfExtraParams = rarity.getNumOfExtraParams

    val item = if (random.nextBoolean()) { // attack
      val itemType      = ItemType.attackItems.random
      val attack        = 0
      val accuracy      = 0
      val armor         = modifyParameter(rarity.factorR * itemLvl)
      val defence       = modifyParameter(rarity.factorR1 * itemLvl)
      val evasion       = 0
      val concentration = 0
      Item(
        id,
        "default attack item name",
        itemType,
        attack,
        accuracy,
        concentration,
        armor,
        defence,
        evasion
      )
    } else { // defence
      val itemType      = ItemType.defenceItems.random
      val attack        = modifyParameter(rarity.factorR * itemLvl)
      val accuracy      = 0
      val armor         = 0
      val concentration = 0
      val defence       = 0
      val evasion       = modifyParameter(rarity.factorR1 * itemLvl)
      Item(
        id,
        "default defence item name",
        itemType,
        attack,
        accuracy,
        concentration,
        armor,
        defence,
        evasion
      )
    }

    item // TODO add more stats based on numOfExtraStats
  }
}
