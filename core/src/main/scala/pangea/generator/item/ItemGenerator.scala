package pangea.generator.item

import pangea.domain.Rng
import pangea.model.item.stats.Stat
import pangea.model.item.{Item, ItemDetails, ItemType, PotionKind, Rarity}
import pangea.model.skill.Skill

import scala.annotation.tailrec

object ItemGenerator {
  private val id = -1

  private def getModifiedLvl(lvl: Long, rng: Rng): (Long, Rng) = {
    val (delta, next) = rng.between(0L, 7L)
    val scaled        = (lvl - delta + 1).max(1L).min(150L)
    (scaled, next)
  }

  private def modifyParameter(param: Double, rng: Rng): (Long, Rng) =
    modifySpread(param, rng, 10L)

  // param ± spread% (равномерный разброс)
  private def modifySpread(
      param: Double,
      rng: Rng,
      spread: Long
  ): (Long, Rng) = {
    val (pct, next) = rng.between(-spread, spread + 1L)
    ((param + param / 100.0 * pct).toLong, next)
  }

  // Обязательные прибавки: оружию (Weapon) — к атаке lvl×(4+R3) ±20%;
  // нагруднику (ChestPlate) — к HP персонажа lvl×(12+R3) ±10%. R3 = rarity.factorR3.
  private def applyMandatory(item: Item, rng: Rng): (Item, Rng) = {
    val r3 = item.rarity.factorR3
    item.itemType match {
      case ItemType.Weapon =>
        val (bonus, next) = modifySpread(item.lvl * (4.0 + r3), rng, 20L)
        (item.withAttack(item.attack + bonus), next)
      case ItemType.ChestPlate =>
        val (bonus, next) = modifySpread(item.lvl * (12.0 + r3), rng, 10L)
        (item.withHp(item.hp + bonus), next)
      case _ => (item, rng)
    }
  }

  @tailrec
  private def updateExtraParams(n: Long, item: Item, rng: Rng): (Item, Rng) =
    if (n <= 0) (item, rng)
    else {
      val (stat, rng1) = rng.pick(Stat.values.toList)
      val (modified, rng2) = stat match {
        case Stat.Attack =>
          val (v, r) =
            modifyParameter(item.rarity.factorR3 * 0.5 * item.lvl, rng1)
          (item.withAttack(item.attack + v), r)
        case Stat.Accuracy =>
          val (v, r) = modifyParameter(item.rarity.factorR3 * item.lvl, rng1)
          (item.withAccuracy(item.accuracy + v), r)
        case Stat.Energy =>
          val (v, r) =
            modifyParameter(item.rarity.factorR3 * 0.3 * item.lvl, rng1)
          (item.withEnergy(item.energy + v), r)
        case Stat.Armor =>
          val (v, r) =
            modifyParameter(item.rarity.factorR3 * 0.5 * item.lvl, rng1)
          (item.withArmor(item.armor + v), r)
        case Stat.Defence =>
          val (v, r) =
            modifyParameter(item.rarity.factorR3 * 0.5 * item.lvl, rng1)
          (item.withDefence(item.defence + v), r)
        case Stat.Evasion =>
          val (v, r) = modifyParameter(item.rarity.factorR3 * item.lvl, rng1)
          (item.withEvasion(item.evasion + v), r)
      }
      updateExtraParams(n - 1, modified, rng2)
    }

  def rarityForLevel(dungeonLevel: Int, rng: Rng): (Rarity, Rng) = {
    val (roll, next) = rng.between(0L, 100L)
    val rarity = dungeonLevel match {
      case l if l <= 15 =>
        if (roll < 60) Rarity.Gray
        else if (roll < 90) Rarity.White
        else Rarity.Green
      case l if l <= 35 =>
        if (roll < 25) Rarity.White
        else if (roll < 65) Rarity.Green
        else if (roll < 92) Rarity.Blue
        else Rarity.Purple
      case l if l <= 60 =>
        if (roll < 20) Rarity.Green
        else if (roll < 60) Rarity.Blue
        else if (roll < 88) Rarity.Purple
        else Rarity.Violet
      case l if l <= 100 =>
        if (roll < 10) Rarity.Blue
        else if (roll < 50) Rarity.Purple
        else if (roll < 80) Rarity.Violet
        else Rarity.Orange
      case _ =>
        if (roll < 20) Rarity.Purple
        else if (roll < 55) Rarity.Violet
        else Rarity.Orange
    }
    (rarity, next)
  }

  def createItem(lvl: Long, rarity: Rarity, rng: Rng): (Item, Rng) = {
    val (itemLvl, rng1) = getModifiedLvl(lvl, rng)
    buildAtLevel(itemLvl, rarity, rng1)
  }

  /** Создаёт предмет ровно на заданный уровень, без разброса (для магазина). */
  def createItemAtLevel(lvl: Long, rarity: Rarity, rng: Rng): (Item, Rng) =
    buildAtLevel(lvl.max(1L).min(150L), rarity, rng)

  private def buildAtLevel(
      itemLvl: Long,
      rarity: Rarity,
      rng: Rng
  ): (Item, Rng) = {
    val (numberOfExtraParams, rng2) = rarity.getNumOfExtraParams(rng)
    val (isAttack, rng3)            = rng2.nextBoolean

    val (item, rng4) =
      if (isAttack) {
        val (itemType, rng3a) = rng3.pick(ItemType.attackItems)
        val (armor, rng3b)    = modifyParameter(rarity.factorR * itemLvl, rng3a)
        val (defence, rng3c) = modifyParameter(rarity.factorR1 * itemLvl, rng3b)
        (
          Item(
            id,
            "?",
            itemLvl,
            rarity,
            itemType,
            attack = 0,
            accuracy = 0,
            energy = 0,
            armor = armor,
            defence = defence,
            evasion = 0
          ),
          rng3c
        )
      } else {
        val (itemType, rng3a) = rng3.pick(ItemType.defenceItems)
        val (attack, rng3b)   = modifyParameter(rarity.factorR * itemLvl, rng3a)
        val (evasion, rng3c) = modifyParameter(rarity.factorR1 * itemLvl, rng3b)
        (
          Item(
            id,
            "?",
            itemLvl,
            rarity,
            itemType,
            attack = attack,
            accuracy = 0,
            energy = 0,
            armor = 0,
            defence = 0,
            evasion = evasion
          ),
          rng3c
        )
      }

    val (withExtras, rng5) = updateExtraParams(numberOfExtraParams, item, rng4)
    val (withMandatory, rng5b) = applyMandatory(withExtras, rng5)
    val (withDetails, rng5c)   = applyTypeDetails(withMandatory, rng5b)
    val (name, rng6) =
      ItemNameGenerator.generate(withDetails.itemType, rarity, rng5c)
    (withDetails.withName(name), rng6)
  }

  // Спец-данные типа при генерации:
  //  - оружию — активный навык (один из weaponSkills) → ItemDetails.Weapon;
  //  - нагруднику — активный навык (один из armorSkills) → ItemDetails.Armor;
  //  - поясу — случайное зелье (равновероятно из PotionKind) с зарядами по
  //    редкости → ItemDetails.Belt.
  private def applyTypeDetails(item: Item, rng: Rng): (Item, Rng) =
    item.itemType match {
      case ItemType.Weapon =>
        val (skill, next) = rng.pick(Skill.weaponSkills)
        (item.copy(details = ItemDetails.Weapon(skill)), next)
      case ItemType.ChestPlate =>
        val (skill, next) = rng.pick(Skill.armorSkills)
        (item.copy(details = ItemDetails.Armor(skill)), next)
      case ItemType.Belt =>
        val (potion, next) = rng.pick(PotionKind.values.toList)
        val cap            = ItemDetails.Belt.capacityFor(item.rarity)
        (item.copy(details = ItemDetails.Belt(potion, cap, cap)), next)
      case _ => (item, rng)
    }
}
