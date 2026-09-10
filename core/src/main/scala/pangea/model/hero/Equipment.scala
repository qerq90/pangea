package pangea.model.hero

import doobie.Meta
import doobie.postgres.circe.jsonb.implicits.{pgDecoderGet, pgEncoderPut}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import pangea.model.item.{Item, ItemType}

case class Equipment(
  helmet: Item,
  shoulderPads: Item,
  chestPlate: Item,
  bracelets: Item,
  gloves: Item,
  pants: Item,
  boots: Item,
  amulet: Item,
  firstRing: Item,
  secondRing: Item,
  belt: Item,
  flask: Item,
  weapon: Item,
  additionalWeapon: Item
) {
  def allArmor: Long =
    helmet.armor + shoulderPads.armor + chestPlate.armor + bracelets.armor +
    gloves.armor + pants.armor + boots.armor + amulet.armor +
    firstRing.armor + secondRing.armor + belt.armor + flask.armor +
    weapon.armor + additionalWeapon.armor

  // Суммарная прибавка к HP от экипировки (сейчас её даёт только нагрудник)
  // Суммарная прибавка к максимуму Энергии от экипировки (слагаемое «с экипировки»
  // в Hero.maxEnergy).
  def allEnergy: Long =
    helmet.energy + shoulderPads.energy + chestPlate.energy + bracelets.energy +
    gloves.energy + pants.energy + boots.energy + amulet.energy +
    firstRing.energy + secondRing.energy + belt.energy + flask.energy +
    weapon.energy + additionalWeapon.energy

  def allHp: Long =
    helmet.hp + shoulderPads.hp + chestPlate.hp + bracelets.hp +
    gloves.hp + pants.hp + boots.hp + amulet.hp +
    firstRing.hp + secondRing.hp + belt.hp + flask.hp +
    weapon.hp + additionalWeapon.hp

  /** Все надетые предметы (включая пустые слоты) — единый список для агрегаций,
   *  которым не важен конкретный слот (пассивки, будущие суммы статов). */
  def allItems: List[Item] = List(
    helmet, shoulderPads, chestPlate, bracelets, gloves, pants, boots, amulet,
    firstRing, secondRing, belt, flask, weapon, additionalWeapon
  )

  /** Ставит предмет в его же слот вместо прежнего: вещь та же самая, но
    * изменилась — например из её гнезда выломали камень. Кольца различаются по
    * id; надеванием это НЕ является — статы пересчитывает вызывающий. */
  def replacing(item: pangea.model.item.Item): Equipment = item.itemType match {
    case pangea.model.item.ItemType.Helmet           => copy(helmet = item)
    case pangea.model.item.ItemType.ShoulderPads     => copy(shoulderPads = item)
    case pangea.model.item.ItemType.ChestPlate       => copy(chestPlate = item)
    case pangea.model.item.ItemType.Bracelets        => copy(bracelets = item)
    case pangea.model.item.ItemType.Gloves           => copy(gloves = item)
    case pangea.model.item.ItemType.Pants            => copy(pants = item)
    case pangea.model.item.ItemType.Leggings         => copy(pants = item)
    case pangea.model.item.ItemType.Boots            => copy(boots = item)
    case pangea.model.item.ItemType.Amulet           => copy(amulet = item)
    case pangea.model.item.ItemType.Ring             =>
      if (firstRing.id == item.id) copy(firstRing = item) else copy(secondRing = item)
    case pangea.model.item.ItemType.Belt             => copy(belt = item)
    case pangea.model.item.ItemType.Flask            => copy(flask = item)
    case pangea.model.item.ItemType.Weapon           => copy(weapon = item)
    case pangea.model.item.ItemType.AdditionalWeapon => copy(additionalWeapon = item)
    case _                                           => this
  }

  /** Набор пассивок с надетых предметов. Множество само схлопывает дубли —
   *  «работает только одна» (см. [[pangea.model.item.PassiveKind]]). */
  def passiveKinds: Set[pangea.model.item.PassiveKind] =
    allItems.flatMap(_.passive).toSet

  /** Двенадцать слотов, которые считаются в наборы. Фляга и доп. оружие в них не
   *  входят, поэтому полный набор — ровно 12 предметов (см. [[pangea.model.item.ItemSet]]). */
  def setSlots: List[Item] = List(
    helmet, shoulderPads, chestPlate, bracelets, gloves, pants, boots, amulet,
    firstRing, secondRing, belt, weapon
  )

  /** Сколько надето предметов каждого набора (учитываются только сетовые слоты). */
  def setCounts: Map[pangea.model.item.ItemSet, Int] =
    setSlots.flatMap(_.set).groupBy(identity).map { case (s, xs) => s -> xs.size }

  /** Камни в гнёздах оружия (основного и дополнительного) — дают «оружейную» грань. */
  def weaponGems: List[pangea.model.item.Gem] =
    weapon.socketedGems ++ additionalWeapon.socketedGems

  /** Камни в гнёздах остального снаряжения (всё, кроме оружия) — дают «броневую»
   *  грань. Стакаются (в отличие от пассивок), поэтому список, а не множество. */
  def armorGems: List[pangea.model.item.Gem] =
    List(helmet, shoulderPads, chestPlate, bracelets, gloves, pants, boots, amulet,
      firstRing, secondRing, belt, flask).flatMap(_.socketedGems)

  /** Какие предметы из снаряжения занимают слот данного типа. Для Ring возвращает
   *  оба кольца (есть два слота); для прочих типов — один. Пустые слоты включены —
   *  фильтрацию по [[Item.itemType]] делает вызывающий. */
  def equippedFor(itemType: ItemType): List[Item] = itemType match {
    case ItemType.Helmet           => List(helmet)
    case ItemType.ShoulderPads     => List(shoulderPads)
    case ItemType.ChestPlate       => List(chestPlate)
    case ItemType.Bracelets        => List(bracelets)
    case ItemType.Gloves           => List(gloves)
    case ItemType.Pants            => List(pants)
    case ItemType.Leggings         => List(pants)
    case ItemType.Boots            => List(boots)
    case ItemType.Amulet           => List(amulet)
    case ItemType.Ring             => List(firstRing, secondRing)
    case ItemType.Belt             => List(belt)
    case ItemType.Flask            => List(flask)
    case ItemType.Weapon           => List(weapon)
    case ItemType.AdditionalWeapon => List(additionalWeapon)
    case ItemType.Trophy           => Nil
    case ItemType.TreasureMap      => Nil
    case ItemType.TreasureMapHalf  => Nil
    case ItemType.Gem              => Nil
    case ItemType.Material         => Nil
    case ItemType.NoItem           => Nil
  }
}

object Equipment {
  implicit val encoder: Encoder[Equipment] = deriveEncoder[Equipment]
  implicit val decoder: Decoder[Equipment] = deriveDecoder[Equipment]

  implicit val meta: Meta[Equipment] = new Meta(pgDecoderGet, pgEncoderPut)
}
