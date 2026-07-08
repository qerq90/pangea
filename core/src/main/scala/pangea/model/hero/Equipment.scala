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
    case ItemType.NoItem           => Nil
  }
}

object Equipment {
  implicit val encoder: Encoder[Equipment] = deriveEncoder[Equipment]
  implicit val decoder: Decoder[Equipment] = deriveDecoder[Equipment]

  implicit val meta: Meta[Equipment] = new Meta(pgDecoderGet, pgEncoderPut)
}
