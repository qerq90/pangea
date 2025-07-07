package pangea.model.item

import enumeratum._

sealed trait ItemType extends EnumEntry

object ItemType extends Enum[ItemType] with DoobieEnum[ItemType] {

  val values = findValues

  case object Helmet           extends ItemType
  case object ShoulderPads     extends ItemType
  case object ChestPlate       extends ItemType
  case object Bracelets        extends ItemType
  case object Gloves           extends ItemType
  case object Pants            extends ItemType
  case object Leggings         extends ItemType
  case object Amulet           extends ItemType
  case object Ring             extends ItemType
  case object Belt             extends ItemType
  case object Flask            extends ItemType
  case object Weapon           extends ItemType
  case object AdditionalWeapon extends ItemType

}
