package pangea.model.item

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

sealed trait ItemType extends EnumEntry

object ItemType extends Enum[ItemType] {

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

  case object NoItem extends ItemType

  implicit val encoder: Encoder[ItemType] =
    (itemType: ItemType) => itemType.entryName.asJson

  implicit val decoder: Decoder[ItemType] =
    (json: HCursor) => json.as[String].map(s => ItemType.withName(s))
}
