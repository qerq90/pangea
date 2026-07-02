package pangea.model.item

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

sealed trait ItemType extends EnumEntry

object ItemType extends Enum[ItemType] {

  val values = findValues

  val itemTypes: List[ItemType] = values.filter(_ != NoItem).toList

  val attackItems: List[ItemType] =
    List(
      Weapon,
      Boots,
      Helmet,
      ShoulderPads,
      Gloves,
      Ring,
      Amulet,
      Bracelets,
      Leggings
    )
  val defenceItems: List[ItemType] =
    List(Helmet, ShoulderPads, ChestPlate, Pants, Boots, Belt, Leggings)

  // Типы, которые не надеваются: трофеи и карты клада. Пусто (NoItem) сюда тоже
  // относится, но оно и так исключено из itemTypes.
  private val nonEquippable: Set[ItemType] = Set(Trophy, TreasureMap, TreasureMapHalf)

  /** Надеваемые типы — всё, кроме трофеев, карт клада и NoItem. Экраны, где
   *  предмет предлагается к надеванию, опираются на этот список. */
  val equippable: List[ItemType] = itemTypes.filterNot(nonEquippable.contains)

  case object Helmet           extends ItemType
  case object ShoulderPads     extends ItemType
  case object ChestPlate       extends ItemType
  case object Bracelets        extends ItemType
  case object Gloves           extends ItemType
  case object Pants            extends ItemType
  case object Boots            extends ItemType
  case object Leggings         extends ItemType
  case object Amulet           extends ItemType
  case object Ring             extends ItemType
  case object Belt             extends ItemType
  case object Flask            extends ItemType
  case object Weapon           extends ItemType
  case object AdditionalWeapon extends ItemType

  // Трофей с убитого моба: лежит в инвентаре как обычный предмет (раса + уровень),
  // но не экипируется — не входит в attackItems/defenceItems.
  case object Trophy extends ItemType

  // Карта клада и её половинка: лежат в инвентаре, занимают слот, не надеваются.
  case object TreasureMap     extends ItemType
  case object TreasureMapHalf extends ItemType

  case object NoItem extends ItemType

  implicit val encoder: Encoder[ItemType] =
    (itemType: ItemType) => itemType.entryName.asJson

  implicit val decoder: Decoder[ItemType] =
    (json: HCursor) => json.as[String].map(s => ItemType.withName(s))
}
