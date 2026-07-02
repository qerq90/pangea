package pangea.model.item

import enumeratum._
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax.EncoderOps

/**
 * Зона карты клада. Уровень зоны определяется диапазоном уровней (1–25, 26–50 …
 * 126–150); имя целой карты и половинки, а также текст-описание — данные в enum
 * (как [[TrophyKind.displayName]]), не хранятся в БД. Описание есть только у
 * Кинэт — у остальных пока заглушка.
 */
sealed trait MapZone extends EnumEntry {
  /** Инклюзивный диапазон уровней героя, для которого генерируется эта карта. */
  def levels: Range

  /** Имя клада («Кинэт», «Корабль на дереве» …). */
  def treasureName: String

  /** Описание целой карты. */
  def description: String

  def mapName: String  = s"🗺 Карта клада $treasureName"
  def halfName: String = s"🗺 Половинка карты клада $treasureName"

  /** Описание, зависящее от типа предмета: у половинки — общая заглушка, у целой
   *  карты — описание зоны плюс подсказка «надо выйти За город» (осмотр карты). */
  def descriptionFor(itemType: ItemType): String = itemType match {
    case ItemType.TreasureMapHalf => MapZone.HalfDescription
    case _                        => s"$description\n\n${MapZone.InspectHint}"
  }
}

object MapZone extends Enum[MapZone] {

  val values = findValues

  /** Общий текст-заглушка для половинок: полное описание половинка не раскрывает. */
  val HalfDescription: String =
    "Это лишь половина карты клада. Чтобы прочесть её целиком, нужно найти вторую половину."

  /** Подсказка при осмотре целой карты: куда идти, чтобы отправиться за кладом. */
  val InspectHint: String =
    "Кажется отмеченное место находится на материке… Надо выйти За город."

  private def stub(name: String): String =
    s"Описание клада «$name» ещё не составлено."

  case object Kinet extends MapZone {
    override val levels: Range        = 1 to 25
    override val treasureName: String = "Кинэт"
    override val description: String =
      "«Карта явно свежая, и бумага еще не успела пожелтеть. Деревня, " +
        "расположенная недалеко от города Кинэт, нарисована впопыхах и с трудом " +
        "узнается, но, по обозначениям, клад спрятан в пещере правее самой деревни.»"
  }

  case object TreeShip extends MapZone {
    override val levels: Range        = 26 to 50
    override val treasureName: String = "Корабль на дереве"
    override val description: String  = stub(treasureName)
  }

  case object DeadmansGorge extends MapZone {
    override val levels: Range        = 51 to 75
    override val treasureName: String = "Ущелье мертвецов"
    override val description: String  = stub(treasureName)
  }

  case object GiantSkeleton extends MapZone {
    override val levels: Range        = 76 to 100
    override val treasureName: String = "Гигантский скелет"
    override val description: String  = stub(treasureName)
  }

  case object IceForest extends MapZone {
    override val levels: Range        = 101 to 125
    override val treasureName: String = "Ледяной Лес"
    override val description: String  = stub(treasureName)
  }

  case object AbandonedTemple extends MapZone {
    override val levels: Range        = 126 to 150
    override val treasureName: String = "Заброшенный Храм"
    override val description: String  = stub(treasureName)
  }

  /** Зона по уровню героя. Уровень клампится в [1, 150] и попадает ровно в одну
   *  зону (диапазоны сплошь покрывают весь интервал уровней). */
  def forLevel(lvl: Long): MapZone = {
    val clamped = lvl.max(1L).min(150L).toInt
    values.find(_.levels.contains(clamped)).getOrElse(values.last)
  }

  implicit val encoder: Encoder[MapZone] = (z: MapZone) => z.entryName.asJson
  implicit val decoder: Decoder[MapZone] = (c: HCursor) => c.as[String].map(MapZone.withName)
}
