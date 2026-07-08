package pangea.model.item

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

/** Тип зелья на поясе. Пояс при генерации получает ровно одну «пассивку» из этого
 *  набора (равновероятно), а его вместимость (число зарядов) задаётся редкостью —
 *  см. [[Rarity.beltCapacity]]. В бою кнопка «Пояс» тратит один заряд и применяет
 *  эффект соответствующего вида. `label` используется в сообщениях и на экранах
 *  снаряжения. */
sealed abstract class PotionKind(val label: String) extends EnumEntry

object PotionKind extends Enum[PotionKind] {

  val values: IndexedSeq[PotionKind] = findValues

  case object Healing       extends PotionKind("Склянка лечения")
  case object Poison        extends PotionKind("Склянка яда")
  case object Evasion       extends PotionKind("Склянка уворота")
  case object Defence       extends PotionKind("Склянка защиты")
  case object Attack        extends PotionKind("Склянка атаки")
  case object Energy        extends PotionKind("Склянка энергии")
  case object Metal         extends PotionKind("Склянка металла")
  case object Regeneration  extends PotionKind("Склянка регенерации")

  implicit val encoder: Encoder[PotionKind] =
    (pk: PotionKind) => pk.entryName.asJson

  implicit val decoder: Decoder[PotionKind] =
    (c: HCursor) => c.as[String].map(PotionKind.withName)
}
