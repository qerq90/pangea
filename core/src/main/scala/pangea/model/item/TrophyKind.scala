package pangea.model.item

import enumeratum._
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax.EncoderOps

sealed trait TrophyKind extends EnumEntry {
  val displayName: String
  val coef: Double
}

object TrophyKind extends Enum[TrophyKind] {

  val values = findValues

  case object Relic extends TrophyKind {
    override val displayName: String = "Реликвия"
    override val coef: Double        = 4.0
  }

  case object Talisman extends TrophyKind {
    override val displayName: String = "Талисман"
    override val coef: Double        = 2.0
  }

  case object Head extends TrophyKind {
    override val displayName: String = "Голова"
    override val coef: Double        = 1.0
  }

  case object Sack extends TrophyKind {
    override val displayName: String = "Мешок с пожитками"
    override val coef: Double        = 0.5
  }

  implicit val encoder: Encoder[TrophyKind] = (k: TrophyKind) => k.entryName.asJson
  implicit val decoder: Decoder[TrophyKind] = (c: HCursor) =>
    c.as[String].map(TrophyKind.withName)
}
