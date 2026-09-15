package pangea.model.item

import enumeratum._
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax.EncoderOps

sealed trait TrophyKind extends EnumEntry {
  val displayName: String

  /** Коэффициент репутации за трофей: `5 + Ур. × coef`. У клыка Белого волка он
   *  зависит от уровня босса и хранится на самом предмете
   *  (см. [[ItemDetails.Trophy.coefValue]]); здесь — ставка за один BossLvL. */
  val coef: Double

  /** Описание для инвентаря; у обычных трофеев его нет. */
  val description: String = ""
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

  /** Клык Белого волка. В обычном дропе не участвует — падает только с волка;
   *  коэффициент 6 за каждый BossLvL, уровень — этаж, где волк встретился. */
  case object Fang extends TrophyKind {
    override val displayName: String = "Клык Белого волка"
    override val coef: Double        = 6.0
    override val description: String =
      "Большой и очень острый клык некогда принадлежавший не менее смертоносному чудовищу."
  }

  implicit val encoder: Encoder[TrophyKind] = (k: TrophyKind) => k.entryName.asJson
  implicit val decoder: Decoder[TrophyKind] = (c: HCursor) =>
    c.as[String].map(TrophyKind.withName)
}
