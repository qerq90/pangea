package pangea.model.vk.keyboard

import enumeratum.EnumEntry.Lowercase
import enumeratum._
import io.circe.Encoder
import io.circe.syntax.EncoderOps

sealed trait ButtonColor extends EnumEntry with Lowercase

object ButtonColor extends Enum[ButtonColor] {

  val values = findValues

  case object Primary   extends ButtonColor
  case object Secondary extends ButtonColor
  case object Negative  extends ButtonColor
  case object Positive  extends ButtonColor

  implicit val encoder: Encoder[ButtonColor] =
    (buttonColor: ButtonColor) => buttonColor.entryName.asJson
}
