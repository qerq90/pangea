package pangea.engine

// Платформо-независимый цвет кнопки. Адаптер (VkRenderer) маппит его в свой формат.
sealed trait ChoiceColor

object ChoiceColor {
  case object Primary   extends ChoiceColor
  case object Secondary extends ChoiceColor
  case object Negative  extends ChoiceColor
  case object Positive  extends ChoiceColor

  def fromString(s: String): ChoiceColor = s.trim.toLowerCase match {
    case "secondary" => Secondary
    case "negative"  => Negative
    case "positive"  => Positive
    case _           => Primary
  }
}
