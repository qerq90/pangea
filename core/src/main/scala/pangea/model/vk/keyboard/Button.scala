package pangea.model.vk.keyboard

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

case class Button(action: Action, color: ButtonColor)

object Button {
  implicit val encoder: Encoder[Button] = deriveEncoder[Button]
}
