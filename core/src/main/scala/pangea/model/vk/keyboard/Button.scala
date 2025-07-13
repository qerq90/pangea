package pangea.model.vk.keyboard

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import pangea.model.vk.keyboard.ButtonColor.Primary

case class Button(action: Action, color: ButtonColor) {
  def withColor(color: ButtonColor): Button = this.copy(color = color)
}

object Button {
  def withAction(action: Action): Button = Button(action, Primary)

  implicit val encoder: Encoder[Button] = deriveEncoder[Button]
}
