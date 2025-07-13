package pangea.model.vk.keyboard

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax.EncoderOps

case class Keyboard(
  buttons: List[List[Button]],
  one_time: Boolean,
  inline: Boolean
) {
  def getJson: String = this.asJson.toString

  def withOneTime(oneTime: Boolean): Keyboard = this.copy(one_time = oneTime)
  def withInline(inline: Boolean): Keyboard   = this.copy(inline = inline)
  def addRow(): Keyboard =
    this.copy(buttons = this.buttons.appended(List.empty))

  def addButton(button: Button): Keyboard =
    if (this.buttons.last.length == 5) this
    else {
      val newList = this.buttons.last.appended(button)
      this.copy(buttons = this.buttons.dropRight(1).appended(newList))
    }
}

object Keyboard {
  val default: Keyboard = Keyboard(List.empty, one_time = true, inline = false)

  implicit val encoder: Encoder[Keyboard] = deriveEncoder[Keyboard]
}
