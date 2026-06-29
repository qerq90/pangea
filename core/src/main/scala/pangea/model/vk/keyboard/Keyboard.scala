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

  def addOptionalRow(bool: Boolean): Keyboard =
    if (bool) addRow()
    else this

  def addButton(button: Button): Keyboard =
    if (this.buttons.last.length == 5) this
    else {
      val newList = this.buttons.last.appended(button)
      this.copy(buttons = this.buttons.dropRight(1).appended(newList))
    }

  def addOptionalButton(bool: Boolean)(button: Button): Keyboard =
    if (bool) addButton(button)
    else this
}

object Keyboard {
  /** Клавиатура с пустым `buttons: []` — стартовая заготовка для сборки рядов и
   *  одновременно «сигнальная» клавиатура, которой ВК скрывает предыдущие кнопки
   *  (см. [[pangea.engine.Screen.hideKeyboard]] и [[pangea.service.sender.vk.VkRenderer]]). */
  val empty: Keyboard = Keyboard(List.empty, one_time = false, inline = false)

  implicit val encoder: Encoder[Keyboard] = deriveEncoder[Keyboard]
}
