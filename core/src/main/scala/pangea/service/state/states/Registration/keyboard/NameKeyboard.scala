package pangea.service.state.states.Registration.keyboard

import pangea.model.vk.keyboard.Action.Text
import pangea.model.vk.keyboard.{Button, Keyboard}
import pangea.service.state.states.Registration.Action

object NameKeyboard {
  def keyboard(name: String): Keyboard =
    Keyboard.default
      .addRow()
      .addButton(Button.withAction(Text(name, Some(Action.Travel8.json))))
}
