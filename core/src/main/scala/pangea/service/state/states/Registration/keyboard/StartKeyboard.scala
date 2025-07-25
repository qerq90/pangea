package pangea.service.state.states.Registration.keyboard

import pangea.model.vk.keyboard.Action.Text
import pangea.model.vk.keyboard.{Button, Keyboard}
import pangea.service.state.states.Registration.Action

object StartKeyboard {
  val keyboard: Keyboard = Keyboard.default
    .withInline(false)
    .withOneTime(true)
    .addRow()
    .addButton(Button.withAction(Text("Начать", Some(Action.Race.json))))
}
