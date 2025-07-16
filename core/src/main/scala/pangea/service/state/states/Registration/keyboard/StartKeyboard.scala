package pangea.service.state.states.Registration.keyboard

import pangea.model.vk.keyboard.Action.Text
import pangea.model.vk.keyboard.{Button, Keyboard}
import pangea.service.state.states.Registration.Actions

object StartKeyboard {
  val keyboard: Keyboard = Keyboard.default
    .withInline(false)
    .withOneTime(true)
    .addRow()
    .addButton(Button.withAction(Text("Начать", Actions.Start.entryName)))
}
