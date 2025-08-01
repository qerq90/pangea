package pangea.service.state.states.dungeon.keyboard

import pangea.model.vk.keyboard.Action.Text
import pangea.model.vk.keyboard.{Button, Keyboard}
import pangea.service.state.states.dungeon.Action

object EnterKeyboard {
  val keyboard: Keyboard = Keyboard.default
    .addRow()
    .addButton(
      Button.withAction(
        Text("Отправиться на поиски", Some(Action.FindEvent.json))
      )
    )
}
