package pangea.service.state.states.events.item.keyboard

import pangea.model.vk.keyboard.Action.Text
import pangea.model.vk.keyboard.{Button, Keyboard}
import pangea.service.state.states.events.item.Action

object FoundItemKeyboard {
  val keyboard: Keyboard = Keyboard.default
    .addRow()
    .addButton(Button.withAction(Text("Забрать", Some(Action.TakeItem.json))))
    .addButton(
      Button.withAction(Text("Оставить", Some(Action.DontTakeItem.json)))
    )
}
