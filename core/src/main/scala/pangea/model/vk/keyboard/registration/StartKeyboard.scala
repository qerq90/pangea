package pangea.model.vk.keyboard.registration

import pangea.model.vk.keyboard.Action.{payloadEmpty, Text}
import pangea.model.vk.keyboard.{Button, Keyboard}

object StartKeyboard {
  val keyboard: Keyboard = Keyboard.default
    .withInline(false)
    .withOneTime(true)
    .addRow()
    .addButton(Button.withAction(Text("Начать", """{ "start": 1 }""")))
}
