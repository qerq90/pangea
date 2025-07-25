package pangea.service.state.states.Registration.keyboard

import pangea.model.monster.Race
import pangea.model.vk.keyboard.Action.Text
import pangea.model.vk.keyboard.{Button, Keyboard}
import pangea.service.state.states.Registration.Action

object RaceKeyboard {
  val keyboard: Keyboard = Keyboard.default
    .withInline(false)
    .withOneTime(true)
    .addRow()
    .addButton(button(Race.Demon))
    .addButton(button(Race.Elf))
    .addButton(button(Race.Gnome))
    .addRow()
    .addButton(button(Race.Goblin))
    .addButton(button(Race.Human))
    .addButton(button(Race.Khajiit))
    .addRow()
    .addButton(button(Race.Murloc))
    .addButton(button(Race.Orc))

  private def button(race: Race): Button =
    Button.withAction(Text(race.toString, Some(Action.RaceDescription.json)))
}
