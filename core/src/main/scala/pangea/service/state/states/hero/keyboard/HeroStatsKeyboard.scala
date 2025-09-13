package pangea.service.state.states.hero.keyboard

import pangea.model.vk.keyboard.Action.Text
import pangea.model.vk.keyboard.ButtonColor.Negative
import pangea.model.vk.keyboard.{Button, Keyboard}
import pangea.service.state.states.hero.Action

object HeroStatsKeyboard {
  def keyboard(upgradePoints: Long) =
    Keyboard.default
      .addRow()
      .addButton(
        Button
          .withAction(Text("Инвентарь", Some(???)))
      )
      .addButton(
        Button.withAction(Text("Снаряжение", Some(???)))
      )
      .addOptionalRow(upgradePoints > 0)
      .addOptionalButton(upgradePoints > 0)(
        Button.withAction(Text("Распределить характеристики", Some(???)))
      )
      .addRow()
      .addButton(
        Button
          .withAction(Text("Покинуть костер", Some(Action.Back.json)))
          .withColor(Negative)
      )
}
