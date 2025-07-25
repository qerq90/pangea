package pangea.service.state.states.Registration.keyboard

import io.circe.syntax.EncoderOps
import pangea.model.monster.Race
import pangea.model.vk.keyboard.Action.Text
import pangea.model.vk.keyboard.{Button, Keyboard}
import pangea.service.state.states.Registration.Action

object RaceDescriptionKeyboard {
  def keyboard(race: Race): Keyboard =
    Keyboard.default
      .withInline(false)
      .withOneTime(true)
      .addRow()
      .addButton(
        Button.withAction(
          Text(
            "Подтвердить расу",
            Some(
              Action.Travel.json.asObject.get
                .+:("race", race.entryName.asJson)
                .toJson
            )
          )
        )
      )
      .addButton(
        Button.withAction(
          Text("Вернуться к выбору расы", Some(Action.Race.json))
        )
      )

}
