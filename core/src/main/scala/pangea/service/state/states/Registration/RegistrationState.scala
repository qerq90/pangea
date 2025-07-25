package pangea.service.state.states.Registration

import io.circe.jawn.decode
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.state.StateType.Registration
import pangea.model.user.User
import pangea.service.sender.Sender
import pangea.service.state.states.Registration.keyboard.{
  RaceDescriptionKeyboard,
  RaceKeyboard,
  StartKeyboard
}
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

case class RegistrationState(sender: Sender) extends State {

  // never gonna be used
  override def enter(): Task[Unit] = ZIO.unit

  private def matchUserAction(action: UserAction): Action =
    action.payload match {
      case Some(payload) => decode[Action](payload).toOption.get
      case None          => Action.Text
    }

  override def action(user: User, action: UserAction): Task[StateType] =
    matchUserAction(action) match {
      case Action.Travel =>
        sender.sendMessage(user, "SPS", List.empty, None).as(Registration)
      case Action.RaceDescription =>
        getRaceDescription(user, Race.withNameOption(action.text))
      case Action.Race => getRace(user)
      case _ =>
        for {
          _ <- sender.sendMessage(
            user,
            "Добро пожаловать на Пангею! Для того что бы приступить, отправьте слово «Начать»\nЭто текстовая онлайн-ммо- игра «Клеймо Пангеи», все действия в ней происходят при использовании команд под окном чата, результаты этих действий приходят в виде ответа от сообщества. В игре представлено большое число вариативности и уникальности вашего игрового пути к прохождению всей игровой компании. Так же не забывайте взаимодействовать с другими игроками для облегчения своей игры.\nкартинка c приветствием\nПрежде всего, проверим управление. Вы видите кнопки внизу?\n\n⚠Если у Вас нет кнопок для управления под окном чата - проверьте, что они не скрыты (кнопка \"показать/скрыть\" в строке набора текста в мобильном приложении, покатана на скрине), а также что используется последняя версия официального мобильного приложения \"Вконтакте\", либо обычный браузер.",
            List.empty,
            Some(StartKeyboard.keyboard)
          )
        } yield Registration
    }

  private def getRaceDescription(
      user: User,
      race: Option[Race]
  ): Task[StateType] =
    race match {
      case Some(race) =>
        sender
          .sendMessage(
            user,
            race.description,
            List.empty,
            Some(RaceDescriptionKeyboard.keyboard(race))
          )
          .as(Registration)

      case None =>
        sender
          .sendMessage(
            user,
            "Теперь, выберите свою расу. Позже её можно будет сменить.",
            List.empty,
            Some(RaceKeyboard.keyboard)
          )
          .as(Registration)
    }

  private def getRace(user: User): Task[StateType] =
    for {
      _ <- sender.sendMessage(
        user,
        "Теперь, выберите свою расу. Позже её можно будет сменить.",
        List.empty,
        Some(RaceKeyboard.keyboard)
      )
    } yield Registration
}
