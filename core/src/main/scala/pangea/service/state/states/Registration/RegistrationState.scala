package pangea.service.state.states.Registration

import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.sender.Sender
import pangea.service.state.states.Registration.keyboard.StartKeyboard
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

case class RegistrationState(sender: Sender) extends State {

  // never gonna be used
  override def enter(): Task[Unit] = ZIO.unit

  private def matchUserAction(action: UserAction): Action =
    action.payload match {
      case Some(payload) => payload.as[Action].toOption.get
      case None          => Action.Text
    }

  override def action(user: User, action: UserAction): Task[StateType] =
    matchUserAction(action) match {
      case Action.Start => getStart(user, action)
      case Action.Text =>
        for {
          _ <- sender.sendMessage(
            user,
            "Добро пожаловать на Пангею! Для того что бы приступить, отправьте слово «Начать»\nЭто текстовая онлайн-ммо- игра «Клеймо Пангеи», все действия в ней происходят при использовании команд под окном чата, результаты этих действий приходят в виде ответа от сообщества. В игре представлено большое число вариативности и уникальности вашего игрового пути к прохождению всей игровой компании. Так же не забывайте взаимодействовать с другими игроками для облегчения своей игры.\nкартинка c приветствием\nПрежде всего, проверим управление. Вы видите кнопки внизу?\n\n⚠Если у Вас нет кнопок для управления под окном чата - проверьте, что они не скрыты (кнопка \"показать/скрыть\" в строке набора текста в мобильном приложении, покатана на скрине), а также что используется последняя версия официального мобильного приложения \"Вконтакте\", либо обычный браузер.",
            List.empty,
            Some(StartKeyboard.keyboard)
          )
        } yield StateType.Registration
    }

  private def getStart(user: User, action: UserAction): Task[StateType] =
    for {
      _ <- sender.sendMessage(
        user,
        "Спасибо за регистрацию, бро",
        List.empty,
        None
      )
    } yield StateType.Registration
}
