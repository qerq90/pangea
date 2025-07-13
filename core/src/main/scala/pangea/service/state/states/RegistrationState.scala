package pangea.service.state.states

import pangea.model.state.StateType
import pangea.model.user.User
import pangea.model.vk.keyboard.Keyboard
import pangea.model.vk.keyboard.registration.StartKeyboard
import pangea.service.sender.Sender
import pangea.service.state.State
import zio.{Task, ZIO}

case class RegistrationState(sender: Sender) extends State {

  // never gonna be used
  override def enter(): Task[Unit] = ZIO.unit

  override def action(user: User, action: String): Task[StateType] =
    action match {
      case _ =>
        for {
          _ <- sender.sendMessage(
            user,
            "Добро пожаловать на Пангею! Для того что бы приступить, отправьте слово «Начать»\nЭто текстовая онлайн-ммо- игра «Клеймо Пангеи», все действия в ней происходят при использовании команд под окном чата, результаты этих действий приходят в виде ответа от сообщества. В игре представлено большое число вариативности и уникальности вашего игрового пути к прохождению всей игровой компании. Так же не забывайте взаимодействовать с другими игроками для облегчения своей игры.\nкартинка c приветствием\nПрежде всего, проверим управление. Вы видите кнопки внизу?\n\n⚠Если у Вас нет кнопок для управления под окном чата - проверьте, что они не скрыты (кнопка \"показать/скрыть\" в строке набора текста в мобильном приложении, покатана на скрине), а также что используется последняя версия официального мобильного приложения \"Вконтакте\", либо обычный браузер.",
            List.empty,
            Some(StartKeyboard.keyboard)
          )
        } yield StateType.Registration
    }
}
