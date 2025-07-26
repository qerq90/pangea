package pangea.service.state.states.Registration

import io.circe.jawn.decode
import pangea.dao.hero.HeroDao
import pangea.model.monster.Race
import pangea.model.state.StateType
import pangea.model.state.StateType.Registration
import pangea.model.user.User
import pangea.model.vk.keyboard.Action.Text
import pangea.model.vk.keyboard.{Button, Keyboard}
import pangea.service.sender.Api
import pangea.service.state.states.Registration.keyboard.{
  NameKeyboard,
  RaceDescriptionKeyboard,
  RaceKeyboard,
  StartKeyboard
}
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

case class RegistrationState(api: Api, heroDao: HeroDao) extends State {

  // never gonna be used
  override def enter(): Task[Unit] = ZIO.unit

  private def matchUserAction(action: UserAction): Action =
    action.payload match {
      case Some(payload) => decode[Action](payload).toOption.get
      case None          => Action.Text
    }

  override def action(user: User, action: UserAction): Task[StateType] =
    matchUserAction(action) match {
      case Action.Travel8 => ???
      case Action.Travel7 => getTravel7(user)
      case Action.Travel6 => getTravel6(user)
      case Action.Travel5 => getTravel5(user)
      case Action.Travel4 => getTravel4(user)
      case Action.Travel3 => getTravel3(user)
      case Action.Travel2 => getTravel2(user)
      case Action.Travel1 => getTravel1(user)
      case Action.Travel  => updateRace(user, action)
      case Action.RaceDescription =>
        getRaceDescription(user, Race.withNameOption(action.text))
      case Action.Race => getRace(user)
      case _ =>
        for {
          _ <- api.sendMessage(
            user,
            "Добро пожаловать на Пангею! Для того что бы приступить, отправьте слово «Начать»\nЭто текстовая онлайн-ммо- игра «Клеймо Пангеи», все действия в ней происходят при использовании команд под окном чата, результаты этих действий приходят в виде ответа от сообщества. В игре представлено большое число вариативности и уникальности вашего игрового пути к прохождению всей игровой компании. Так же не забывайте взаимодействовать с другими игроками для облегчения своей игры.\nкартинка c приветствием\nПрежде всего, проверим управление. Вы видите кнопки внизу?\n\n⚠Если у Вас нет кнопок для управления под окном чата - проверьте, что они не скрыты (кнопка \"показать/скрыть\" в строке набора текста в мобильном приложении, покатана на скрине), а также что используется последняя версия официального мобильного приложения \"Вконтакте\", либо обычный браузер.",
            List.empty,
            Some(StartKeyboard.keyboard)
          )
        } yield Registration
    }

  private def getTravel7(user: User): Task[StateType] =
    api
      .getName(user)
      .flatMap { response =>
        val firstName = response.response.head.firstName
        val lastName  = response.response.head.lastName
        api
          .sendMessage(
            user,
            "Спустя несколько минут, в комнату вошла другая высокая женщина - человек, её лицо было посечено шрамами, но оно излучала доброту, которая никаким образом не подходила такому лицу. По одежде можно было понять, что она не служанка.\n«Доброе утро. Я Вельсмера , хозяйка здешней таверны.» представилась она.",
            List.empty,
            Some(NameKeyboard.keyboard(s"Меня зовут $firstName $lastName"))
          )
          .as(Registration)
      }

  private def getTravel6(user: User): Task[StateType] =
    api
      .sendMessage(
        user,
        "Вы осматриваете помещение. Судя по всему это гостиная комната для одного посетителя, есть пустой шкаф с одеждой, небольшой рюкзак, стол за которым можно поесть, а так же вы обнаружили на себе незнакомую но чистую одежду, идеально подходящую под вашу комплекцию.В дверь входит служанка и ставит миску с, аппетитно пахнущим супом на стол, кладёт несколько ломтиков хлеба и уходит пожелав вам приятного аппетита. Вы почуствовали сильный голод, и немедленно принялись за трапезу.",
        List.empty,
        Some(
          Keyboard.default
            .addRow()
            .addButton(
              Button
                .withAction(Text("Наконец то еда!", Some(Action.Travel7.json)))
            )
        )
      )
      .as(Registration)

  private def getTravel5(user: User): Task[StateType] =
    api
      .sendMessage(
        user,
        "Вы проснулись в белоснежной кровати, тело чувствуется лёгким и абсолютно здоровым, вы проверяете свои конечности, и они слушаются вас без каких-либо проблем, при попытке встать с кровати, она предательски издаёт скрип, в эту же секунду вы слышите топот ног откуда-то снизу поднимающий всё ближе к вам.\nЕщё несколько секунд и в комнату врывается вместе со свежим и холодным воздухом, девушка-человек, одетая в форму служанки.\n«Вам уже лучше! Это хорошо. Я сейчас же позову хозяйку, и принесу еду, ни в коем случае никуда не уходите! Вам нужно по-есть, и не бойтесь, всё уже оплачено!».",
        List.empty,
        Some(
          Keyboard.default
            .addRow()
            .addButton(
              Button.withAction(
                Text(
                  "Молча кивнуть девушке и осмотреть комнату получше",
                  Some(Action.Travel6.json)
                )
              )
            )
        )
      )
      .as(Registration)

  private def getTravel4(user: User): Task[StateType] =
    api
      .sendMessage(
        user,
        "Неопределенное время, вы ощущаете будто бродите в тумане из своих мыслей. Нет ни солнца, ни луны, никакого источника света или понимая, безцельное блуждание на задворках неизвестности...\nОднако ветер доносит до вас обрывки фраз...«Жизненные силы восполняются с каждым днём, серьёзных травм нет. В общем будет жить» — сказал неизвестный голос.\n«Надеюсь, что все так и будет...» — прозвучал трепетно, необычайно нежный голос неизвестного, но кажется родного для вас существа...",
        List.empty,
        Some(
          Keyboard.default
            .addRow()
            .addButton(
              Button
                .withAction(Text("Открыть глаза", Some(Action.Travel5.json)))
            )
        )
      )
      .as(Registration)

  private def getTravel3(user: User): Task[StateType] =
    api
      .sendMessage(
        user,
        "С трудом пройдя десятки метров, вы замечаете как среди трупов бродит фигура, чьё лицо и тело спрятано под накидкой, когда же она обращает на вас внимание, на вашей руке загорается странный символ и кажется сейчас от боли вы потеряете сознание.",
        List.empty,
        Some(
          Keyboard.default
            .addRow()
            .addButton(
              Button.withAction(
                Text("Потерять сознание...", Some(Action.Travel4.json))
              )
            )
        )
      )
      .as(Registration)

  private def getTravel2(user: User): Task[StateType] =
    api
      .sendMessage(
        user,
        "При попытках вспомнить хоть, что-то, ваша голова только сильнее начинала болеть. Здравый смысл подсказывал лишь, что надо лишь двигаться подальше от места где лежат одни трупы самых разных невиданных ранее существ и рас...",
        List.empty,
        Some(
          Keyboard.default
            .addRow()
            .addButton(
              Button.withAction(
                Text(
                  "Идти дальше прочь от этого места",
                  Some(Action.Travel3.json)
                )
              )
            )
        )
      )
      .as(Registration)

  private def getTravel1(user: User): Task[StateType] =
    api
      .sendMessage(
        user,
        "Вы начали двигаться вверх, раздвигая тяжёлые и мокрые камни впереди себя. Спустя несколько минут ваших усилий. Вы выбрались и оказались в лесу... немного осмотревшись вы поняли, что вылезали не из каменного завала, а из целой горы трупов...",
        List.empty,
        Some(
          Keyboard.default
            .addRow()
            .addButton(
              Button.withAction(
                Text("Как же болит моя голова", Some(Action.Travel2.json))
              )
            )
        )
      )
      .as(Registration)

  private def updateRace(user: User, action: UserAction): Task[StateType] =
    for {
      race <- ZIO.fromEither(decode[Race](action.payload.get))
      _    <- heroDao.updateRace(user.userId, race)
      _ <- api.sendMessage(
        user,
        "Вы открываете глаза. Практически ничего не видно, только сверху проблёскивает свет звёзд. Тяжело дышать, вы замкнуты и стеснены в движениях, рядом лишь камни и земля, кажется ваших сил хватит что бы выбраться наверх.",
        List.empty,
        Some(
          Keyboard.default
            .addRow()
            .addButton(
              Button.withAction(
                Text("Выбраться наверх", Some(Action.Travel1.json))
              )
            )
        )
      )
    } yield Registration

  private def getRaceDescription(
      user: User,
      race: Option[Race]
  ): Task[StateType] =
    race match {
      case Some(race) =>
        api
          .sendMessage(
            user,
            race.description,
            List.empty,
            Some(RaceDescriptionKeyboard.keyboard(race))
          )
          .as(Registration)

      case None =>
        api
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
      _ <- api.sendMessage(
        user,
        "Теперь, выберите свою расу. Позже её можно будет сменить.",
        List.empty,
        Some(RaceKeyboard.keyboard)
      )
    } yield Registration
}
