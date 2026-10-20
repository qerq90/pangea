package pangea.engine

// inline = true → кнопки рендерятся внутри самого сообщения (VK inline keyboard),
// а не отдельной клавиатурой снизу.
// hideKeyboard = true → отправить пустую bot-клавиатуру, чтобы скрыть кнопки
// прошлого экрана. По умолчанию без кнопок мы НЕ трогаем прошлую клавиатуру —
// её часто хочется сохранить (например, промежуточные подтверждения боя).
// image → картинка к сообщению: фото из альбома группы ВК, `photo-<группа>_<фото>`
// или ссылка на него. ВК прикрепляет уже загруженное фото — заново его не грузим.
case class Screen(text: String, choices: List[Choice], inline: Boolean = false, hideKeyboard: Boolean = false,
                  image: Option[String] = None)
