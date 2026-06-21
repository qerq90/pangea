package pangea.engine

// inline = true → кнопки рендерятся внутри самого сообщения (VK inline keyboard),
// а не отдельной клавиатурой снизу.
case class Screen(text: String, choices: List[Choice], inline: Boolean = false)
