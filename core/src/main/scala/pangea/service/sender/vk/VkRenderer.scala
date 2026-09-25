package pangea.service.sender.vk

import io.circe.syntax.EncoderOps
import pangea.engine.{Choice, ChoiceColor, Renderer, Screen}
import pangea.model.user.User
import pangea.model.vk.keyboard.{Action => VkAction, Button, ButtonColor, Keyboard}
import pangea.service.sender.Api
import zio.Task

class VkRenderer(api: Api) extends Renderer {
  def show(user: User, screen: Screen): Task[Unit] = {
    val kbOpt =
      if (screen.choices.isEmpty)
        // Без кнопок: либо явно скрываем прошлую клавиатуру (пустая bot-клавиатура),
        // либо вообще не отправляем `keyboard` — тогда ВК сохраняет прежнюю.
        if (screen.hideKeyboard) Some(Keyboard.empty) else None
      else {
        // Если хотя бы у одной кнопки задан `row` — рендерим ряды: choices группируются
        // по row, ряды отсортированы по возрастанию. Кнопки без row идут каждая в свою
        // строку (старое поведение — сохранено для совместимости).
        val hasRows = screen.choices.exists(_.row.isDefined)
        val kbInit  = Keyboard.empty.withInline(screen.inline)
        val kb =
          if (!hasRows) screen.choices.foldLeft(kbInit) { (acc, choice) =>
            acc.addRow().addButton(toButton(choice))
          }
          else {
            val grouped = screen.choices.zipWithIndex
              .groupBy { case (c, _) => c.row.getOrElse(Int.MaxValue) }
              .toList
              .sortBy(_._1)
              .map { case (_, items) => items.sortBy(_._2).map(_._1) }
            VkRenderer.fit(grouped).foldLeft(kbInit) { (acc, rowChoices) =>
              rowChoices.foldLeft(acc.addRow()) { (k, choice) => k.addButton(toButton(choice)) }
            }
          }
        Some(kb)
      }
    api.sendMessage(user, screen.text, List.empty, kbOpt)
  }

  private def toButton(choice: Choice): Button = {
    val payload = (Map("action" -> choice.id) ++ choice.data).asJson
    // Подпись длиннее лимита ВК роняет отправку — режем здесь, чтобы ни одна
    // сцена не могла уронить экран.
    Button.withAction(VkAction.Text(Choice.fit(choice.label), Some(payload)))
      .withColor(VkRenderer.toButtonColor(choice.color))
  }
}

object VkRenderer {
  def apply(api: Api): VkRenderer = new VkRenderer(api)

  /** Сколько рядов и кнопок в ряду принимает клавиатура ВК. Сообщение с более
    * чем [[MaxRows]] рядами ВК отклоняет целиком, и игрок вместо экрана видит
    * «Произошла ошибка». */
  val MaxRows: Int           = 10
  val MaxButtonsPerRow: Int  = 5

  /** Ужимает раскладку под лимит ВК: если рядов больше, чем принимает
    * клавиатура, кнопки перекладываются плотно — по пять в ряд. Это страховка:
    * сцены и так считают ряды сами, но одна забытая кнопка не должна ронять
    * экран целиком. Совсем не влезшее (свыше пятидесяти кнопок) отсекается —
    * лучше показать экран без хвоста, чем не показать ничего. */
  def fit(rows: List[List[Choice]]): List[List[Choice]] =
    if (rows.sizeIs <= MaxRows) rows
    else rows.flatten.grouped(MaxButtonsPerRow).toList.take(MaxRows)

  def toButtonColor(color: ChoiceColor): ButtonColor = color match {
    case ChoiceColor.Negative  => ButtonColor.Negative
    case ChoiceColor.Positive  => ButtonColor.Positive
    case ChoiceColor.Secondary => ButtonColor.Secondary
    case ChoiceColor.Primary   => ButtonColor.Primary
  }
}
