package pangea.service.sender.vk

import io.circe.syntax.EncoderOps
import pangea.engine.{ChoiceColor, Renderer, Screen}
import pangea.model.user.User
import pangea.model.vk.keyboard.{Action => VkAction, Button, ButtonColor, Keyboard}
import pangea.service.sender.Api
import zio.Task

class VkRenderer(api: Api) extends Renderer {
  def show(user: User, screen: Screen): Task[Unit] = {
    val kbOpt =
      if (screen.choices.isEmpty) None
      else {
        val kb = screen.choices.foldLeft(Keyboard.default.withInline(screen.inline)) { (kb, choice) =>
          val payload = (Map("action" -> choice.id) ++ choice.data).asJson
          val button  = Button.withAction(VkAction.Text(choice.label, Some(payload)))
                          .withColor(VkRenderer.toButtonColor(choice.color))
          kb.addRow().addButton(button)
        }
        Some(kb)
      }
    api.sendMessage(user, screen.text, List.empty, kbOpt)
  }
}

object VkRenderer {
  def apply(api: Api): VkRenderer = new VkRenderer(api)

  def toButtonColor(color: ChoiceColor): ButtonColor = color match {
    case ChoiceColor.Negative  => ButtonColor.Negative
    case ChoiceColor.Positive  => ButtonColor.Positive
    case ChoiceColor.Secondary => ButtonColor.Secondary
    case ChoiceColor.Primary   => ButtonColor.Primary
  }
}
