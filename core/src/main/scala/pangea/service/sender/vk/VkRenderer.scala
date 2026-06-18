package pangea.service.sender.vk

import io.circe.syntax.EncoderOps
import pangea.engine.{Renderer, Screen}
import pangea.model.user.User
import pangea.model.vk.keyboard.{Action => VkAction, Button, Keyboard}
import pangea.service.sender.Api
import zio.Task

class VkRenderer(api: Api) extends Renderer {
  def show(user: User, screen: Screen): Task[Unit] = {
    val kbOpt =
      if (screen.choices.isEmpty) None
      else {
        val kb = screen.choices.foldLeft(Keyboard.default) { (kb, choice) =>
          val payload = (Map("action" -> choice.id) ++ choice.data).asJson
          kb.addRow().addButton(Button.withAction(VkAction.Text(choice.label, Some(payload))))
        }
        Some(kb)
      }
    api.sendMessage(user, screen.text, List.empty, kbOpt)
  }
}

object VkRenderer {
  def apply(api: Api): VkRenderer = new VkRenderer(api)
}
