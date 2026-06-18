package pangea.service.sender.vk

import pangea.engine.Players
import pangea.model.user.User
import pangea.service.sender.Api
import zio.Task

class VkPlayers(api: Api) extends Players {
  def getDisplayName(user: User): Task[String] =
    api.getName(user).map { response =>
      val u = response.response.head
      s"${u.firstName} ${u.lastName}"
    }
}

object VkPlayers {
  def apply(api: Api): VkPlayers = new VkPlayers(api)
}
