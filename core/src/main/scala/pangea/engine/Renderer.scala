package pangea.engine

import pangea.model.user.User
import zio.Task

trait Renderer {
  def show(user: User, screen: Screen): Task[Unit]
}
