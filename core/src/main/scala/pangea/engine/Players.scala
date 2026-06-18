package pangea.engine

import pangea.model.user.User
import zio.Task

trait Players {
  def getDisplayName(user: User): Task[String]
}
