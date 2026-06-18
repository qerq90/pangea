package pangea.test

import pangea.engine.Players
import pangea.model.user.User
import zio.{Task, ZIO}

class TestPlayers extends Players {
  def getDisplayName(user: User): Task[String] = ZIO.succeed("Иван Иванов")
}
