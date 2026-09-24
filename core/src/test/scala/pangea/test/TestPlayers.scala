package pangea.test

import pangea.engine.Players
import pangea.model.user.User
import zio.{Task, ZIO}

class TestPlayers extends Players {
  private var announcements: List[String] = Nil

  def getDisplayName(user: User): Task[String] = ZIO.succeed("Иван Иванов")

  def announce(message: String): Task[Unit] = ZIO.succeed { announcements = announcements :+ message }

  /** Что улетело в общий чат Пангеи. */
  def announced: List[String] = announcements
}
