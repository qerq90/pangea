package pangea.engine

import pangea.model.user.User
import zio.Task

trait Players {
  def getDisplayName(user: User): Task[String]

  /** Объявление в общий чат Пангеи — например, о новом лоте на аукционе. */
  def announce(message: String): Task[Unit]
}
