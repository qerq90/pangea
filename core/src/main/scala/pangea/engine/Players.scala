package pangea.engine

import pangea.model.user.User
import zio.Task

trait Players {
  def getDisplayName(user: User): Task[String]

  /** Объявление в общий чат Пангеи — например, о новом лоте на аукционе. */
  def announce(message: String): Task[Unit]

  /** Письмо игроку в личные сообщения — например, продавцу о том, что его лот
    * купили. Клавиатуру прошлого экрана не трогает. */
  def notify(user: User, message: String): Task[Unit]
}
