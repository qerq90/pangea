package pangea.test

import pangea.engine.Players
import pangea.model.user.{User, UserId}
import zio.{Task, ZIO}

class TestPlayers extends Players {
  private var announcements: List[String]        = Nil
  private var letters: List[(UserId, String)]    = Nil

  def getDisplayName(user: User): Task[String] = ZIO.succeed("Иван Иванов")

  def announce(message: String): Task[Unit] = ZIO.succeed { announcements = announcements :+ message }

  def notify(user: User, message: String): Task[Unit] =
    ZIO.succeed { letters = letters :+ (user.userId -> message) }

  /** Что улетело в общий чат Пангеи. */
  def announced: List[String] = announcements

  /** Письма в личку: кому и что. */
  def sentLetters: List[(UserId, String)] = letters
}
