package pangea.engine

import pangea.model.user.User
import zio.{Task, ZIO}

case class Beat(text: String, buildChoices: User => Task[List[Choice]])

object Beat {
  def simple(text: String, choices: List[Choice]): Beat =
    Beat(text, _ => ZIO.succeed(choices))
}
