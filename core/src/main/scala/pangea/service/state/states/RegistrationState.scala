package pangea.service.state.states

import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.sender.Sender
import pangea.service.state.State
import zio.{Task, ZIO}

case class RegistrationState(sender: Sender) extends State {

  // never gonna be used
  override def enter(): Task[Unit] = ZIO.unit

  override def action(user: User, action: String): Task[StateType] =
    action match {
      case _ =>
        for {
          _ <- sender.sendMessage(
            user,
            "Spasibo, bro, zaregal tebe geroya",
            List.empty
          )
        } yield StateType.Registration
    }
}
