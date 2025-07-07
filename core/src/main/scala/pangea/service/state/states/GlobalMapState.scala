package pangea.service.state.states

import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.State
import zio.Task

case class GlobalMapState() extends State {
  override def enter(): Task[Unit] = ???

  override def action(user: User, action: String): Task[StateType] = ???
}
