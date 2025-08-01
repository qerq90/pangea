package pangea.service.state.states

import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.Task

case class GlobalMapState() extends State {
  override def enter(user: User): Task[Unit] = ???

  override def action(user: User, action: UserAction): Task[StateType] = ???
}
