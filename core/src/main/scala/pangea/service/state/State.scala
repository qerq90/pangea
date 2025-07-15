package pangea.service.state

import pangea.model.state.StateType
import pangea.model.user.User
import zio.Task

trait State {
  def enter(): Task[Unit]
  def action(user: User, action: UserAction): Task[StateType]
}
