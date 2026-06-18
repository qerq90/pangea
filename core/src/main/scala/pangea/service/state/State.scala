package pangea.service.state

import pangea.engine.Renderer
import pangea.model.state.StateType
import pangea.model.user.User
import zio.Task

trait State {
  def targetStates: Set[StateType]                                             = Set.empty
  def enter(user: User, renderer: Renderer): Task[Unit]
  def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType]
}
