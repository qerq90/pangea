package pangea.service.state

import pangea.engine.Renderer
import pangea.model.state.StateType
import pangea.model.user.User
import zio.Task

trait State {
  def targetStates: Set[StateType]                                             = Set.empty
  def enter(user: User, renderer: Renderer): Task[Unit]
  def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType]

  /**
   * Effect node: when an `enter` fully resolves on its own (runs its event and
   * has nothing for the player to decide), it routes onward here. The handler
   * chains the transition immediately — no button press in between.
   */
  def autoAdvance: Option[StateType] = None
}
