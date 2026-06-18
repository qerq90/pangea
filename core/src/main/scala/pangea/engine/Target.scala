package pangea.engine

import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.UserAction
import zio.Task

sealed trait Target

object Target {
  case class Goto(state: StateType)                                              extends Target
  case class Run(f: (User, UserAction, Renderer) => Task[StateType])            extends Target
}
