package pangea.engine

import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.UserAction

class Narrative(steps: List[(String, Beat)]) {
  private val beatMap: Map[String, Beat] = steps.toMap

  def toRoutes(nextState: StateType): Map[String, Target] =
    beatMap.map { case (key, beat) =>
      key -> Target.Run { (user: User, _: UserAction, renderer: Renderer) =>
        for {
          choices <- beat.buildChoices(user)
          _       <- renderer.show(user, Screen(beat.text, choices))
        } yield nextState
      }
    }
}
