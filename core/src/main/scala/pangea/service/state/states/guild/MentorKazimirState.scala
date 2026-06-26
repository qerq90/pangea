package pangea.service.state.states.guild

import pangea.engine.{Branch, Renderer, SceneContent, Target}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.Task

/** Заглушка наставника Казимира: содержание будет позже. */
case class MentorKazimirState(content: SceneContent) extends State {

  private val branch = new Branch(
    routes   = Map("LeaveMentorKazimir" -> Target.Goto(StateType.TrainingHall)),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.MentorKazimir) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, content.screen("guild.mentorKazimir"))

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)
}
