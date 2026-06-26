package pangea.service.state.states.guild

import pangea.engine.{Branch, Renderer, SceneContent, Target}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.Task

/** Заглушка тренировочного зала: содержание будет позже. */
case class TrainingHallState(content: SceneContent) extends State {

  private val branch = new Branch(
    routes   = Map("LeaveTrainingHall" -> Target.Goto(StateType.Guild)),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.TrainingHall) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, content.screen("guild.trainingHall"))

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)
}
