package pangea.service.state.states

import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.Task

case class HarborQuarterState(content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "UnassumingBarrel" -> Target.Run { (user, _, renderer) =>
                              renderer.show(user, Screen(content.text("harborQuarter.unassumingBarrel"), Nil)) *>
                                enter(user, renderer).as(StateType.HarborQuarter) },
      "BackToCity"       -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.HarborQuarter) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] = {
    val byId = content.screen("harborQuarter.enter").choices.map(c => c.id -> c).toMap
    val choices = List(
      byId("UnassumingBarrel").copy(color = ChoiceColor.Positive, row = Some(0)),
      byId("BackToCity").copy(color = ChoiceColor.Negative, row = Some(1))
    )
    renderer.show(user, Screen(content.text("harborQuarter.enter.text"), choices))
  }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)
}
