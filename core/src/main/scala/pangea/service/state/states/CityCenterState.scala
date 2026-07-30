package pangea.service.state.states

import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.Task

/** Центр города (Центральный район). Пока содержит только Храм Азата и «Назад». */
case class CityCenterState(content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "TempleAzat" -> Target.Goto(StateType.TempleAzat),
      "BackToCity" -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.CityCenter) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] = {
    val byId = content.screen("cityCenter.enter").choices.map(c => c.id -> c).toMap
    val choices = List(
      byId("TempleAzat").copy(color = ChoiceColor.Positive, row = Some(0)),
      byId("BackToCity").copy(color = ChoiceColor.Negative, row = Some(1))
    )
    renderer.show(user, Screen(content.text("cityCenter.enter.text"), choices))
  }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)
}
