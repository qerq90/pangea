package pangea.service.state.states

import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.Task

case class MarketSquareState(content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "StreetMerchants"    -> Target.Run { (user, _, renderer) =>
                                renderer.show(user, content.screen("marketSquare.streetMerchants"))
                                  .as(StateType.MarketSquare) },
      "MerchantRichelieu"  -> Target.Goto(StateType.Merchant),
      "Construction"       -> Target.Goto(StateType.Construction),
      "BackToMarketSquare" -> Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.MarketSquare) },
      "BackToCity"         -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.MarketSquare) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] = {
    val byId = content.screen("marketSquare.enter").choices.map(c => c.id -> c).toMap
    val choices = List(
      byId("StreetMerchants").copy(row = Some(0)),
      byId("Construction").copy(row = Some(0)),
      byId("BackToCity").copy(color = ChoiceColor.Negative, row = Some(1))
    )
    renderer.show(user, Screen(content.text("marketSquare.enter.text"), choices))
  }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)
}
