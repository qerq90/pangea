package pangea.service.state.states

import pangea.engine.{Branch, Renderer, SceneContent, Target}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{CityExit, State, UserAction}
import zio.Task

/** Уличные торговцы Торговой площади: Ришелье и Густаво. Своя сцена, а не
  * подменю площади, — чтобы «Назад» из лавки возвращал сюда, к выбору между
  * ними, а не на саму площадь. */
case class StreetMerchantsState(content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "MerchantRichelieu"  -> Target.Goto(StateType.Merchant),
      "Gustavo"            -> Target.Goto(StateType.Gustavo),
      "BackToMarketSquare" -> Target.Goto(StateType.MarketSquare),
      CityExit.route
    ),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.StreetMerchants) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, CityExit.on(content.screen("marketSquare.streetMerchants"), content))

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)
}
