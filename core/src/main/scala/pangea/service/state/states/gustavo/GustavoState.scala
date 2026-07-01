package pangea.service.state.states.gustavo

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.Task

/**
 * Лавка знахаря «Густаво» — второй уличный торговец на Торговой площади. Это только меню
 * из четырёх кнопок; каждая ведёт в свой отдельный стейт:
 *  1. «Исцеление травм»          → [[GustavoHealState]] (зелёная кнопка, краснеет на кулдауне);
 *  2. «Увеличение характеристик»  → [[GustavoBoostState]];
 *  3. «Сдать травы»               → заглушка (трав пока нет);
 *  4. «Пополнить припасы»         → [[GustavoSuppliesState]] (фляга/пояс).
 * Цвет кнопки лечения зависит от кулдауна зелья ([[GustavoData.healCooldownUntil]]).
 */
case class GustavoState(
  heroDao: HeroDao,
  content: SceneContent
) extends State with GustavoScene {

  private val branch = new Branch(
    routes = Map(
      "Heal"     -> Target.Goto(StateType.GustavoHeal),
      "Boost"    -> Target.Goto(StateType.GustavoBoost),
      "Herbs"    -> Target.Run { (u, _, r) =>
                      r.show(u, Screen(content.text("gustavo.herbsStub"), Nil)) *> renderMenu(u, r).as(StateType.Gustavo) },
      "Supplies" -> Target.Goto(StateType.GustavoSupplies),
      "Back"     -> Target.Goto(StateType.MarketSquare)
    ),
    fallback = Target.Run { (u, _, r) => renderMenu(u, r).as(StateType.Gustavo) }
  )

  override def targetStates: Set[StateType] =
    Set(StateType.MarketSquare, StateType.Gustavo, StateType.GustavoHeal,
        StateType.GustavoBoost, StateType.GustavoSupplies)

  override def enter(user: User, renderer: Renderer): Task[Unit] = renderMenu(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def renderMenu(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- nowMs
      data <- loadData(user)
      _    <- renderer.show(user, menuScreen(data, now))
    } yield ()

  private def menuScreen(data: GustavoData, now: Long): Screen = {
    val healBtn = data.healCooldownUntil.filter(_ > now).map(_ - now) match {
      case Some(left) =>
        content.choice("Heal", "gustavo.cooldownLabel", "mins" -> minsOf(left)).copy(color = ChoiceColor.Negative)
      case None =>
        content.choice("Heal", "gustavo.healLabel").copy(color = ChoiceColor.Positive)
    }
    val boostBtn    = content.choice("Boost", "gustavo.boostLabel")
    val herbsBtn    = content.choice("Herbs", "gustavo.herbsLabel")
    val suppliesBtn = content.choice("Supplies", "gustavo.suppliesLabel")
    val choices = List(healBtn, boostBtn, herbsBtn, suppliesBtn, content.choice("Back", "gustavo.back"))
    Screen(content.text("gustavo.menu.text"), choices)
  }
}
