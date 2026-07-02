package pangea.service.state.states.gustavo

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.Task

/**
 * Раздел «Пополнить припасы» лавки Густаво. Меню из трёх кнопок:
 *  1. «Фляга» → [[GustavoFlaskState]] (пополнение зарядов фляги);
 *  2. «Пояс»  → заглушка (пока не реализовано);
 *  3. «Назад» → меню [[GustavoState]].
 * Цена любого пополнения одинаковая — 25 × уровень героя ([[GustavoData.SupplyCostPerLevel]]).
 */
case class GustavoSuppliesState(
  heroDao: HeroDao,
  content: SceneContent
) extends State with GustavoScene {

  private val branch = new Branch(
    routes = Map(
      "Flask" -> Target.Goto(StateType.GustavoFlask),
      "Belt"  -> Target.Run { (u, _, r) =>
                   r.show(u, Screen(content.text("gustavo.supplies.beltStub"), Nil)) *> render(u, r).as(StateType.GustavoSupplies) },
      "Back"  -> Target.Goto(StateType.Gustavo)
    ),
    fallback = Target.Run { (u, _, r) => render(u, r).as(StateType.GustavoSupplies) }
  )

  override def targetStates: Set[StateType] =
    Set(StateType.Gustavo, StateType.GustavoSupplies, StateType.GustavoFlask)

  override def enter(user: User, renderer: Renderer): Task[Unit] = render(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def render(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      choices = List(
        content.choice("Flask", "gustavo.supplies.flaskLabel", "cost" -> supplyCost(hero).toString),
        content.choice("Belt",  "gustavo.supplies.beltLabel",  "cost" -> supplyCost(hero).toString),
        content.choice("Back",  "gustavo.supplies.back")
      )
      _ <- renderer.show(user, Screen(content.format("gustavo.supplies.intro", "cost" -> supplyCost(hero).toString), choices))
    } yield ()
}
