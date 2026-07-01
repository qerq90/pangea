package pangea.service.state.states.gustavo

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemType}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.Task

/** Пополнение зарядов надетой фляги у Густаво. За 25 × уровень золота восстанавливает
 *  `charges` фляги до `maxCharges`. Нет надетой фляги или она уже полная — сообщение без
 *  списаний. По завершении возвращает в раздел припасов [[GustavoSuppliesState]]. */
case class GustavoFlaskState(
  heroDao: HeroDao,
  content: SceneContent
) extends State with GustavoScene {

  private val branch = new Branch(
    routes = Map(
      "Refill" -> Target.Run { (u, _, r) => buy(u, r) },
      "Back"   -> Target.Goto(StateType.GustavoSupplies)
    ),
    fallback = Target.Goto(StateType.GustavoSupplies)
  )

  override def targetStates: Set[StateType] = Set(StateType.GustavoSupplies)

  override def enter(user: User, renderer: Renderer): Task[Unit] = show(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private val back = List.empty[pangea.engine.Choice]

  private def show(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      flask = hero.equipment.flask
      _ <- if (!hasFlask(flask))
             renderer.show(user, Screen(content.text("gustavo.supplies.flaskNone"),
               List(content.choice("Back", "gustavo.offerBack"))))
           else if (isFull(flask))
             renderer.show(user, Screen(content.text("gustavo.supplies.flaskFull"),
               List(content.choice("Back", "gustavo.offerBack"))))
           else
             renderer.show(user, Screen(
               content.format("gustavo.supplies.flaskOffer", "cost" -> supplyCost(hero).toString),
               List(content.choice("Refill", "gustavo.supplies.flaskBuy"), content.choice("Back", "gustavo.offerBack")),
               inline = true))
    } yield ()

  private def buy(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      flask = hero.equipment.flask
      price = supplyCost(hero)
      _ <- if (!hasFlask(flask))
             renderer.show(user, Screen(content.text("gustavo.supplies.flaskNone"), back))
           else if (isFull(flask))
             renderer.show(user, Screen(content.text("gustavo.supplies.flaskFull"), back))
           else if (hero.gold < price)
             renderer.show(user, Screen(content.format("gustavo.supplies.flaskNotEnoughGold", "cost" -> price.toString), back))
           else
             refill(user, hero, flask, price, renderer)
    } yield StateType.GustavoSupplies

  private def refill(user: User, hero: Hero, flask: Item, price: Long, renderer: Renderer): Task[Unit] =
    heroDao.updateGold(user.userId, hero.gold - price) *>
      heroDao.updateEquipment(user.userId, hero.equipment.copy(flask = flask.copy(charges = flask.maxCharges))) *>
      renderer.show(user, Screen(content.format("gustavo.supplies.flaskRefilled",
        "charges" -> flask.maxCharges.getOrElse(0).toString), back))

  private def hasFlask(flask: Item): Boolean =
    flask.itemType != ItemType.NoItem && flask.maxCharges.isDefined

  private def isFull(flask: Item): Boolean = flask.charges == flask.maxCharges
}
