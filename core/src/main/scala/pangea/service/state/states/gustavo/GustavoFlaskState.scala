package pangea.service.state.states.gustavo

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.bank.BankRepository
import pangea.service.purse.Purse
import pangea.service.state.{State, UserAction}
import zio.Task

/** Пополнение зарядов надетой фляги у Густаво. За 25 серебра за каждый недостающий глоток
 *  восстанавливает `charges` фляги до `maxCharges`. Нет надетой фляги или она уже полная —
 *  сообщение без списаний. По завершении возвращает в раздел припасов [[GustavoSuppliesState]]. */
case class GustavoFlaskState(
  heroDao: HeroDao,
  content: SceneContent
,
  bank:    Option[BankRepository] = None
) extends State with GustavoScene {

  /** Кошель: своё серебро, а следом — то, что лежит в ячейке Торгового дома. */
  private val purse = Purse(heroDao, bank)

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
           else if (!refillable(flask))
             renderer.show(user, Screen(content.text("gustavo.supplies.flaskBlood"),
               List(content.choice("Back", "gustavo.offerBack"))))
           else if (isFull(flask))
             renderer.show(user, Screen(content.text("gustavo.supplies.flaskFull"),
               List(content.choice("Back", "gustavo.offerBack"))))
           else
             renderer.show(user, Screen(
               content.format("gustavo.supplies.flaskOffer", "cost" -> flaskRefillCost(hero).toString),
               List(content.choice("Refill", "gustavo.supplies.flaskBuy"), content.choice("Back", "gustavo.offerBack")),
               inline = true))
    } yield ()

  private def flaskDetails(flask: Item): Option[ItemDetails.Flask] = flask.details match {
    case f: ItemDetails.Flask => Some(f)
    case _                    => None
  }

  private def buy(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      wallet <- purse.wallet(hero)
      flask = hero.equipment.flask
      price = flaskRefillCost(hero)
      _ <- if (!hasFlask(flask))
             renderer.show(user, Screen(content.text("gustavo.supplies.flaskNone"), back))
           else if (!refillable(flask))
             renderer.show(user, Screen(content.text("gustavo.supplies.flaskBlood"), back))
           else if (isFull(flask))
             renderer.show(user, Screen(content.text("gustavo.supplies.flaskFull"), back))
           else if (!wallet.canAfford(price))
             renderer.show(user, Screen(content.format("gustavo.supplies.flaskNotEnoughSilver", "cost" -> price.toString), back))
           else
             refill(user, hero, flask, price, renderer)
    } yield StateType.GustavoSupplies

  private def refill(user: User, hero: Hero, flask: Item, price: Long, renderer: Renderer): Task[Unit] =
    purse.charge(user.userId, hero, price) *>
      heroDao.updateEquipment(user.userId, hero.equipment.copy(flask = flask.copy(details = flaskDetails(flask).map(_.refilled).getOrElse(flask.details)))) *>
      renderer.show(user, Screen(content.format("gustavo.supplies.flaskRefilled",
        "charges" -> flaskDetails(flask).map(_.maxCharges).getOrElse(0).toString), back))

  private def hasFlask(flask: Item): Boolean = flaskDetails(flask).isDefined

  /** Вампирскую Густаво не заправляет: она пьёт кровь сама. */
  private def refillable(flask: Item): Boolean = flaskDetails(flask).exists(_.effect.refillsAtGustavo)

  private def isFull(flask: Item): Boolean =
    flaskDetails(flask).exists(f => f.charges == f.maxCharges)
}
