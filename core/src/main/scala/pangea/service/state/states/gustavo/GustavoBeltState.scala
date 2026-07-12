package pangea.service.state.states.gustavo

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.Task

/** Пополнение бутылей надетого пояса у Густаво. За 100 золота за каждую недостающую бутыль
 *  восстанавливает `charges` пояса до `maxCharges`. Нет надетого пояса или он уже полный —
 *  сообщение без списаний. По завершении возвращает в раздел припасов [[GustavoSuppliesState]]. */
case class GustavoBeltState(
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
      belt = hero.equipment.belt
      _ <- if (!hasBelt(belt))
             renderer.show(user, Screen(content.text("gustavo.supplies.beltNone"),
               List(content.choice("Back", "gustavo.offerBack"))))
           else if (isFull(belt))
             renderer.show(user, Screen(content.text("gustavo.supplies.beltFull"),
               List(content.choice("Back", "gustavo.offerBack"))))
           else
             renderer.show(user, Screen(
               content.format("gustavo.supplies.beltOffer", "cost" -> beltRefillCost(hero).toString),
               List(content.choice("Refill", "gustavo.supplies.beltBuy"), content.choice("Back", "gustavo.offerBack")),
               inline = true))
    } yield ()

  private def beltDetails(belt: Item): Option[ItemDetails.Belt] = belt.details match {
    case b: ItemDetails.Belt => Some(b)
    case _                   => None
  }

  private def buy(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      belt = hero.equipment.belt
      price = beltRefillCost(hero)
      _ <- if (!hasBelt(belt))
             renderer.show(user, Screen(content.text("gustavo.supplies.beltNone"), back))
           else if (isFull(belt))
             renderer.show(user, Screen(content.text("gustavo.supplies.beltFull"), back))
           else if (hero.gold < price)
             renderer.show(user, Screen(content.format("gustavo.supplies.beltNotEnoughGold", "cost" -> price.toString), back))
           else
             refill(user, hero, belt, price, renderer)
    } yield StateType.GustavoSupplies

  private def refill(user: User, hero: Hero, belt: Item, price: Long, renderer: Renderer): Task[Unit] =
    heroDao.updateGold(user.userId, hero.gold - price) *>
      heroDao.updateEquipment(user.userId, hero.equipment.copy(belt = belt.copy(details = beltDetails(belt).map(_.refilled).getOrElse(belt.details)))) *>
      renderer.show(user, Screen(content.format("gustavo.supplies.beltRefilled",
        "charges" -> beltDetails(belt).map(_.maxCharges).getOrElse(0).toString), back))

  private def hasBelt(belt: Item): Boolean = beltDetails(belt).isDefined

  private def isFull(belt: Item): Boolean =
    beltDetails(belt).exists(b => b.charges == b.maxCharges)
}
