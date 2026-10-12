package pangea.service.state.states.gustavo

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.{BeltSlot, Item, ItemDetails}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.bank.BankRepository
import pangea.service.purse.Purse
import pangea.service.state.{State, UserAction}
import zio.Task

/** Пополнение бутылей надетого пояса у Густаво. За 100 серебра за каждую недостающую бутыль
 *  восстанавливает `charges` пояса до `maxCharges`. Нет надетого пояса или он уже полный —
 *  сообщение без списаний. По завершении возвращает в раздел припасов [[GustavoSuppliesState]]. */
case class GustavoBeltState(
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
      hero   <- getHero(user)
      wallet <- purse.wallet(hero)
      belt = hero.equipment.belt
      price = beltRefillCost(hero)
      _ <- if (!hasBelt(belt))
             renderer.show(user, Screen(content.text("gustavo.supplies.beltNone"), back))
           else if (isFull(belt))
             renderer.show(user, Screen(content.text("gustavo.supplies.beltFull"), back))
           else if (!wallet.canAfford(price))
             renderer.show(user, Screen(content.format("gustavo.supplies.beltNotEnoughSilver", "cost" -> price.toString), back))
           else
             refill(user, hero, belt, price, renderer)
    } yield StateType.GustavoSupplies

  /** Густаво наливает фляги прямо на поясе — те, что ему по силам (кровь он не
    * разливает, см. [[pangea.model.item.FlaskEffect.refillsAtGustavo]]). */
  private def refill(user: User, hero: Hero, belt: Item, price: Long, renderer: Renderer): Task[Unit] = {
    val filled = beltDetails(belt).map(b => b.copy(slots = b.slots.map {
      case f: BeltSlot.Flask if f.kind.effect.refillsAtGustavo => f.refilled
      case other                                               => other
    }))
    val poured = beltDetails(belt).map(b => thirsty(b).size).getOrElse(0)
    purse.charge(user.userId, hero, price) *>
      heroDao.updateEquipment(user.userId, hero.equipment.copy(
        belt = belt.copy(details = filled.getOrElse(belt.details)))) *>
      renderer.show(user, Screen(content.format("gustavo.supplies.beltRefilled",
        "charges" -> poured.toString), back))
  }

  private def hasBelt(belt: Item): Boolean = beltDetails(belt).isDefined

  /** Фляги на поясе, которые Густаво может долить. */
  private def thirsty(b: ItemDetails.Belt): List[BeltSlot.Flask] = b.slots.collect {
    case f: BeltSlot.Flask if f.kind.effect.refillsAtGustavo && f.charges < f.maxCharges => f
  }

  /** Доливать нечего: пояса нет, он пуст или все фляги полны. */
  private def isFull(belt: Item): Boolean =
    beltDetails(belt).exists(b => thirsty(b).isEmpty)
}
