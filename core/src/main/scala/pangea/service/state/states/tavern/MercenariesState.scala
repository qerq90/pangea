package pangea.service.state.states.tavern

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.{BrewKind, Item}
import pangea.model.squad.{AllyKind, AllyRates, Squad}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.bank.BankRepository
import pangea.service.purse.Purse
import pangea.service.state.{SquadDuty, State, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** «Наёмники» в таверне: трое за дальним столом — история, цена и «Нанять».
  * Кто уже в отряде или ушёл по свитку (на сутки) — за столом не сидит. Вернувшийся
  * встречает репликой о свитке и снова нанимается. Йорген берёт серебром
  * (500 × уровень), Плюх — Живой водой, Брамбл — шнапсом из красавки (уровень
  * на пять, но не меньше одного). */
case class MercenariesState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent,
  bank:          Option[BankRepository] = None
) extends State {
  import MercenariesState._

  /** Кошель: своё серебро, а следом — то, что лежит в ячейке Торгового дома. */
  private val purse = Purse(heroDao, bank)

  private val branch = new Branch(
    routes = Map(
      "MercList"      -> Target.Run { (u, _, r) => showList(u, r).as(StateType.Mercenaries) },
      "MercCard"      -> Target.Run { (u, ua, r) => withKind(ua, u, r)(k => showCard(u, k, r)).as(StateType.Mercenaries) },
      "MercHire"      -> Target.Run { (u, ua, r) => withKind(ua, u, r)(k => hire(u, k, r)).as(StateType.Mercenaries) },
      "BackFromMercs" -> Target.Goto(StateType.Tavern)
    ),
    fallback = Target.Run { (u, _, r) => showList(u, r).as(StateType.Mercenaries) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.Mercenaries

  override def enter(user: User, renderer: Renderer): Task[Unit] = showList(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Кто за столом: не в отряде и не в отлучке. Отработавшие свой день сперва
    * уходят, вернувшихся по свитку — встречаем. */
  private def showList(user: User, renderer: Renderer): Task[Unit] =
    for {
      now   <- nowMs
      hero0 <- getHero(user)
      hero  <- SquadDuty.settle(heroDao, content, user, hero0, now, renderer)
      back   = hero.squad.returned(now)
      _    <- ZIO.foreachDiscard(back) { k =>
                renderer.show(user, Screen(content.format("mercenaries.returned",
                  "name" -> k.name, "line" -> content.text(s"mercenaries.${key(k)}.returned")), Nil))
              }
      squad = back.foldLeft(hero.squad)(_ welcomeBack _)
      _    <- ZIO.when(back.nonEmpty)(heroDao.updateSquad(user.userId, squad))
      free  = available(squad, now)
      text  = if (free.isEmpty) content.text("mercenaries.empty") else content.text("mercenaries.title")
      buttons = free.map(k => Choice("MercCard", Choice.fit(k.name), data = Map("kind" -> k.entryName), row = Some(0)))
      _    <- renderer.show(user, Screen(text, buttons :+ content.choice("BackFromMercs", "mercenaries.back").copy(row = Some(1))))
    } yield ()

  /** Карточка: история, реплика с ценой, «Нанять». */
  private def showCard(user: User, kind: AllyKind, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap { hero =>
      val story = content.text(s"mercenaries.${key(kind)}.story")
      val price = kind match {
        case AllyKind.Human => content.format("mercenaries.human.price", "cost" -> silverCost(hero).toString)
        case k              => content.format(s"mercenaries.${key(k)}.price", "n" -> AllyKind.brewsFor(hero.lvl).toString)
      }
      renderer.show(user, Screen(story + "\n\n" + price, List(
        content.choice("MercHire", "mercenaries.hireLabel").copy(data = Map("kind" -> kind.entryName), row = Some(0)),
        content.choice("MercList", "mercenaries.toList").copy(row = Some(1)))))
    }

  /** Найм: место в отряде, плата, и наёмник встаёт на первую свободную позицию. */
  private def hire(user: User, kind: AllyKind, renderer: Renderer): Task[Unit] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      _    <-
        if (!available(hero.squad, now).contains(kind)) showList(user, renderer)
        else if (hero.squad.freePosition.isEmpty)
          renderer.show(user, Screen(content.text("mercenaries.squadFull"), Nil)) *> showCard(user, kind, renderer)
        else pay(user, hero, kind).flatMap {
          case Some(refusal) => renderer.show(user, Screen(refusal, Nil)) *> showCard(user, kind, renderer)
          case None =>
            heroDao.updateSquad(user.userId, hero.squad.hire(kind, hero.lvl, now)) *>
              renderer.show(user, Screen(content.format("mercenaries.hired", "name" -> kind.name), Nil)) *>
              showList(user, renderer)
        }
    } yield ()

  /** Списать плату; текст отказа, если платить нечем. */
  private def pay(user: User, hero: Hero, kind: AllyKind): Task[Option[String]] =
    kind match {
      case AllyKind.Human =>
        val cost = silverCost(hero)
        purse.wallet(hero).flatMap { wallet =>
          if (!wallet.canAfford(cost))
            ZIO.succeed(Some(content.format("mercenaries.notEnoughSilver", "cost" -> cost.toString, "silver" -> wallet.total.toString)))
          else purse.charge(user.userId, hero, cost).as(None)
        }
      case k =>
        val brew = brewFor(k)
        val n    = AllyKind.brewsFor(hero.lvl).toInt
        inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString)).flatMap { inv =>
          val bottles = brews(inv.items.data, brew)
          if (bottles.size < n)
            ZIO.succeed(Some(content.format("mercenaries.notEnoughBrews",
              "n" -> n.toString, "brew" -> brew.label, "have" -> bottles.size.toString)))
          else
            inventoryRepo.removeItems(bottles.take(n).map(_.id).toSet, hero.id)
              .mapError(e => new Throwable(e.toString)).as(None)
        }
    }

  private def withKind(ua: UserAction, user: User, renderer: Renderer)(f: AllyKind => Task[Unit]): Task[Unit] =
    ua.payload
      .flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("kind")))
      .flatMap(AllyKind.withNameOption)
      .fold(showList(user, renderer))(f)

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object MercenariesState {

  /** Ключ в scenes.yaml: `mercenaries.<key>`. */
  def key(kind: AllyKind): String = kind match {
    case AllyKind.Human  => "human"
    case AllyKind.Murloc => "murloc"
    case AllyKind.Gnome  => "gnome"
  }

  /** Кто сейчас за столом. */
  def available(squad: Squad, nowMs: Long): List[AllyKind] =
    AllyKind.values.filterNot(k => squad.has(k) || squad.isAway(k, nowMs)).toList

  def silverCost(hero: Hero): Long = AllyRates.HumanSilverPerLvl * hero.lvl

  /** Чем берут Плюх и Брамбл. */
  def brewFor(kind: AllyKind): BrewKind = kind match {
    case AllyKind.Gnome => BrewKind.Schnapps
    case _              => BrewKind.LivingWater
  }

  def brews(items: List[Item], kind: BrewKind): List[Item] = items.filter(_.brew.contains(kind))
}
