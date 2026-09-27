package pangea.service.state.states.hero

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.squad.{Ally, AllyRates}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{SquadDuty, State, UserAction}
import zio.{Task, ZIO}

/** «Отряд» в меню персонажа: строй с позициями, карточка союзника — его
  * состояние и место, перестановка (занятое место — меняются местами, в том
  * числе с героем) и увольнение. Позиции те же, что в бою: союзник на позиции N
  * стоит против врага на месте N. */
case class SquadState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "SquadList"   -> Target.Run { (u, _, r) => showList(u, r).as(StateType.Squad) },
      "SquadAlly"   -> Target.Run { (u, ua, r) => withAlly(ua)(p => showAlly(u, p, r)).as(StateType.Squad) },
      "SquadMove"   -> Target.Run { (u, ua, r) => move(u, ua, r).as(StateType.Squad) },
      "SquadDismiss" -> Target.Run { (u, ua, r) => withAlly(ua)(p => confirmDismiss(u, p, r)).as(StateType.Squad) },
      "SquadDismissDo" -> Target.Run { (u, ua, r) => withAlly(ua)(p => dismiss(u, p, r)).as(StateType.Squad) },
      "BackFromSquad" -> Target.Goto(StateType.HeroStats)
    ),
    fallback = Target.Run { (u, _, r) => showList(u, r).as(StateType.Squad) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.Squad

  override def enter(user: User, renderer: Renderer): Task[Unit] = showList(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Строй: все места, на каждом герой, союзник или пусто. */
  private def showList(user: User, renderer: Renderer): Task[Unit] =
    for {
      now   <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
      hero0 <- getHero(user)
      hero  <- SquadDuty.settle(heroDao, content, user, hero0, now, renderer)
      _     <- showLines(user, hero, renderer)
    } yield ()

  private def showLines(user: User, hero: Hero, renderer: Renderer): Task[Unit] = {
      val lines = (1 to AllyRates.Positions).map { pos =>
        if (pos == hero.squad.heroPos) content.format("squad.lineHero", "n" -> pos.toString)
        else hero.squad.allyAt(pos) match {
          case Some(a) =>
            val s = a.statsAt(hero.lvl)
            content.format("squad.lineAlly", "n" -> pos.toString, "name" -> a.name,
              "hp" -> a.hp.toString, "maxHp" -> s.hp.toString, "armor" -> a.armor.toString, "maxArmor" -> s.armor.toString)
          case None => content.format("squad.lineEmpty", "n" -> pos.toString)
        }
      }
      // Союзников бывает до десяти — по двое в ряд, «Назад» под ними: в
      // клавиатуру ВК это укладывается с запасом.
      val buttons = hero.squad.inOrder.zipWithIndex.map { case (a, i) =>
        Choice("SquadAlly", Choice.fit(a.name), data = Map("pos" -> a.position.toString), row = Some(i / SquadState.PerRow))
      }
      val rows = (hero.squad.allies.size + SquadState.PerRow - 1) / SquadState.PerRow
      val back = content.choice("BackFromSquad", "squad.back").copy(row = Some(rows))
      renderer.show(user, Screen(content.text("squad.title") + "\n" + lines.mkString("\n"), buttons :+ back))
  }

  /** Карточка союзника: статы, текущее состояние, позиция, кнопки. Ключ здесь
    * — место в строю: поднятых с алтаря в отряде может быть несколько, и вид
    * их не различает. */
  private def showAlly(user: User, at: Int, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap { hero =>
      hero.squad.allyAt(at) match {
        case None => showList(user, renderer)
        case Some(a) =>
          val s = a.statsAt(hero.lvl)
          val text = content.format("squad.card",
            "name" -> a.name, "race" -> a.kind.race.toString, "element" -> a.kind.element.emoji, "pos" -> a.position.toString,
            "hp" -> a.hp.toString, "maxHp" -> s.hp.toString, "armor" -> a.armor.toString, "maxArmor" -> s.armor.toString,
            "energy" -> a.energy.toString, "maxEnergy" -> s.energy.toString,
            "atk" -> s.atk.toString, "accuracy" -> s.accuracy.toString, "defence" -> s.defence.toString, "evasion" -> s.evasion.toString)
          // Мест в строю одиннадцать — кнопки перестановки идут по трое в ряд.
          val moves = (1 to AllyRates.Positions).filter(_ != a.position).zipWithIndex.map { case (pos, i) =>
            val who = if (pos == hero.squad.heroPos) content.text("squad.posHero")
                      else hero.squad.allyAt(pos).map(x => Choice.fit(x.name)).getOrElse(content.text("squad.posFree"))
            Choice("SquadMove", Choice.fit(content.format("squad.moveLabel", "n" -> pos.toString, "who" -> who)),
              data = Map("pos" -> a.position.toString, "to" -> pos.toString), row = Some(i / SquadState.MovesPerRow))
          }.toList
          val rows = (moves.size + SquadState.MovesPerRow - 1) / SquadState.MovesPerRow
          val dismiss = content.choice("SquadDismiss", "squad.dismissLabel")
            .copy(data = Map("pos" -> a.position.toString), row = Some(rows))
          val back = content.choice("SquadList", "squad.toList").copy(row = Some(rows + 1))
          renderer.show(user, Screen(text, moves ++ List(dismiss, back)))
      }
    }

  private def move(user: User, ua: UserAction, renderer: Renderer): Task[Unit] =
    (parseAlly(ua), payloadField(ua, "to").flatMap(_.toIntOption)) match {
      case (Some(from), Some(to)) =>
        getHero(user).flatMap { hero =>
          hero.squad.allyAt(from) match {
            case None => showList(user, renderer)
            case Some(a) =>
              val moved = hero.squad.moveAt(from, to)
              heroDao.updateSquad(user.userId, moved) *>
                renderer.show(user, Screen(content.format("squad.moved", "name" -> a.name, "n" -> to.toString), Nil)) *>
                showAlly(user, to, renderer)
          }
        }
      case _ => showList(user, renderer)
    }

  private def confirmDismiss(user: User, at: Int, renderer: Renderer): Task[Unit] =
    withAllyAt(user, at, renderer) { a =>
      renderer.show(user, Screen(content.format("squad.dismissConfirm", "name" -> a.name), List(
        Choice("SquadDismissDo", content.text("squad.dismissYes"), data = Map("pos" -> at.toString), color = ChoiceColor.Negative, row = Some(0)),
        Choice("SquadAlly", content.text("squad.dismissNo"), data = Map("pos" -> at.toString), row = Some(0)))))
    }

  private def dismiss(user: User, at: Int, renderer: Renderer): Task[Unit] =
    withAllyAt(user, at, renderer) { a =>
      getHero(user).flatMap { hero =>
        heroDao.updateSquad(user.userId, hero.squad.dismissAt(at)) *>
          renderer.show(user, Screen(content.format("squad.dismissed", "name" -> a.name), Nil)) *>
          showList(user, renderer)
      }
    }

  private def withAllyAt(user: User, at: Int, renderer: Renderer)(f: Ally => Task[Unit]): Task[Unit] =
    getHero(user).flatMap(_.squad.allyAt(at).fold(showList(user, renderer))(f))

  private def withAlly(ua: UserAction)(f: Int => Task[Unit]): Task[Unit] =
    parseAlly(ua).fold[Task[Unit]](ZIO.unit)(f)

  private def parseAlly(ua: UserAction): Option[Int] =
    payloadField(ua, "pos").flatMap(_.toIntOption)

  private def payloadField(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object SquadState {
  /** Сколько кнопок союзников и перестановок помещается в ряд клавиатуры. */
  val PerRow: Int      = 2
  val MovesPerRow: Int = 3
}
