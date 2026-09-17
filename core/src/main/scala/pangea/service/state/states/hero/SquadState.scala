package pangea.service.state.states.hero

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.squad.{AllyKind, AllyRates}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

/** «Отряд» в меню персонажа: строй с позициями, карточка союзника — его
  * состояние и место, перестановка (занятое место — меняются местами, в том
  * числе с героем) и увольнение. Позиции те же, что в бою: союзник на позиции N
  * стоит против врага на месте N. */
case class SquadState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "SquadList"   -> Target.Run { (u, _, r) => showList(u, r).as(StateType.Squad) },
      "SquadAlly"   -> Target.Run { (u, ua, r) => withAlly(ua)(k => showAlly(u, k, r)).as(StateType.Squad) },
      "SquadMove"   -> Target.Run { (u, ua, r) => move(u, ua, r).as(StateType.Squad) },
      "SquadDismiss" -> Target.Run { (u, ua, r) => withAlly(ua)(k => confirmDismiss(u, k, r)).as(StateType.Squad) },
      "SquadDismissDo" -> Target.Run { (u, ua, r) => withAlly(ua)(k => dismiss(u, k, r)).as(StateType.Squad) },
      "BackFromSquad" -> Target.Goto(StateType.HeroStats)
    ),
    fallback = Target.Run { (u, _, r) => showList(u, r).as(StateType.Squad) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.Squad

  override def enter(user: User, renderer: Renderer): Task[Unit] = showList(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Строй: позиции 1..4, на каждой герой, союзник или пусто. */
  private def showList(user: User, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap { hero =>
      val lines = (1 to AllyRates.Positions).map { pos =>
        if (pos == hero.squad.heroPos) content.format("squad.lineHero", "n" -> pos.toString)
        else hero.squad.allyAt(pos) match {
          case Some(a) =>
            val s = a.kind.stats(hero.lvl)
            content.format("squad.lineAlly", "n" -> pos.toString, "name" -> a.name,
              "hp" -> a.hp.toString, "maxHp" -> s.hp.toString, "armor" -> a.armor.toString, "maxArmor" -> s.armor.toString)
          case None => content.format("squad.lineEmpty", "n" -> pos.toString)
        }
      }
      val buttons = hero.squad.inOrder.zipWithIndex.map { case (a, i) =>
        Choice("SquadAlly", Choice.fit(a.name), data = Map("ally" -> a.kind.entryName), row = Some(i / 2))
      }
      val back = content.choice("BackFromSquad", "squad.back").copy(row = Some(2))
      renderer.show(user, Screen(content.text("squad.title") + "\n" + lines.mkString("\n"), buttons :+ back))
    }

  /** Карточка союзника: статы на уровень героя, текущее состояние, позиция, кнопки. */
  private def showAlly(user: User, kind: AllyKind, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap { hero =>
      hero.squad.allies.find(_.kind == kind) match {
        case None => showList(user, renderer)
        case Some(a) =>
          val s = kind.stats(hero.lvl)
          val text = content.format("squad.card",
            "name" -> a.name, "race" -> kind.race.toString, "element" -> kind.element.emoji, "pos" -> a.position.toString,
            "hp" -> a.hp.toString, "maxHp" -> s.hp.toString, "armor" -> a.armor.toString, "maxArmor" -> s.armor.toString,
            "energy" -> a.energy.toString, "maxEnergy" -> s.energy.toString,
            "atk" -> s.atk.toString, "accuracy" -> s.accuracy.toString, "defence" -> s.defence.toString, "evasion" -> s.evasion.toString)
          val moves = (1 to AllyRates.Positions).filter(_ != a.position).map { pos =>
            val who = if (pos == hero.squad.heroPos) content.text("squad.posHero")
                      else hero.squad.allyAt(pos).map(x => Choice.fit(x.name)).getOrElse(content.text("squad.posFree"))
            Choice("SquadMove", Choice.fit(content.format("squad.moveLabel", "n" -> pos.toString, "who" -> who)),
              data = Map("ally" -> kind.entryName, "pos" -> pos.toString), row = Some(0))
          }.toList
          val dismiss = content.choice("SquadDismiss", "squad.dismissLabel")
            .copy(data = Map("ally" -> kind.entryName), row = Some(1))
          val back = content.choice("SquadList", "squad.toList").copy(row = Some(2))
          renderer.show(user, Screen(text, moves ++ List(dismiss, back)))
      }
    }

  private def move(user: User, ua: UserAction, renderer: Renderer): Task[Unit] =
    (parseAlly(ua), payloadField(ua, "pos").flatMap(_.toIntOption)) match {
      case (Some(kind), Some(pos)) =>
        getHero(user).flatMap { hero =>
          val moved = hero.squad.move(kind, pos)
          heroDao.updateSquad(user.userId, moved) *>
            renderer.show(user, Screen(content.format("squad.moved", "name" -> kind.name, "n" -> pos.toString), Nil)) *>
            showAlly(user, kind, renderer)
        }
      case _ => showList(user, renderer)
    }

  private def confirmDismiss(user: User, kind: AllyKind, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(content.format("squad.dismissConfirm", "name" -> kind.name), List(
      Choice("SquadDismissDo", content.text("squad.dismissYes"), data = Map("ally" -> kind.entryName), color = ChoiceColor.Negative, row = Some(0)),
      Choice("SquadAlly", content.text("squad.dismissNo"), data = Map("ally" -> kind.entryName), row = Some(0)))))

  private def dismiss(user: User, kind: AllyKind, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap { hero =>
      heroDao.updateSquad(user.userId, hero.squad.dismiss(kind)) *>
        renderer.show(user, Screen(content.format("squad.dismissed", "name" -> kind.name), Nil)) *>
        showList(user, renderer)
    }

  private def withAlly(ua: UserAction)(f: AllyKind => Task[Unit]): Task[Unit] =
    parseAlly(ua).fold[Task[Unit]](ZIO.unit)(f)

  private def parseAlly(ua: UserAction): Option[AllyKind] =
    payloadField(ua, "ally").flatMap(AllyKind.withNameOption)

  private def payloadField(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
