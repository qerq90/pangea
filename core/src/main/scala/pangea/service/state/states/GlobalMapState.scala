package pangea.service.state.states

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{CharacterMenu, State, UserAction}
import zio.{Task, ZIO}
import java.util.concurrent.TimeUnit

case class GlobalMapState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "Tavern"          -> Target.Goto(StateType.Tavern),
      "Guild"           -> Target.Goto(StateType.Guild),
      "HarborQuarter"   -> Target.Goto(StateType.HarborQuarter),
      "MarketSquare"    -> Target.Goto(StateType.MarketSquare),
      "ReturnToDungeon" -> Target.Goto(StateType.Dungeon),
      "OpenCharacter"   -> Target.Run { (user, _, _) => CharacterMenu.open(heroDao, user.userId, StateType.GlobalMap) },
      "LeaveCity"       -> Target.Goto(StateType.Outskirts)
    ),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.GlobalMap) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.HeroStats

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero <- getHero(user)
      text  = content.format("globalMap.enter.text",
                "heroHp"    -> hero.fightStats.hp.toString,
                "heroMax"   -> hero.effectiveMaxHp(now).toString,
                "heroArmor" -> hero.fightStats.armor.toString,
                "gold"      -> hero.gold.toString)
      byId  = content.screen("globalMap.enter").choices.map(c => c.id -> c).toMap
      choices = List(
        byId("HarborQuarter").copy(row = Some(0)),
        byId("MarketSquare").copy(row = Some(0)),
        byId("Tavern").copy(color = ChoiceColor.Positive, row = Some(1)),
        byId("Guild").copy(color = ChoiceColor.Positive, row = Some(1)),
        byId("OpenCharacter").copy(row = Some(2)),
        byId("ReturnToDungeon").copy(row = Some(2)),
        byId("LeaveCity").copy(color = ChoiceColor.Negative, row = Some(3))
      )
      _ <- renderer.show(user, Screen(text, choices))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
