package pangea.service.state.states

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
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
      "Guild"           -> Target.Run { (user, _, renderer) =>
                             renderer.show(user, Screen(content.text("globalMap.guild"), Nil)).as(StateType.GlobalMap) },
      "Construction"    -> Target.Goto(StateType.Construction),
      "ReturnToDungeon" -> Target.Goto(StateType.Dungeon),
      "StreetMerchants" -> Target.Run { (user, _, renderer) =>
                             renderer.show(user, content.screen("globalMap.streetMerchants")).as(StateType.GlobalMap) },
      "MerchantRichelieu" -> Target.Goto(StateType.Merchant),
      "ReturnToCity"    -> Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.GlobalMap) },
      "OpenCharacter"   -> Target.Run { (user, _, _) => CharacterMenu.open(heroDao, user.userId, StateType.GlobalMap) }
    ),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.GlobalMap) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.HeroStats

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero <- getHero(user)
      text  = content.format("globalMap.enter.text",
                "heroHp"  -> hero.fightStats.hp.toString,
                "heroMax" -> hero.effectiveMaxHp(now).toString,
                "heroArmor" -> hero.fightStats.armor.toString,
                "gold"    -> hero.gold.toString)
      _    <- renderer.show(user, Screen(text, content.screen("globalMap.enter").choices))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
