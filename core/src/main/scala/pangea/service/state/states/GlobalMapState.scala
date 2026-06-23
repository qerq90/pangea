package pangea.service.state.states

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}
import java.util.concurrent.TimeUnit

case class GlobalMapState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "Tavern"          -> Target.Run { (user, _, renderer) => showTavern(user, renderer).as(StateType.GlobalMap) },
      "Guild"           -> Target.Run { (user, _, renderer) =>
                             renderer.show(user, Screen(content.text("globalMap.guild"), Nil)).as(StateType.GlobalMap) },
      "Construction"    -> Target.Run { (user, _, renderer) =>
                             renderer.show(user, Screen(content.text("globalMap.construction"), Nil)).as(StateType.GlobalMap) },
      "ReturnToDungeon" -> Target.Goto(StateType.Dungeon),
      "Heal"            -> Target.Run { (user, _, renderer) => heal(user, renderer) },
      "StreetMerchants" -> Target.Run { (user, _, renderer) =>
                             renderer.show(user, content.screen("globalMap.streetMerchants")).as(StateType.GlobalMap) },
      "MerchantRichelieu" -> Target.Goto(StateType.Merchant),
      "ReturnToCity"    -> Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.GlobalMap) },
      "LeaveTavern"     -> Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.GlobalMap) }
    ),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.GlobalMap) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

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

  private def showTavern(user: User, renderer: Renderer): Task[Unit] =
    getHero(user).flatMap { hero =>
      val cost = healCost(hero)
      val text = content.format("globalMap.tavern.text",
        "cost" -> cost.toString,
        "gold" -> hero.gold.toString)
      renderer.show(user, Screen(text, content.screen("globalMap.tavern").choices))
    }

  private def heal(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero <- getHero(user)
      cost  = healCost(hero)
      maxHp = hero.effectiveMaxHp(now)
      result <- if (hero.fightStats.hp >= maxHp && !hero.traumaActive(now))
                  renderer.show(user, Screen(content.text("globalMap.alreadyFull"), Nil))
                    .as(StateType.GlobalMap)
                else if (hero.gold < cost)
                  renderer.show(user, Screen(
                    content.format("globalMap.notEnoughGold",
                      "cost" -> cost.toString, "gold" -> hero.gold.toString), Nil))
                    .as(StateType.GlobalMap)
                else
                  for {
                    _ <- heroDao.updateFightStats(user.userId, hero.fightStats.copy(hp = maxHp, armor = hero.maxArmor))
                    _ <- heroDao.updateGold(user.userId, hero.gold - cost)
                    _ <- heroDao.updateTrauma(user.userId, None, Nil)
                    _ <- renderer.show(user, Screen(content.text("globalMap.healed"), Nil))
                    _ <- enter(user, renderer)
                  } yield StateType.GlobalMap
    } yield result

  private def healCost(hero: Hero): Long = (hero.dungeonLevel * 10L).max(10L)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}
