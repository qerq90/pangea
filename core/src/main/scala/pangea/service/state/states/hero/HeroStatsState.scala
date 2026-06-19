package pangea.service.state.states.hero

import java.util.concurrent.TimeUnit
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

case class HeroStatsState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "Back"           -> Target.Goto(StateType.Dungeon),
      "OpenInventory"  -> Target.Goto(StateType.Inventory),
      "OpenEquipment"  -> Target.Goto(StateType.Equipment),
      "Upgrade"      -> Target.Run { (user, _, renderer) => showUpgradeScreen(user, renderer).as(StateType.HeroStats) },
      "UpgradeStr"   -> Target.Run { (user, _, renderer) => applyUpgrade(user, renderer, "str") },
      "UpgradeVit"   -> Target.Run { (user, _, renderer) => applyUpgrade(user, renderer, "vit") },
      "UpgradeAgi"   -> Target.Run { (user, _, renderer) => applyUpgrade(user, renderer, "agi") },
      "UpgradeInt"   -> Target.Run { (user, _, renderer) => applyUpgrade(user, renderer, "int") },
      "BackToStats"  -> Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.HeroStats) }
    ),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.HeroStats) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero <- getHero(user)
      _    <- renderer.show(user, buildStatsScreen(hero, now))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def showUpgradeScreen(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero <- getHero(user)
      _    <- if (hero.upgradePoints <= 0)
                renderer.show(user, Screen(content.text("heroStats.noPoints"), buildStatsScreen(hero, now).choices))
              else {
                val text = content.format("heroStats.upgradeScreen", "points" -> hero.upgradePoints.toString)
                val choices = List(
                  Choice("UpgradeStr", content.text("heroStats.upgradeStr")),
                  Choice("UpgradeVit", content.text("heroStats.upgradeVit")),
                  Choice("UpgradeAgi", content.text("heroStats.upgradeAgi")),
                  Choice("UpgradeInt", content.text("heroStats.upgradeInt")),
                  Choice("BackToStats", content.text("heroStats.backToStats"))
                )
                renderer.show(user, Screen(text, choices))
              }
    } yield ()

  private def applyUpgrade(user: User, renderer: Renderer, stat: String): Task[StateType] =
    for {
      hero <- getHero(user)
      _    <- if (hero.upgradePoints <= 0)
                renderer.show(user, Screen(content.text("heroStats.noPoints"), Nil))
              else {
                val newBase = stat match {
                  case "str" => hero.baseStats.copy(str = hero.baseStats.str + 1)
                  case "vit" => hero.baseStats.copy(vit = hero.baseStats.vit + 1)
                  case "agi" => hero.baseStats.copy(agi = hero.baseStats.agi + 1)
                  case "int" => hero.baseStats.copy(int = hero.baseStats.int + 1)
                  case _     => hero.baseStats
                }
                for {
                  _ <- heroDao.updateBaseStats(user.userId, newBase)
                  _ <- heroDao.updateExpAndLevel(user.userId, hero.exp, hero.lvl, hero.upgradePoints - 1)
                  _ <- renderer.show(user, Screen(content.text("heroStats.upgradeApplied"), Nil))
                  _ <- showUpgradeScreen(user, renderer)
                } yield ()
              }
    } yield StateType.HeroStats

  private def buildStatsScreen(hero: Hero, nowMs: Long): Screen = {
    val traumaLine = hero.traumaRemainingText(nowMs).map { remaining =>
      val names = hero.activeTraumas(nowMs).map(_.name)
      val namesStr = if (names.isEmpty) "Травмы" else names.mkString(", ")
      "\n" + content.format("heroStats.traumaActive",
        "traumaNames" -> namesStr,
        "remaining"   -> remaining)
    }.getOrElse("")
    val choices = List(
      Some(Choice("OpenInventory", content.text("heroStats.inventory"))),
      Some(Choice("OpenEquipment", content.text("heroStats.equipment"))),
      Option.when(hero.upgradePoints > 0)(Choice("Upgrade", content.text("heroStats.upgrade"))),
      Some(Choice("Back", content.text("heroStats.leave")))
    ).flatten
    Screen(hero.getInfo(nowMs) + traumaLine, choices)
  }

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero found for user ${user.userId}"))
}
