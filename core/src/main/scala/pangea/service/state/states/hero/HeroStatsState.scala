package pangea.service.state.states.hero

import java.util.concurrent.TimeUnit
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

case class HeroStatsState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "Back"           -> Target.Run { (user, _, _) => returnToCaller(user) },
      "OpenInventory"  -> Target.Goto(StateType.Inventory),
      "OpenEquipment"  -> Target.Goto(StateType.Equipment),
      "OpenTraumas"  -> Target.Run { (user, _, renderer) => showTraumasScreen(user, renderer).as(StateType.HeroStats) },
      "Upgrade"      -> Target.Run { (user, _, renderer) => showUpgradeScreen(user, renderer).as(StateType.HeroStats) },
      "UpgradeStr"   -> Target.Run { (user, _, renderer) => applyUpgrade(user, renderer, "str") },
      "UpgradeVit"   -> Target.Run { (user, _, renderer) => applyUpgrade(user, renderer, "vit") },
      "UpgradeAgi"   -> Target.Run { (user, _, renderer) => applyUpgrade(user, renderer, "agi") },
      "UpgradeInt"   -> Target.Run { (user, _, renderer) => applyUpgrade(user, renderer, "int") },
      "BackToStats"  -> Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.HeroStats) }
    ),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.HeroStats) }
  )

  // «Назад» возвращает в локацию-источник (`return_state`), поэтому в targetStates —
  // все хабы, из которых можно открыть «Персонаж», плюс под-экраны инвентаря/снаряжения.
  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(
      StateType.Dungeon, StateType.GlobalMap, StateType.Tavern,
      StateType.Merchant, StateType.QuestBoard, StateType.Innkeeper)

  // Куда вернуться из «Персонажа»: читаем return_state, по умолчанию — лабиринт.
  private def returnToCaller(user: User): Task[StateType] =
    heroDao.readReturnState(user.userId).map(_.getOrElse(StateType.Dungeon))

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
                  content.choice("UpgradeStr", "heroStats.upgradeStr"),
                  content.choice("UpgradeVit", "heroStats.upgradeVit"),
                  content.choice("UpgradeAgi", "heroStats.upgradeAgi"),
                  content.choice("UpgradeInt", "heroStats.upgradeInt"),
                  content.choice("BackToStats", "heroStats.backToStats")
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

  private def showTraumasScreen(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero <- getHero(user)
      _    <- renderer.show(user, buildTraumasScreen(hero, now))
    } yield ()

  private def buildTraumasScreen(hero: Hero, nowMs: Long): Screen = {
    val traumas = hero.activeTraumas(nowMs)
    val back    = content.choice("BackToStats", "heroStats.backToStats")
    if (traumas.isEmpty)
      Screen(content.text("heroStats.traumaEmpty"), List(back))
    else {
      val remaining = hero.traumaRemainingText(nowMs).getOrElse("—")
      val header    = content.format("heroStats.traumaScreenTitle", "remaining" -> remaining)
      val lines     = traumas.map { t =>
        content.format("heroStats.traumaLine", "name" -> t.name, "penalties" -> t.penalties.shortText)
      }
      Screen(header + "\n" + lines.mkString("\n"), List(back))
    }
  }

  private def buildStatsScreen(hero: Hero, nowMs: Long): Screen = {
    val traumaLine = hero.traumaRemainingText(nowMs).map { remaining =>
      val names = hero.activeTraumas(nowMs).map(_.name)
      val namesStr = if (names.isEmpty) "Травмы" else names.mkString(", ")
      "\n" + content.format("heroStats.traumaActive",
        "traumaNames" -> namesStr,
        "remaining"   -> remaining)
    }.getOrElse("")
    val choices = List(
      Some(content.choice("OpenInventory", "heroStats.inventory").copy(row = Some(0))),
      Some(content.choice("OpenEquipment", "heroStats.equipment").copy(row = Some(0))),
      Option.when(hero.activeTraumas(nowMs).nonEmpty)(
        content.choice("OpenTraumas", "heroStats.traumas").copy(color = ChoiceColor.Secondary, row = Some(1))),
      Option.when(hero.upgradePoints > 0)(
        content.choice("Upgrade", "heroStats.upgrade").copy(color = ChoiceColor.Positive, row = Some(1))),
      Some(content.choice("Back", "heroStats.leave").copy(color = ChoiceColor.Negative, row = Some(2)))
    ).flatten
    Screen(hero.getInfo(nowMs) + traumaLine, choices)
  }

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero found for user ${user.userId}"))
}
