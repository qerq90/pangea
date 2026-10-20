package pangea.service.state.states.hero

import java.util.concurrent.TimeUnit
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.{Achievement, Hero}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.artifact.ArtifactRepository
import pangea.service.state.{AzatData, QuestJournal, SquadDuty, State, UserAction}
import zio.{Task, ZIO}

/** Меню «Персонаж». Первая кнопка ведёт в рюкзак, но только тому, кому есть что
  * в нём открывать: пока ларца, живой сумки и шкафа нет, рюкзак показал бы одну
  * сумку — и герой идёт прямо в инвентарь, как до Лавки Фета. */
case class HeroStatsState(
  heroDao:   HeroDao,
  content:   SceneContent,
  artifacts: Option[ArtifactRepository] = None
) extends State {

  private val branch = new Branch(
    routes = Map(
      "Back"           -> Target.Run { (user, _, _) => returnToCaller(user) },
      "OpenInventory"  -> Target.Goto(StateType.Backpack),
      "OpenBag"        -> Target.Goto(StateType.Inventory),
      "OpenEquipment"  -> Target.Goto(StateType.Equipment),
      "OpenSkills"     -> Target.Goto(StateType.Skills),
      "OpenKnowledge"  -> Target.Goto(StateType.Knowledge),
      "OpenSquad"      -> Target.Goto(StateType.Squad),
      "OpenFriendCode" -> Target.Goto(StateType.FriendCode),
      "OpenTraumas"  -> Target.Run { (user, _, renderer) => showTraumasScreen(user, renderer).as(StateType.HeroStats) },
      "OpenQuests"   -> Target.Run { (user, _, renderer) => showQuests(user, renderer).as(StateType.HeroStats) },
      "OpenAchievements" -> Target.Run { (user, _, renderer) => showAchievements(user, renderer).as(StateType.HeroStats) },
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
      now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero0 <- getHero(user)
      hero  <- SquadDuty.settle(heroDao, content, user, hero0, now, renderer)
      azat  <- AzatData.load(heroDao, user.userId, now)
      keeps <- hasStorage(hero)
      _     <- renderer.show(user, buildStatsScreen(hero, now, azat.blessingActive(now), azat.instantRests, keeps))
    } yield ()

  /** Есть ли у героя хранилище сверх сумки. Репозитория нет — значит и хранилищ
    * нет: так живут тесты и сборки без Лавки Фета. */
  private def hasStorage(hero: Hero): Task[Boolean] =
    artifacts.fold(ZIO.succeed(false): Task[Boolean])(
      _.get(hero.id).map(_.anyOwned).orElseSucceed(false))

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** «📜 Задания»: всё, что в работе, одним сообщением (см. QuestJournal), а
    * под ним — те же кнопки «Персонажа». */
  private def showQuests(user: User, renderer: Renderer): Task[Unit] =
    for {
      now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero  <- getHero(user)
      keeps <- hasStorage(hero)
      text  <- QuestJournal.text(heroDao, content, hero, now)
      _     <- renderer.show(user, Screen(text,
                 buildStatsScreen(hero, now, blessed = false, instantRests = 0, keeps).choices))
    } yield ()

  /** «Достижения»: счёт полученных из всех, полученные — с тем, что дают, а
    * неполученные скрыты: только их число. Кнопки «Персонажа» остаются, как у
    * журнала заданий. */
  private def showAchievements(user: User, renderer: Renderer): Task[Unit] =
    for {
      now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero  <- getHero(user)
      keeps <- hasStorage(hero)
      _     <- renderer.show(user, Screen(HeroStatsState.achievementsText(content, hero),
                 buildStatsScreen(hero, now, blessed = false, instantRests = 0, keeps).choices))
    } yield ()

  private def showUpgradeScreen(user: User, renderer: Renderer): Task[Unit] =
    for {
      now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      hero  <- getHero(user)
      keeps <- hasStorage(hero)
      _    <- if (hero.upgradePoints <= 0)
                renderer.show(user, Screen(content.text("heroStats.noPoints"),
                  buildStatsScreen(hero, now, blessed = false, instantRests = 0, keeps).choices))
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

  private def buildStatsScreen(hero: Hero, nowMs: Long, blessed: Boolean, instantRests: Int,
                               hasStorage: Boolean): Screen = {
    val traumaLine = hero.traumaRemainingText(nowMs).map { remaining =>
      val names = hero.activeTraumas(nowMs).map(_.name)
      val namesStr = if (names.isEmpty) "Травмы" else names.mkString(", ")
      "\n" + content.format("heroStats.traumaActive",
        "traumaNames" -> namesStr,
        "remaining"   -> remaining)
    }.getOrElse("")
    val choices = List(
      Some(
        if (hasStorage) content.choice("OpenInventory", "heroStats.inventory").copy(row = Some(0))
        else content.choice("OpenBag", "heroStats.bag").copy(row = Some(0))),
      Some(content.choice("OpenEquipment", "heroStats.equipment").copy(row = Some(0))),
      Some(content.choice("OpenSkills", "heroStats.skills").copy(row = Some(0))),
      Some(content.choice("OpenKnowledge", "heroStats.knowledge").copy(row = Some(1))),
      Option.when(hero.squad.nonEmpty)(content.choice("OpenSquad", "heroStats.squad").copy(row = Some(1))),
      Option.when(hero.activeTraumas(nowMs).nonEmpty)(
        content.choice("OpenTraumas", "heroStats.traumas").copy(color = ChoiceColor.Secondary, row = Some(1))),
      Option.when(hero.upgradePoints > 0)(
        content.choice("Upgrade", "heroStats.upgrade").copy(color = ChoiceColor.Positive, row = Some(1))),
      Some(content.choice("OpenQuests", "heroStats.quests").copy(row = Some(2))),
      Some(content.choice("OpenAchievements", "heroStats.achievements").copy(row = Some(2))),
      Some(content.choice("OpenFriendCode", "heroStats.friendCode").copy(row = Some(2))),
      // «Назад» — отдельным последним рядом: в третьем ряду ему уже тесно.
      Some(content.choice("Back", "heroStats.leave").copy(color = ChoiceColor.Negative, row = Some(3)))
    ).flatten
    Screen(hero.getInfo(nowMs, blessed, instantRests) + traumaLine, choices)
  }

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero found for user ${user.userId}"))
}

object HeroStatsState {

  /** Текст экрана достижений: «получено из всех», полученные (✅) — в порядке,
    * в каком заведены, с тем, что дают. Неполученные скрыты: ни названия, ни
    * бонуса — только сколько их ещё. */
  def achievementsText(content: SceneContent, hero: Hero): String = {
    val got    = Achievement.values.toList.filter(hero.hasAchievement)
    val hidden = Achievement.values.size - got.size
    val lines  =
      if (got.isEmpty) List(content.text("heroStats.achievementsNone"))
      else got.map(a => content.format("heroStats.achievementLine", "title" -> a.title, "bonus" -> a.bonusLine))
    val locked = Option.when(hidden > 0)(content.format("heroStats.achievementsHidden", "count" -> hidden.toString))
    content.format("heroStats.achievementsHeader",
      "got" -> got.size.toString, "total" -> Achievement.values.size.toString) + "\n\n" +
      (lines ++ locked.toList).mkString("\n")
  }
}
