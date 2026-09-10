package pangea.service.state.states.hero

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder, jawn}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.{ItemSet, PassiveKind}
import pangea.model.skill.Skill
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{ItemMenu, State, UiScene, UserAction}
import zio.{Task, ZIO}

/**
 * Экран «Навыки» — список кнопками ВСЕХ активных умений и пассивок, которые
 * сейчас действуют на герое (сняты с надетого снаряжения): активные — с
 * оружия/нагрудника ([[Hero.activeSkillSlots]]), пассивные — со всего
 * остального снаряжения ([[pangea.model.hero.Equipment.passiveKinds]], дубли
 * уже схлопнуты в множество). Нажатие на кнопку показывает полное описание
 * умения; список — только просмотр, никаких действий тут не происходит.
 */
case class SkillsState(heroDao: HeroDao, content: SceneContent) extends State {
  import SkillsState._

  private val branch = new Branch(
    routes = Map(
      "SkillsList"     -> Target.Run { (u, _, r) => writeScene(u, SkillsScene(page = Some(0))) *> showList(u, r).as(StateType.Skills) },
      "SkillsPrev"     -> Target.Run { (u, _, r) => navigate(u, r, -1) },
      "SkillsNext"     -> Target.Run { (u, _, r) => navigate(u, r, +1) },
      "BackFromSkills" -> Target.Goto(StateType.HeroStats)
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = Set(StateType.HeroStats, StateType.Skills)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    writeScene(user, SkillsScene(page = Some(0))) *> showList(user, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Список ──────────────────────────────────────────────────────────────────

  private def showList(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      items  = entries(hero)
      scene <- readScene(user)
      _ <- if (items.isEmpty)
             renderer.show(user, Screen(content.text("skills.empty"),
               List(content.choice("BackFromSkills", "skills.back"))))
           else {
             val (pageItems, totalPages, page) = ItemMenu.page(items, scene.page.getOrElse(0))
             val header  = content.text("skills.header") + (if (totalPages > 1) s" (${page + 1}/$totalPages)" else "")
             val btns    = pageItems.zipWithIndex.map { case (e, idx) =>
               Choice(e.buttonId, ItemMenu.truncate(e.label), row = Some(idx))
             }
             val nav = navRow(page, totalPages)
             renderer.show(user, Screen(header, btns ++ nav))
           }
    } yield StateType.Skills

  private def navigate(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    for {
      hero  <- getHero(user)
      (_, totalPages, _) = ItemMenu.page(entries(hero), 0)
      scene <- readScene(user)
      cur    = scene.page.getOrElse(0)
      np     = (cur + delta).max(0).min(totalPages - 1)
      _     <- writeScene(user, scene.copy(page = Some(np)))
      res   <- showList(user, renderer)
    } yield res

  private def navRow(page: Int, totalPages: Int): List[Choice] = {
    val row = ItemMenu.NavRow
    List(
      Some(content.choice("BackFromSkills", "skills.back").copy(row = Some(row))),
      Option.when(page > 0)(content.choice("SkillsPrev", "common.prev").copy(row = Some(row))),
      Option.when(page < totalPages - 1)(content.choice("SkillsNext", "common.next").copy(row = Some(row)))
    ).flatten
  }

  // ── Детальный экран одного умения ──────────────────────────────────────────

  private def showDetail(user: User, entry: SkillEntry, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      text  = s"${entry.label}\n\n${entry.describe(hero)}"
      _    <- renderer.show(user, Screen(text,
                List(content.choice("SkillsList", "skills.back").copy(color = ChoiceColor.Negative))))
    } yield StateType.Skills

  // ── Fallback: динамические id вида ActiveSkill_<itemId> / PassiveSkill_<kind> ─

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      res  <- parseAction(ua.payload)
                .flatMap(a => entries(hero).find(_.buttonId == a)) match {
                  case Some(entry) => showDetail(user, entry, renderer)
                  case None        => showList(user, renderer)
                }
    } yield res

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def readScene(user: User): Task[SkillsScene] =
    UiScene.read(heroDao, user.userId, UiScene.Skills, SkillsScene())

  private def writeScene(user: User, scene: SkillsScene): Task[Unit] =
    UiScene.write(heroDao, user.userId, UiScene.Skills, scene)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object SkillsState {

  /** Одно умение/пассивка в списке: активные — с оружия/нагрудника (ключ —
   *  id предмета, т.к. у двух предметов с «одним» скиллом независимые кулдауны),
   *  пассивные — по виду пассивки (дубли уже схлопнуты в `Equipment.passiveKinds`). */
  sealed trait SkillEntry {
    def buttonId: String
    def label: String
    def describe(hero: Hero): String
  }
  object SkillEntry {
    final case class Active(itemId: Long, skill: Skill) extends SkillEntry {
      def buttonId: String = s"$ActivePrefix$itemId"
      def label: String    = skill.label
      def describe(hero: Hero): String = skill.describe(hero)
    }
    final case class Passive(kind: PassiveKind) extends SkillEntry {
      def buttonId: String = s"$PassivePrefix${kind.entryName}"
      def label: String    = kind.label
      def describe(hero: Hero): String = kind.describe
    }

    /** Набор снаряжения: в списке — сколько предметов надето, в описании —
     *  только те пороги, до которых игрок добрал. */
    final case class Set(set: ItemSet) extends SkillEntry {
      def buttonId: String = s"$SetPrefix${set.entryName}"
      def label: String    = s"🛡 ${set.label}"
      def describe(hero: Hero): String = {
        val sets   = hero.sets
        val worn   = sets.pieces(set)
        val header = s"Надето предметов: $worn из ${ItemSet.Thresholds.max}"
        val lines  = sets.bonuses(set).map { b =>
          val tail = if (b.active) "" else " (пока не действует)"
          s"• ${b.pieces}: ${b.text}$tail"
        }
        val next = ItemSet.Thresholds.find(_ > worn)
          .map(n => s"\n\nСледующий бонус — при $n предметах.")
          .getOrElse("")
        (header +: lines).mkString("\n") + next
      }
    }
  }

  private val ActivePrefix  = "ActiveSkill_"
  private val PassivePrefix = "PassiveSkill_"
  private val SetPrefix     = "Set_"

  /** Все умения/пассивки/наборы, реально действующие на герое прямо сейчас —
   *  снятые с надетого снаряжения. Активные (оружие/нагрудник) идут первыми,
   *  затем пассивки по алфавиту метки, затем наборы, набравшие хотя бы один
   *  порог — стабильный порядок между перерисовками. */
  def entries(hero: Hero): List[SkillEntry] =
    hero.activeSkillSlots.map(s => SkillEntry.Active(s.itemId, s.skill)) ++
      hero.passives.kinds.toList.sortBy(_.label).map(SkillEntry.Passive) ++
      hero.sets.activeBonuses.map { case (set, _) => SkillEntry.Set(set) }

  final case class SkillsScene(page: Option[Int] = None)
  object SkillsScene {
    implicit val encoder: Encoder[SkillsScene] = deriveEncoder
    implicit val decoder: Decoder[SkillsScene] = deriveDecoder
  }
}
