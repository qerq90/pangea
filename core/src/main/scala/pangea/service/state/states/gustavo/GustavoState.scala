package pangea.service.state.states.gustavo

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.ItemDetails
import pangea.model.quest.NpcQuest
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.{NpcQuestDialog, NpcQuestLog, State, UserAction}
import zio.{Task, ZIO}

/**
 * Лавка знахаря «Густаво» — второй уличный торговец на Торговой площади. Это только меню
 * из четырёх кнопок; каждая ведёт в свой отдельный стейт:
 *  1. «Исцеление травм»          → [[GustavoHealState]] (зелёная кнопка, краснеет на кулдауне);
 *  2. «Увеличение характеристик»  → [[GustavoBoostState]];
 *  3. «Сдать травы»               → заглушка (трав пока нет);
 *  4. «Пополнить припасы»         → [[GustavoSuppliesState]] (фляга/пояс).
 * Цвет кнопки лечения зависит от кулдауна зелья ([[GustavoData.healCooldownUntil]]).
 */
case class GustavoState(
  heroDao: HeroDao,
  content: SceneContent
) extends State with GustavoScene {

  /** «Подопытный»: выпить зелье, победить троих, пока оно действует, и
    * отчитаться — Густаво наполняет флягу и рассказывает, из чего варит. */
  private val quest = NpcQuestDialog(heroDao, content, NpcQuest.Gustavo, "Gus")

  private val branch = new Branch(
    routes = Map(
      quest.questAction   -> Target.Run { (u, _, r) => questTalk(u, r) },
      quest.acceptAction  -> Target.Run { (u, _, r) => quest.accept(u, r) *> renderMenu(u, r).as(StateType.Gustavo) },
      quest.declineAction -> Target.Run { (u, _, r) => renderMenu(u, r).as(StateType.Gustavo) },
      "Heal"     -> Target.Goto(StateType.GustavoHeal),
      "Boost"    -> Target.Goto(StateType.GustavoBoost),
      "Herbs"    -> Target.Run { (u, _, r) =>
                      r.show(u, Screen(content.text("gustavo.herbsStub"), Nil)) *> renderMenu(u, r).as(StateType.Gustavo) },
      "Supplies" -> Target.Goto(StateType.GustavoSupplies),
      "Back"     -> Target.Goto(StateType.MarketSquare)
    ),
    fallback = Target.Run { (u, _, r) => renderMenu(u, r).as(StateType.Gustavo) }
  )

  override def targetStates: Set[StateType] =
    Set(StateType.MarketSquare, StateType.Gustavo, StateType.GustavoHeal,
        StateType.GustavoBoost, StateType.GustavoSupplies)

  override def enter(user: User, renderer: Renderer): Task[Unit] = renderMenu(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def renderMenu(user: User, renderer: Renderer): Task[Unit] =
    for {
      now    <- nowMs
      data   <- loadData(user)
      quests <- quest.load(user)
      _      <- renderer.show(user, menuScreen(data, now, quest.button(quests)))
    } yield ()

  /** Кнопка задания: завязка, пока не взято; дальше — по шагу: напомнить про
    * зелье, посчитать побитых (или налить ещё, если зелье выветрилось), принять
    * отчёт. */
  private def questTalk(user: User, renderer: Renderer): Task[StateType] =
    for {
      quests <- quest.load(user)
      p       = quests.of(NpcQuest.Gustavo)
      _ <- if (p.done) renderMenu(user, renderer)
           else if (!p.taken) quest.offer(user, renderer, quest.text("intro"))
           else p.step match {
             case 1 => renderer.show(user, Screen(quest.text("step1Hint"), Nil)) *> renderMenu(user, renderer)
             case 2 => questCount(user, renderer)
             case _ => questReport(user, renderer)
           }
    } yield StateType.Gustavo

  /** Шаг 2: зелье ещё действует — сколько побито; выветрилось — ещё одно
    * бесплатное и счёт заново. */
  private def questCount(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      _ <- if (GustavoState.potionActive(hero, now))
             quest.load(user).flatMap(q => renderer.show(user, Screen(
               quest.format("step2Fail", "count" -> q.of(NpcQuest.Gustavo).counter.toString), Nil)))
           else
             NpcQuestLog.modify(heroDao, user.userId)(_.update(NpcQuest.Gustavo)(_.copy(step = 1, counter = 0L, bonus = true))) *>
               renderer.show(user, Screen(quest.text("step2Expired"), Nil))
      _ <- renderMenu(user, renderer)
    } yield ()

  /** Шаг 3: отчёт принят — фляга до краёв (или серебро «на флягу»), серебро, опыт. */
  private def questReport(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      flask = hero.equipment.flask
      refilled = flask.details match {
        case f: ItemDetails.Flask => Some(flask.copy(details = f.refilled))
        case _                    => None
      }
      silver = GustavoState.QuestSilver + (if (refilled.isEmpty) GustavoState.QuestFlaskSilver else 0L)
      _    <- ZIO.foreachDiscard(refilled)(fl => heroDao.updateEquipment(user.userId, hero.equipment.copy(flask = fl)))
      _    <- heroDao.updateSilver(user.userId, hero.silver + silver)
      done <- quest.complete(user, hero, identity)
      (_, expLine) = done
      _    <- renderer.show(user, Screen(quest.text("outro"), Nil))
      _    <- renderer.show(user, Screen(quest.format(if (refilled.isDefined) "rewardFlask" else "rewardNoFlask",
                "silver" -> silver.toString, "exp" -> expLine), Nil))
      _    <- renderMenu(user, renderer)
    } yield ()

  private def menuScreen(data: GustavoData, now: Long, questBtn: Option[Choice]): Screen = {
    val healBtn = data.healCooldownUntil.filter(_ > now).map(_ - now) match {
      case Some(left) =>
        content.choice("Heal", "gustavo.cooldownLabel", "mins" -> minsOf(left)).copy(color = ChoiceColor.Negative)
      case None =>
        content.choice("Heal", "gustavo.healLabel").copy(color = ChoiceColor.Positive)
    }
    val boostBtn    = content.choice("Boost", "gustavo.boostLabel")
    val herbsBtn    = content.choice("Herbs", "gustavo.herbsLabel")
    val suppliesBtn = content.choice("Supplies", "gustavo.suppliesLabel")
    val choices = List(healBtn, boostBtn, herbsBtn, suppliesBtn) ++ questBtn.toList :+ content.choice("Back", "gustavo.back")
    Screen(content.text("gustavo.menu.text"), choices)
  }
}

object GustavoState {
  /** Серебро за отчёт подопытного. */
  val QuestSilver: Long = 50L

  /** «На флягу» — если фляги у героя нет и наполнять нечего. */
  val QuestFlaskSilver: Long = 75L

  /** Действует ли сейчас хоть одно зелье Густаво. */
  def potionActive(hero: Hero, now: Long): Boolean =
    BoostStat.all.exists(bs => hero.statBoosts.remainingMs(bs.boostName, now).isDefined)
}
