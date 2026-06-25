package pangea.service.state.states.tavern

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.monster.Race
import pangea.model.quest.QuestData
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.states.tavern.QuestBoardState._
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/**
 * Доска заданий таверны. У каждого игрока свой пул из `QuestSlots` заданий «принести
 * трофей расы X», обновляемый раз в `QuestRefreshMs`. Доска показывает по одному
 * заданию; «Взять» списывает слот и роллит следующее. Активным может быть только одно
 * задание — новое заменяет предыдущее. Состояние durable (`heroes.quest_data`).
 */
case class QuestBoardState(heroDao: HeroDao, content: SceneContent) extends State {

  private val branch = new Branch(
    routes = Map(
      "TakeQuest"     -> Target.Run { (user, _, renderer) => takeQuest(user, renderer) },
      "AbandonQuest"  -> Target.Run { (user, _, renderer) => abandonQuest(user, renderer) },
      "BackFromQuest" -> Target.Goto(StateType.Tavern)
    ),
    fallback = Target.Run { (user, _, renderer) => showBoard(user, renderer).as(StateType.QuestBoard) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] = showBoard(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // Показ доски: при истёкшем таймере перекатываем пул (активное задание сохраняется).
  // Пустая доска показывает обратный отсчёт до новых заданий.
  private def showBoard(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- nowMs
      data <- loadQuests(user, now)
      _    <- renderer.show(user, boardScreen(data, now))
    } yield ()

  private def boardScreen(data: QuestData, now: Long): Screen = {
    val activeLine = data.active
      .map(r => content.format("quest.activeLine", "race" -> Race.withName(r).toString))
      .getOrElse("")
    data.current.filter(_ => data.remaining > 0) match {
      case Some(raceName) =>
        val text = content.format("quest.board",
          "race"      -> Race.withName(raceName).toString,
          "remaining" -> data.remaining.toString,
          "active"    -> activeLine)
        Screen(text, List(
          content.choice("TakeQuest",     "quest.takeLabel"),
          content.choice("AbandonQuest",  "quest.abandonLabel"),
          content.choice("BackFromQuest", "quest.backLabel")))
      case None =>
        val text = content.format("quest.empty",
          "remaining" -> formatRemaining(data.refreshAt - now),
          "active"    -> activeLine)
        val buttons =
          (if (data.active.isDefined) List(content.choice("AbandonQuest", "quest.abandonLabel")) else Nil) :+
            content.choice("BackFromQuest", "quest.backLabel")
        Screen(text, buttons)
    }
  }

  // Взять квест: списываем слот, роллим следующее задание, делаем взятое активным.
  // Активным может быть только одно задание — иначе предупреждаем о замене.
  private def takeQuest(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      data <- loadQuests(user, now)
      _ <- data.current.filter(_ => data.remaining > 0) match {
             case Some(raceName) =>
               for {
                 seed         <- Random.nextLong
                 remaining     = data.remaining - 1
                 (nextRace, _) = Rng(seed).pick(Race.values.toList)
                 newCurrent    = if (remaining > 0) Some(nextRace.entryName) else None
                 newData       = data.copy(remaining = remaining, current = newCurrent, active = Some(raceName))
                 _ <- heroDao.writeQuestData(user.userId, newData.asJson)
                 _ <- ZIO.when(data.active.isDefined)(
                        renderer.show(user, Screen(content.text("quest.onlyOneWarning"), Nil)))
                 _ <- renderer.show(user, Screen(
                        content.format("quest.taken", "race" -> Race.withName(raceName).toString), Nil))
                 _ <- renderer.show(user, boardScreen(newData, now))
               } yield ()
             case None => showBoard(user, renderer)
           }
    } yield StateType.QuestBoard

  // Отказаться: сбрасываем активное задание (если оно есть).
  private def abandonQuest(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      data <- loadQuests(user, now)
      _ <- data.active match {
             case Some(_) =>
               val newData = data.copy(active = None)
               heroDao.writeQuestData(user.userId, newData.asJson) *>
                 renderer.show(user, Screen(content.text("quest.abandoned"), Nil)) *>
                 renderer.show(user, boardScreen(newData, now))
             case None =>
               renderer.show(user, Screen(content.text("quest.noActiveToAbandon"), Nil)) *>
                 renderer.show(user, boardScreen(data, now))
           }
    } yield StateType.QuestBoard

  private def loadQuests(user: User, now: Long): Task[QuestData] =
    heroDao.readQuestData(user.userId).flatMap {
      case Some(json) =>
        json.as[QuestData].toOption match {
          case Some(d) if now < d.refreshAt => ZIO.succeed(d)
          case Some(d)                      => regenerate(user, now, d.active)
          case None                         => regenerate(user, now, None)
        }
      case None => regenerate(user, now, None)
    }

  // Перекат пула: QuestSlots свежих заданий, новый таймер. Активное задание игрока
  // (уже взятое) переживает обновление.
  private def regenerate(user: User, now: Long, active: Option[String]): Task[QuestData] =
    for {
      seed     <- Random.nextLong
      (race, _) = Rng(seed).pick(Race.values.toList)
      data      = QuestData(QuestSlots, Some(race.entryName), now + QuestRefreshMs, active)
      _        <- heroDao.writeQuestData(user.userId, data.asJson)
    } yield data

  private def formatRemaining(ms: Long): String = {
    val secs = (ms / 1000L).max(0L)
    val h    = secs / 3600
    val m    = (secs % 3600) / 60
    if (h > 0) s"${h}ч ${m}мин"
    else if (m > 0) s"${m}мин"
    else s"${secs}с"
  }

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
}

object QuestBoardState {
  val QuestSlots: Int        = 3
  val QuestRefreshMs: Long   = 20L * 60L * 60L * 1000L // 20 часов
}
