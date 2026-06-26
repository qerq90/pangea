package pangea.service.state.states

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.schedule.Scheduler
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/**
 * Городская стройка. На входе — меню из трёх работ разной длительности; после
 * выбора игрок «занят» (как [[events.GoldVeinState]]): scene_data хранит вид
 * работы и старт, поллер по таймеру (`Construction`) выдаёт золото и
 * возвращает в GlobalMap. Кнопка «Уйти» — досрочное прерывание с
 * подтверждением.
 *
 * Формула золота: `lvl(героя) × часы`, домноженное на `100 ± d` процентов,
 * где `d ∈ [10, 20]` (знак и величина роллятся отдельно).
 */
case class ConstructionState(heroDao: HeroDao, scheduler: Scheduler, content: SceneContent) extends State {
  import ConstructionState._

  private val branch = new Branch(
    routes = Map(
      "CarryStone"       -> Target.Run { (user, _, renderer) => startJob(user, renderer, Job.CarryStone) },
      "BreakWall"        -> Target.Run { (user, _, renderer) => startJob(user, renderer, Job.BreakWall) },
      "BuildWall"        -> Target.Run { (user, _, renderer) => startJob(user, renderer, Job.BuildWall) },
      "LeaveWork"        -> Target.Run { (user, _, renderer) => leaveWork(user, renderer) },
      "ConfirmLeaveWork" -> Target.Run { (user, _, renderer) => confirmLeave(user, renderer) },
      "CancelLeaveWork"  -> Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.Construction) },
      "Finish"           -> Target.Run { (user, _, renderer) => finish(user, renderer) },
      "LeaveConstruction" -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (user, _, renderer) => enter(user, renderer).as(StateType.Construction) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  // Вход: активная работа в scene_data → экран работы, иначе — меню выбора.
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    activeJob(user).flatMap {
      case Some(_) => showWork(user, renderer)
      case None    => showMenu(user, renderer)
    }

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def showMenu(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, content.screen("construction.menu"))

  private def showWork(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(
      content.text("construction.work.text"),
      content.screen("construction.work").choices))

  private def startJob(user: User, renderer: Renderer, job: Job): Task[StateType] =
    activeJob(user).flatMap {
      // Защита от перезапуска: если работа уже идёт, просто возвращаем её экран.
      case Some(_) => showWork(user, renderer).as(StateType.Construction)
      case None =>
        for {
          now <- nowMs
          _   <- heroDao.writeSceneData(user.userId, Json.obj(
                   KindKey      -> job.entryName.asJson,
                   StartedAtKey -> now.asJson))
          _   <- scheduler.schedule(user.userId, now + job.durationMs, TaskKind.Construction, StateType.Construction, FinishAction)
          _   <- showWork(user, renderer)
        } yield StateType.Construction
    }

  // Ручной выход: до конца — confirm с остатком времени; после — сам завершаем.
  private def leaveWork(user: User, renderer: Renderer): Task[StateType] =
    for {
      now <- nowMs
      job <- activeJob(user)
      result <- job match {
        case None => enter(user, renderer).as(StateType.Construction)
        case Some((j, start)) if now - start >= j.durationMs => finish(user, renderer)
        case Some((j, start)) =>
          val remaining = formatRemaining(j.durationMs - (now - start))
          renderer.show(user, Screen(
            content.format("construction.confirmLeave.text", "remaining" -> remaining),
            content.screen("construction.confirmLeave").choices)).as(StateType.Construction)
      }
    } yield result

  // Досрочный выход: золота нет, снимаем задачу, чистим scene_data.
  private def confirmLeave(user: User, renderer: Renderer): Task[StateType] =
    scheduler.cancel(user.userId, TaskKind.Construction) *>
      heroDao.writeSceneData(user.userId, Json.Null) *>
      renderer.show(user, Screen(content.text("construction.left"), Nil)).as(StateType.GlobalMap)

  // Завершение работы — выдача золота и финальный текст. Возврат в GlobalMap.
  private def finish(user: User, renderer: Renderer): Task[StateType] =
    activeJob(user).flatMap {
      case None => ZIO.succeed(StateType.GlobalMap)
      case Some((job, _)) =>
        for {
          hero    <- getHero(user)
          base     = hero.lvl.toLong * job.hours.toLong
          delta   <- Random.nextIntBetween(MinSpreadPct, MaxSpreadPct + 1)
          sign    <- Random.nextBoolean.map(if (_) 1 else -1)
          reward   = (base * (100L + sign * delta.toLong)) / 100L
          _       <- heroDao.updateGold(user.userId, hero.gold + reward)
          _       <- scheduler.cancel(user.userId, TaskKind.Construction)
          _       <- heroDao.writeSceneData(user.userId, Json.Null)
          _       <- renderer.show(user, Screen(
                       content.format("construction.done", "gold" -> reward.toString), Nil))
        } yield StateType.GlobalMap
    }

  private def activeJob(user: User): Task[Option[(Job, Long)]] =
    heroDao.readSceneData(user.userId).map { data =>
      for {
        json  <- data
        kind  <- json.hcursor.get[String](KindKey).toOption
        job   <- Job.fromName(kind)
        start <- json.hcursor.get[Long](StartedAtKey).toOption
      } yield (job, start)
    }

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def formatRemaining(ms: Long): String = {
    val secs = (ms / 1000L).max(0L)
    val h    = secs / 3600
    val m    = (secs % 3600) / 60
    if (h > 0) s"${h}ч ${m}мин"
    else if (m > 0) s"${m}мин"
    else s"${secs}с"
  }
}

object ConstructionState {
  import enumeratum._

  sealed abstract class Job(val hours: Int) extends EnumEntry {
    val durationMs: Long = hours.toLong * 3600L * 1000L
  }
  object Job extends Enum[Job] {
    val values = findValues
    case object CarryStone extends Job(1)
    case object BreakWall  extends Job(4)
    case object BuildWall  extends Job(8)
    def fromName(name: String): Option[Job] = withNameOption(name)
  }

  val MinSpreadPct: Int = 10
  val MaxSpreadPct: Int = 20

  private val KindKey       = "constructionKind"
  private val StartedAtKey  = "constructionStartedAt"
  private val FinishAction  = """{"action":"Finish"}"""
}
