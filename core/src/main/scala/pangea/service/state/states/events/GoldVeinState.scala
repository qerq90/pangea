package pangea.service.state.states.events

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.schedule.Scheduler
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/**
 * Событие «Золотая жила». Игрок входит и сразу начинает добычу: state хранит
 * `goldVeinStartedAt` в `scene_data`, поллер по таймеру (`Harvest`) сам выдаёт
 * золото и возвращает в Dungeon. Ручная кнопка «Уйти» работает как fallback и
 * для досрочного прерывания (с подтверждением).
 *
 * Формула золота: `(dungeonLevel + 5) × 4`, домноженное на 100 ± d процентов,
 * где `d ∈ [10, 20]` — направление и величина разброса роллятся отдельно.
 */
case class GoldVeinState(heroDao: HeroDao, scheduler: Scheduler, content: SceneContent) extends State {
  import GoldVeinState._

  private val branch = new Branch(
    routes = Map(
      "LeaveVein"        -> Target.Run { (user, _, renderer) => leaveVein(user, renderer) },
      "ConfirmLeaveVein" -> Target.Run { (user, _, renderer) => confirmLeave(user, renderer) },
      "CancelLeaveVein"  -> Target.Run { (user, _, renderer) => showVein(user, renderer).as(StateType.GoldVein) },
      "Harvest"          -> Target.Run { (user, _, renderer) => harvest(user, renderer) }
    ),
    fallback = Target.Run { (user, _, renderer) => showVein(user, renderer).as(StateType.GoldVein) }
  )

  override def targetStates: Set[StateType] = Set(StateType.Dungeon)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now <- nowMs
      _   <- heroDao.writeSceneData(user.userId, Json.obj(StartedAtKey -> now.asJson))
      _   <- scheduler.schedule(user.userId, now + HarvestDurationMs, TaskKind.Harvest, StateType.GoldVein, HarvestAction)
      _   <- showVein(user, renderer)
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def showVein(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(
      content.format("goldVein.enter.text", "duration" -> formatRemaining(HarvestDurationMs)),
      content.screen("goldVein.enter").choices))

  // Ручная попытка уйти: до конца добычи показываем confirm c остатком времени;
  // если поллер ещё не успел сработать после `fireAt` — сами выдадим золото.
  private def leaveVein(user: User, renderer: Renderer): Task[StateType] =
    for {
      now     <- nowMs
      started <- startedAt(user)
      result <- started match {
        case None => harvest(user, renderer)
        case Some(start) if now - start >= HarvestDurationMs => harvest(user, renderer)
        case Some(start) =>
          val remaining = formatRemaining(HarvestDurationMs - (now - start))
          renderer.show(user, Screen(
            content.format("goldVein.confirmLeave.text", "remaining" -> remaining),
            content.screen("goldVein.confirmLeave").choices)).as(StateType.GoldVein)
      }
    } yield result

  // Досрочный выход: золота нет, снимаем отложенный Harvest, чистим scene_data.
  private def confirmLeave(user: User, renderer: Renderer): Task[StateType] =
    scheduler.cancel(user.userId, TaskKind.Harvest) *>
      heroDao.writeSceneData(user.userId, Json.Null) *>
      renderer.show(user, Screen(content.text("goldVein.left"), Nil)).as(StateType.Dungeon)

  // Завершение добычи (по таймеру от поллера или вручную после fireAt).
  private def harvest(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero    <- getHero(user)
      base     = (hero.dungeonLevel.toLong + 5L) * 4L
      delta   <- Random.nextIntBetween(MinSpreadPct, MaxSpreadPct + 1)
      sign    <- Random.nextBoolean.map(if (_) 1 else -1)
      reward   = (base * (100L + sign * delta.toLong)) / 100L
      _       <- heroDao.updateGold(user.userId, hero.gold + reward)
      _       <- scheduler.cancel(user.userId, TaskKind.Harvest)
      _       <- heroDao.writeSceneData(user.userId, Json.Null)
      _       <- renderer.show(user, Screen(
                   content.format("goldVein.done", "gold" -> reward.toString), Nil))
    } yield StateType.Dungeon

  private def startedAt(user: User): Task[Option[Long]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.hcursor.get[Long](StartedAtKey).toOption))

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[pangea.model.hero.Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def formatRemaining(ms: Long): String = {
    val secs = (ms / 1000L).max(0L)
    val m    = secs / 60
    val s    = secs % 60
    if (m > 0) s"${m}мин ${s}с" else s"${s}с"
  }
}

object GoldVeinState {
  val HarvestDurationMs: Long = 15L * 60L * 1000L
  val MinSpreadPct: Int       = 10
  val MaxSpreadPct: Int       = 20

  private val StartedAtKey  = "goldVeinStartedAt"
  private val HarvestAction = """{"action":"Harvest"}"""
}
