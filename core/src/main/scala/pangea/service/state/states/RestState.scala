package pangea.service.state.states

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Renderer, SceneContent, Screen}
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.schedule.Scheduler
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

case class RestState(heroDao: HeroDao, scheduler: Scheduler, content: SceneContent) extends State {

  private val DefaultRestMs = 30_000L  // regular campfire rest

  // payload синтетического действия пробуждения (RestState.action игнорирует его
  // содержимое — пробуждение решает таймер, не ключ маршрута).
  private val ReviveAction = """{"action":"Revive"}"""

  override def targetStates: Set[StateType] = Set(StateType.Dungeon)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now          <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
      existingData <- heroDao.readSceneData(user.userId)
      // post-death recovery: DeathState writes restDurationMs before transitioning here
      duration      = existingData.flatMap(_.hcursor.get[Long]("restDurationMs").toOption)
                        .getOrElse(DefaultRestMs)
      _            <- heroDao.writeSceneData(user.userId, Json.obj(
                        "restStartedAt"  -> now.asJson,
                        "restDurationMs" -> duration.asJson))
      // push-пробуждение: поллер по таймеру сам выполнит wakeUp (кнопка отдыха
      // остаётся ручным fallback'ом). Перепланирование снимает прежний Revive.
      _            <- scheduler.schedule(user.userId, now + duration, TaskKind.Revive, StateType.Rest, ReviveAction)
      _            <- renderer.show(user, Screen(
                        content.format("rest.enter.text", "duration" -> formatDuration(duration / 1000L)),
                        content.screen("rest.enter").choices))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      now       <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
      sceneData <- heroDao.readSceneData(user.userId)
      startedAt  = sceneData.flatMap(_.hcursor.get[Long]("restStartedAt").toOption).getOrElse(now)
      duration   = sceneData.flatMap(_.hcursor.get[Long]("restDurationMs").toOption).getOrElse(DefaultRestMs)
      elapsed    = now - startedAt
      result    <- if (elapsed >= duration)
                     heroDao.getHeroByUserId(user.userId)
                       .flatMap(ZIO.fromOption(_))
                       .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
                       .flatMap(hero => wakeUp(user, now, hero, renderer))
                   else {
                     val remainSec = ((duration - elapsed) / 1000L).max(1L)
                     renderer.show(user, Screen(
                       content.format("rest.waiting", "remaining" -> formatDuration(remainSec)), Nil))
                       .as(StateType.Rest)
                   }
    } yield result

  private def wakeUp(user: User, nowMs: Long, hero: pangea.model.hero.Hero, renderer: Renderer): Task[StateType] = {
    val maxHp    = hero.effectiveMaxHp(nowMs)
    val maxArmor = hero.effectiveMaxArmor(nowMs)
    for {
      _ <- heroDao.updateFightStats(user.userId, hero.fightStats.copy(hp = maxHp, armor = maxArmor))
      _ <- heroDao.writeSceneData(user.userId, Json.Null)
      // снимаем отложенное пробуждение — оно уже отработано (ручным ли поком,
      // самим ли поллером)
      _ <- scheduler.cancel(user.userId, TaskKind.Revive)
      _ <- renderer.show(user, Screen(content.text("rest.done"), Nil))
    } yield StateType.Dungeon
  }

  private def formatDuration(seconds: Long): String = {
    val m = seconds / 60
    val s = seconds % 60
    if (m > 0) s"${m}м ${s}с" else s"${s}с"
  }
}
