package pangea.service.state.states

import io.circe.{Json, jawn}
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Choice, Renderer, SceneContent, Screen}
import pangea.model.hero.AzatState
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
      // post-death recovery: DeathState writes restDurationMs and postDeath=true
      // before transitioning here.
      duration      = existingData.flatMap(_.hcursor.get[Long]("restDurationMs").toOption)
                        .getOrElse(DefaultRestMs)
      postDeath     = existingData.flatMap(_.hcursor.get[Boolean]("postDeath").toOption)
                        .getOrElse(false)
      _            <- heroDao.writeSceneData(user.userId, Json.obj(
                        "restStartedAt"  -> now.asJson,
                        "restDurationMs" -> duration.asJson,
                        "postDeath"      -> postDeath.asJson))
      // push-пробуждение: поллер по таймеру сам выполнит wakeUp. Перепланирование
      // снимает прежний Revive.
      _            <- scheduler.schedule(user.userId, now + duration, TaskKind.Revive, StateType.Rest, ReviveAction)
      // post-death recovery → отдельный «обморочный» текст; обычный отдых — у костра.
      // hideKeyboard = true: скрываем прежние кнопки (бой/подземелье), пока герой
      // в отключке. Если есть мгновенные отдыхи (благословение) и это не обморок —
      // показываем кнопку мгновенного отдыха (тогда клавиатуру не прячем).
      azat          <- loadAzat(user)
      instantOffer   = !postDeath && azat.instantRests > 0
      enterKey       = if (postDeath) "rest.enterRevive" else "rest.enter.text"
      choices        = if (instantOffer)
                         List(Choice("InstantRest", content.format("rest.instantLabel", "n" -> azat.instantRests.toString)))
                       else content.screen("rest.enter").choices
      _            <- renderer.show(user, Screen(
                        content.format(enterKey, "duration" -> formatDuration(duration / 1000L)),
                        choices,
                        hideKeyboard = !instantOffer))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      now       <- ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))
      sceneData <- heroDao.readSceneData(user.userId)
      startedAt  = sceneData.flatMap(_.hcursor.get[Long]("restStartedAt").toOption).getOrElse(now)
      duration   = sceneData.flatMap(_.hcursor.get[Long]("restDurationMs").toOption).getOrElse(DefaultRestMs)
      postDeath  = sceneData.flatMap(_.hcursor.get[Boolean]("postDeath").toOption).getOrElse(false)
      elapsed    = now - startedAt
      isInstant  = parseAction(ua.payload).contains("InstantRest")
      result    <- if (isInstant && !postDeath) instantRest(user, now, renderer)
                   else if (elapsed >= duration)
                     heroDao.getHeroByUserId(user.userId)
                       .flatMap(ZIO.fromOption(_))
                       .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
                       .flatMap(hero => wakeUp(user, now, hero, postDeath, renderer))
                   else {
                     val remainSec = ((duration - elapsed) / 1000L).max(1L)
                     renderer.show(user, Screen(
                       content.format("rest.waiting", "remaining" -> formatDuration(remainSec)), Nil))
                       .as(StateType.Rest)
                   }
    } yield result

  // Мгновенный отдых (благословение Азата): восстанавливает ресурсы как обычный
  // отдых, но сразу, тратя один заряд из azat.instantRests.
  private def instantRest(user: User, nowMs: Long, renderer: Renderer): Task[StateType] =
    for {
      azat <- loadAzat(user)
      res <- if (azat.instantRests <= 0)
               renderer.show(user, Screen(content.text("rest.noInstant"), Nil)).as(StateType.Rest)
             else
               heroDao.getHeroByUserId(user.userId)
                 .flatMap(ZIO.fromOption(_))
                 .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
                 .flatMap { hero =>
                   val maxHp    = hero.effectiveMaxHp(nowMs)
                   val maxArmor = hero.effectiveMaxArmor(nowMs)
                   val maxEn    = hero.maxEnergy(nowMs)
                   heroDao.updateFightStats(user.userId, hero.fightStats.copy(hp = maxHp, armor = maxArmor, energy = maxEn)) *>
                     saveAzat(user, azat.copy(instantRests = azat.instantRests - 1)) *>
                     scheduler.cancel(user.userId, TaskKind.Revive) *>
                     heroDao.writeSceneData(user.userId, Json.Null) *>
                     renderer.show(user, Screen(content.format("rest.instantDone", "n" -> (azat.instantRests - 1).toString), Nil))
                       .as(StateType.Dungeon)
                 }
    } yield res

  private def loadAzat(user: User): Task[AzatState] =
    heroDao.readAzatData(user.userId).map(_.flatMap(_.as[AzatState].toOption).getOrElse(AzatState.empty))

  private def saveAzat(user: User, azat: AzatState): Task[Unit] =
    heroDao.writeAzatData(user.userId, azat.asJson)

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def wakeUp(user: User, nowMs: Long, hero: pangea.model.hero.Hero, postDeath: Boolean, renderer: Renderer): Task[StateType] = {
    val maxHp    = hero.effectiveMaxHp(nowMs)
    val maxArmor = hero.effectiveMaxArmor(nowMs)
    val maxEn    = hero.maxEnergy(nowMs)
    val textKey  = if (postDeath) "rest.revived" else "rest.done"
    for {
      _ <- heroDao.updateFightStats(user.userId, hero.fightStats.copy(hp = maxHp, armor = maxArmor, energy = maxEn))
      _ <- heroDao.writeSceneData(user.userId, Json.Null)
      _ <- scheduler.cancel(user.userId, TaskKind.Revive)
      _ <- renderer.show(user, Screen(content.text(textKey), Nil))
    } yield StateType.Dungeon
  }

  private def formatDuration(seconds: Long): String = {
    val m = seconds / 60
    val s = seconds % 60
    if (m > 0) s"${m}м ${s}с" else s"${s}с"
  }
}
