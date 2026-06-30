package pangea.service.state.states.events.treasure

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.generator.loot.SchronGenerator
import pangea.model.hero.Hero
import pangea.model.monster.Race
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.schedule.Scheduler
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/**
 * Событие «прикопанный схрон» (5%). Игрок начинает раскопки (~10 минут): state
 * хранит `startedAt` в `scene_data`, поллер по таймеру (`SchronDig`) сам доводит
 * до развязки и возвращает в игру. Кнопка «Уйти» — с подтверждением (как добыча
 * руды). По завершении: 80% — схрон (1–2 дублона), 20% — «свежая могила».
 */
case class TreasureDigState(heroDao: HeroDao, scheduler: Scheduler, content: SceneContent) extends State {
  import TreasureDigState._

  private val branch = new Branch(
    routes = Map(
      "LeaveDig"        -> Target.Run { (user, _, renderer) => leaveDig(user, renderer) },
      "ConfirmLeaveDig" -> Target.Run { (user, _, renderer) => confirmLeave(user, renderer) },
      "CancelLeaveDig"  -> Target.Run { (user, _, renderer) => showDig(user, renderer).as(StateType.TreasureDig) },
      "DigDone"         -> Target.Run { (user, _, renderer) => digDone(user, renderer) },
      "GraveLeave"      -> Target.Goto(StateType.Dungeon)
    ),
    fallback = Target.Run { (user, _, renderer) => showDig(user, renderer).as(StateType.TreasureDig) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.Dungeon + StateType.Loot

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now <- nowMs
      _   <- heroDao.writeSceneData(user.userId, TreasureDigProgress(now).asJson)
      _   <- scheduler.schedule(user.userId, now + DigDurationMs, TaskKind.SchronDig, StateType.TreasureDig, DigAction)
      _   <- showDig(user, renderer)
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def showDig(user: User, renderer: Renderer): Task[Unit] =
    for {
      now     <- nowMs
      started <- startedAt(user)
      remaining = started.map(s => (DigDurationMs - (now - s)).max(0L)).getOrElse(DigDurationMs)
      _       <- renderer.show(user, Screen(
                   content.format("treasureDig.enter.text", "duration" -> formatRemaining(remaining)),
                   content.screen("treasureDig.enter").choices))
    } yield ()

  // Ручная попытка уйти: до конца раскопок — confirm с остатком; если поллер ещё
  // не успел сработать после fireAt — доводим развязку сами.
  private def leaveDig(user: User, renderer: Renderer): Task[StateType] =
    for {
      now     <- nowMs
      started <- startedAt(user)
      result <- started match {
        case None => digDone(user, renderer)
        case Some(start) if now - start >= DigDurationMs => digDone(user, renderer)
        case Some(start) =>
          val remaining = formatRemaining(DigDurationMs - (now - start))
          renderer.show(user, Screen(
            content.format("treasureDig.confirmLeave.text", "remaining" -> remaining),
            content.screen("treasureDig.confirmLeave").choices)).as(StateType.TreasureDig)
      }
    } yield result

  // Досрочный выход: добычи нет, снимаем отложенный SchronDig, чистим scene_data.
  private def confirmLeave(user: User, renderer: Renderer): Task[StateType] =
    scheduler.cancel(user.userId, TaskKind.SchronDig) *>
      heroDao.writeSceneData(user.userId, Json.Null) *>
      renderer.show(user, Screen(content.text("treasureDig.left"), Nil)).as(StateType.Dungeon)

  // Развязка раскопок: 80% — схрон, 20% — «свежая могила».
  private def digDone(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      _    <- scheduler.cancel(user.userId, TaskKind.SchronDig)
      roll <- Random.nextIntBetween(1, 101)
      result <- if (roll <= 80) digSuccess(user, hero, renderer)
                else            digGrave(user, renderer)
    } yield result

  private def digSuccess(user: User, hero: Hero, renderer: Renderer): Task[StateType] =
    for {
      raceIdx <- Random.nextIntBounded(Race.values.size)
      race     = Race.values(raceIdx)
      seed    <- Random.nextLong
      (reward, _) = SchronGenerator.roll(race, hero.dungeonLevel.toLong, DoubloonMin, DoubloonMax, Rng(seed))
      loot     = LootData(
                   items     = reward.items,
                   golds     = if (reward.gold > 0L) List(reward.gold) else Nil,
                   doubloons = reward.doubloons)
      _ <- renderer.show(user, Screen(content.text("treasureDig.success"), Nil))
      _ <- heroDao.writeSceneData(user.userId, loot.asJson)
    } yield StateType.Loot

  private def digGrave(user: User, renderer: Renderer): Task[StateType] =
    for {
      raceIdx <- Random.nextIntBounded(Race.values.size)
      race     = Race.values(raceIdx)
      _       <- heroDao.writeSceneData(user.userId, Json.Null)
      _       <- renderer.show(user, Screen(
                   content.format("treasureDig.grave.text", "race" -> race.toString),
                   content.screen("treasureDig.grave").choices))
    } yield StateType.TreasureDig

  private def startedAt(user: User): Task[Option[Long]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[TreasureDigProgress].toOption.map(_.startedAt)))

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
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

object TreasureDigState {
  val DigDurationMs: Long = 10L * 60L * 1000L
  val DoubloonMin: Int    = 1
  val DoubloonMax: Int    = 2

  private val DigAction = """{"action":"DigDone"}"""
}
