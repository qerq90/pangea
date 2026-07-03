package pangea.service.state.states.events.treasure

import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.generator.loot.TreasureHuntGenerator
import pangea.model.item.MapZone
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.schedule.Scheduler
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/**
 * Поход за сокровищем по карте клада (таймер ~10 минут). Отправка происходит из
 * [[pangea.service.state.states.OutskirtsState]]: она расходует карту, пишет
 * [[TreasureHuntProgress]] в `scene_data` и планирует задачу `TreasureHunt`.
 * Здесь показываем экран ожидания; поллер по таймеру доводит до добычи и уводит
 * на общий экран [[StateType.Loot]] (возврат — в город). Добыча — чистое ядро
 * [[TreasureHuntGenerator]] по уровню израсходованной карты.
 */
case class TreasureHuntState(heroDao: HeroDao, scheduler: Scheduler, content: SceneContent) extends State {
  import TreasureHuntState._

  private val branch = new Branch(
    routes = Map(
      "HuntDone" -> Target.Run { (user, _, renderer) => huntDone(user, renderer) }
    ),
    fallback = Target.Run { (user, _, renderer) => onTick(user, renderer) }
  )

  override def targetStates: Set[StateType] = Set(StateType.Loot, StateType.TreasureHunt)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now       <- nowMs
      started   <- startedAt(user)
      remaining  = started.map(s => (HuntDurationMs - (now - s)).max(0L)).getOrElse(HuntDurationMs)
      _         <- renderer.show(user, Screen(
                     content.format("treasureHunt.enter.text", "duration" -> formatRemaining(remaining)), Nil))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // Любой ввод во время похода: время вышло — доводим до добычи, иначе показываем остаток.
  private def onTick(user: User, renderer: Renderer): Task[StateType] =
    for {
      now     <- nowMs
      started <- startedAt(user)
      result <- started match {
        case Some(s) if now - s >= HuntDurationMs => huntDone(user, renderer)
        case Some(s) =>
          val remaining = formatRemaining(HuntDurationMs - (now - s))
          renderer.show(user, Screen(content.format("treasureHunt.wait.text", "remaining" -> remaining), Nil))
            .as(StateType.TreasureHunt)
        case None => huntDone(user, renderer)
      }
    } yield result

  // Развязка похода: гарантированная добыча по уровню карты → общий экран Loot.
  private def huntDone(user: User, renderer: Renderer): Task[StateType] =
    for {
      progress <- heroDao.readSceneData(user.userId).map(_.flatMap(_.as[TreasureHuntProgress].toOption))
      zone      = progress.map(_.zone).getOrElse(MapZone.values.head)
      _        <- scheduler.cancel(user.userId, TaskKind.TreasureHunt)
      seed     <- Random.nextLong
      (reward, _) = TreasureHuntGenerator.roll(zone, Rng(seed))
      loot      = LootData(
                    items       = reward.items,
                    golds       = if (reward.gold > 0L) List(reward.gold) else Nil,
                    doubloons   = reward.doubloons,
                    returnState = Some(StateType.GlobalMap))
      _        <- renderer.show(user, Screen(content.text("treasureHunt.success"), Nil))
      _        <- heroDao.writeSceneData(user.userId, loot.asJson)
    } yield StateType.Loot

  private def startedAt(user: User): Task[Option[Long]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[TreasureHuntProgress].toOption.map(_.startedAt)))

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def formatRemaining(ms: Long): String = {
    val secs = (ms / 1000L).max(0L)
    val m    = secs / 60
    val s    = secs % 60
    if (m > 0) s"${m}мин ${s}с" else s"${s}с"
  }
}

object TreasureHuntState {
  val HuntDurationMs: Long   = 10L * 60L * 1000L
  val HuntDoneAction: String = """{"action":"HuntDone"}"""
}
