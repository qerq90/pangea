package pangea.service.state.states.road

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.cave.{CaveGenerator, SewerRates}
import pangea.model.monster.Race
import pangea.model.quest.BoardKind
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.schedule.Scheduler
import pangea.service.state.states.events.thieves.{ThievesScene, ThievesState}
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/** Дорога к месту выездного задания: куда герой идёт, с какого часа и какого
  * уровня само задание. Живёт в `scene_data`, поэтому кодеки рукописные — по
  * прибытии колонку занимает уже сцена канализации. */
final case class RoadProgress(startedAt: Long, kind: BoardKind, lvl: Long)

object RoadProgress {
  implicit val encoder: Encoder[RoadProgress] = (p: RoadProgress) => Json.obj(
    "startedAt" -> p.startedAt.asJson, "kind" -> p.kind.asJson, "lvl" -> p.lvl.asJson)

  implicit val decoder: Decoder[RoadProgress] = (c: HCursor) =>
    for {
      startedAt <- c.get[Long]("startedAt")
      kind      <- c.get[BoardKind]("kind")
      lvl       <- c.getOrElse[Long]("lvl")(0L)
    } yield RoadProgress(startedAt, kind, lvl)
}

/** Экран дороги — общий для всех выездных заданий с доски гильдии.
  *
  * Взяв такое объявление, герой уходит из города сам: кнопок на дороге нет,
  * идти ему [[SewerRates.RoadMs]], и всё, что он может, — ждать. По таймеру
  * поллер доводит его до места: канализация катается здесь же, уровнем в
  * уровень задания, и дальше это обычная пещера ([[StateType.MonsterCave]]).
  *
  * Дорога одна на все выездные задания: новому виду достаточно своей ветки в
  * [[QuestRoadState.arrive]].
  */
case class QuestRoadState(heroDao: HeroDao, scheduler: Scheduler, content: SceneContent) extends State {

  private val branch = new Branch(
    routes   = Map("RoadDone" -> Target.Run { (u, _, r) => arrive(u, r) }),
    fallback = Target.Run { (u, _, r) => onTick(u, r) }
  )

  override def targetStates: Set[StateType] =
    Set(StateType.MonsterCave, StateType.Thieves, StateType.QuestRoad, StateType.GlobalMap)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- nowMs
      road <- readRoad(user)
      left  = road.map(r => (SewerRates.RoadMs - (now - r.startedAt)).max(0L)).getOrElse(SewerRates.RoadMs)
      _    <- renderer.show(user, Screen(
                content.format("questRoad.enter", "remaining" -> remaining(left)), Nil, hideKeyboard = true))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  /** Любое слово в пути: время вышло — приходим, нет — говорим, сколько ещё. */
  private def onTick(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      road <- readRoad(user)
      res <- road match {
        case Some(r) if now - r.startedAt >= SewerRates.RoadMs => arrive(user, renderer)
        case Some(r) =>
          renderer.show(user, Screen(content.format("questRoad.wait",
            "remaining" -> remaining(SewerRates.RoadMs - (now - r.startedAt))), Nil, hideKeyboard = true))
            .as(StateType.QuestRoad)
        case None => lost(user, renderer)
      }
    } yield res

  /** Пришли. Место катается здесь, на месте, — до прихода его ещё нет. */
  private def arrive(user: User, renderer: Renderer): Task[StateType] =
    for {
      road <- readRoad(user)
      _    <- scheduler.cancel(user.userId, TaskKind.QuestRoad)
      res <- road match {
        case Some(r) if r.kind == BoardKind.SewerRats =>
          for {
            seed      <- Random.nextLong
            (scene, _) = CaveGenerator.sewer(r.lvl.max(SewerRates.MinLvl), Rng(seed))
            _         <- heroDao.writeSceneData(user.userId, scene.asJson)
            _         <- renderer.show(user, Screen(content.text("questRoad.arrived"), Nil))
          } yield StateType.MonsterCave

        // Подворотня у таверны: кто выйдет и сколько их, решается здесь — до
        // ночи этого не знает никто.
        case Some(r) if r.kind == BoardKind.Thieves =>
          for {
            raceIdx <- Random.nextIntBounded(Race.mortals.size)
            count   <- Random.nextIntBetween(ThievesState.MinThieves, ThievesState.MaxThieves + 1)
            scene    = ThievesScene(Race.mortals(raceIdx).entryName, r.lvl.max(SewerRates.MinLvl), count)
            _       <- heroDao.writeSceneData(user.userId, scene.asJson)
            _       <- renderer.show(user, Screen(content.text("thieves.alley"), Nil))
          } yield StateType.Thieves

        case _ => lost(user, renderer)
      }
    } yield res

  /** Дороги в колонке нет (её затёрли или задание не из выездных) — не бросать
    * же героя в чистом поле: возвращаем в город. */
  private def lost(user: User, renderer: Renderer): Task[StateType] =
    scheduler.cancel(user.userId, TaskKind.QuestRoad) *>
      heroDao.writeSceneData(user.userId, Json.Null) *>
      renderer.show(user, Screen(content.text("questRoad.lost"), Nil)).as(StateType.GlobalMap)

  private def readRoad(user: User): Task[Option[RoadProgress]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[RoadProgress].toOption))

  private def remaining(ms: Long): String = {
    val secs = (ms / 1000L).max(0L)
    val m    = secs / 60L
    val s    = secs % 60L
    if (m > 0L) s"${m}мин ${s}с" else s"${s}с"
  }

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
}

object QuestRoadState {
  /** payload синтетического действия, которым поллер приводит героя на место. */
  val DoneAction: String = """{"action":"RoadDone"}"""
}
