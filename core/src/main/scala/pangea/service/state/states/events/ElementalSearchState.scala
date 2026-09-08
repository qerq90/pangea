package pangea.service.state.states.events

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.GemGenerator
import pangea.model.hero.Hero
import pangea.model.item.{Gem, GemKind, Item}
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.schedule.Scheduler
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/**
 * Осмотр логова после победы над элементалем. Герой ищет «афк»: поллер сам
 * присылает находку каждые 2–4 минуты, пока не кончатся попытки. Сколько ждать
 * до следующего камня, игрок не видит НИКОГДА — в этом и смысл: осматриваться
 * можно спокойно, не сидя в чате.
 *
 * Уйти можно в любой момент, недобранные попытки просто пропадают.
 */
case class ElementalSearchState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  scheduler:     Scheduler,
  content:       SceneContent
) extends State {
  import ElementalSearchState._

  private val branch = new Branch(
    routes = Map(
      "LeaveSearch"    -> Target.Run { (u, _, r) => leave(u, r) },
      "ElementalFind"  -> Target.Run { (u, _, r) => findOne(u, r) }
    ),
    fallback = Target.Run { (u, _, r) => showSearch(u, r).as(StateType.ElementalSearch) }
  )

  override def targetStates: Set[StateType] = Set(StateType.Dungeon, StateType.ElementalSearch)

  /** Вход: считаем попытки по уровню босса и ставим первый таймер. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      scene <- readScene(user)
      _ <- scene match {
        // Повторный вход (напр. после перезахода) — счётчик уже есть, не сбрасываем.
        case Some(_) => showSearch(user, renderer)
        case None =>
          for {
            hero  <- getHero(user)
            bonus <- Random.nextLongBetween(1L, 3L) // 1 или 2 сверх BossLvL
            tries  = bonus + pangea.model.monster.Elemental.bossLvl(hero.lvl)
            _     <- writeScene(user, SearchScene(tries.toInt))
            _     <- scheduleNext(user)
            _     <- showSearch(user, renderer)
          } yield ()
      }
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def showSearch(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, content.screen("elementalSearch.enter"))

  /** Срабатывание таймера: выдаём камень и, если попытки остались, ставим следующий. */
  private def findOne(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      scene <- readScene(user)
      res <- scene match {
        case None => ZIO.succeed(StateType.Dungeon)
        case Some(s) =>
          for {
            gem       <- randomGem
            persisted <- itemRepo.persist(hero.id, gem)
            // Сумка переполнена — камень просто теряется, как и прочая добыча.
            _         <- inventoryRepo.addItem(hero.id, persisted).ignore
            _         <- renderer.show(user, Screen(
                           content.format("elementalSearch.found", "gem" -> gem.displayTitle), Nil))
            left       = s.triesLeft - 1
            out <- if (left <= 0)
                     heroDao.writeSceneData(user.userId, Json.Null) *>
                       renderer.show(user, Screen(content.text("elementalSearch.done"), Nil))
                         .as(StateType.Dungeon)
                   else
                     writeScene(user, SearchScene(left)) *> scheduleNext(user) *>
                       showSearch(user, renderer).as(StateType.ElementalSearch)
          } yield out
      }
    } yield res

  private def leave(user: User, renderer: Renderer): Task[StateType] =
    scheduler.cancel(user.userId, TaskKind.ElementalSearch) *>
      heroDao.writeSceneData(user.userId, Json.Null) *>
      renderer.show(user, Screen(content.text("elementalSearch.done"), Nil)).as(StateType.Dungeon)

  /** Следующая находка через 2–4 минуты. */
  private def scheduleNext(user: User): Task[Unit] =
    for {
      now   <- ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))
      delta <- Random.nextLongBetween(MinDelayMs, MaxDelayMs + 1L)
      _     <- scheduler.schedule(user.userId, now + delta, TaskKind.ElementalSearch,
                 StateType.ElementalSearch, FindAction)
    } yield ()

  /** Случайный камень: вид любой, кроме черепа; грейд по качеству 90/9/1. */
  private def randomGem: Task[Item] =
    for {
      kindIdx <- Random.nextIntBounded(DroppableKinds.size)
      roll    <- Random.nextIntBetween(1, 101)
      grade    = if (roll <= CrackedPct) Gem.MinGrade
                 else if (roll <= CrackedPct + DamagedPct) Gem.MinGrade + 1
                 else WholeGrade
    } yield GemGenerator.item(DroppableKinds(kindIdx), grade)

  private def readScene(user: User): Task[Option[SearchScene]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[SearchScene].toOption))

  private def writeScene(user: User, scene: SearchScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object ElementalSearchState {

  /** Сколько ещё находок осталось. Время до следующей НЕ храним и не показываем. */
  final case class SearchScene(triesLeft: Int)
  object SearchScene {
    implicit val encoder: Encoder[SearchScene] = deriveEncoder
    implicit val decoder: Decoder[SearchScene] = deriveDecoder
  }

  val MinDelayMs: Long = 2L * 60L * 1000L
  val MaxDelayMs: Long = 4L * 60L * 1000L

  /** Качество находки: «надколотый» почти всегда, «цельный» — большая редкость. */
  val CrackedPct: Int = 90
  val DamagedPct: Int = 9
  /** Грейд «цельного» камня — базовое имя без приставки (напр. просто «Рубин»). */
  val WholeGrade: Int = 3

  /** Виды камней, которые можно найти в логове: череп исключён. */
  val DroppableKinds: IndexedSeq[GemKind] = GemKind.values.filterNot(_ == GemKind.Skull)

  private val FindAction = """{"action":"ElementalFind"}"""
}
