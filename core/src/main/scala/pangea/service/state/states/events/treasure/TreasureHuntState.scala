package pangea.service.state.states.events.treasure

import io.circe.Json
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.TreasureMapGenerator
import pangea.generator.loot.TreasureHuntGenerator
import pangea.model.hero.{Hero, Knowledge}
import pangea.model.item.MapZone
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.schedule.Scheduler
import pangea.service.state.states.LootState.LootData
import pangea.service.state.{CharacterMenu, HerbLore, State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/**
 * Поход за сокровищем по карте клада — [[TreasureHuntState.HuntDurationMs]], 7 часов.
 * Отправка происходит из [[pangea.service.state.states.OutskirtsState]]: она
 * расходует карту, пишет [[TreasureHuntProgress]] в `scene_data` и планирует
 * задачу `TreasureHunt`. Здесь — экран ожидания; поллер по таймеру доводит до
 * добычи и уводит на общий экран [[StateType.Loot]] (возврат — в город). Добыча —
 * чистое ядро [[TreasureHuntGenerator]] по зоне израсходованной карты.
 *
 * В пути у героя две кнопки. «Персонаж» — меню персонажа, назад оно вернёт в поход.
 * «Уйти» — с подтверждением: герой поворачивает назад и возвращается в город,
 * сокровища нет, а карта снова в сумке — у карты нет ничего, кроме зоны, и она
 * собирается заново ([[TreasureMapGenerator.full]]). Влезет в сумку — хорошо, нет —
 * её приходится оставить, как и добычу, которой не нашлось места. Пока герой был
 * в меню, таймер мог выйти и пропустить ход: тогда любая кнопка сперва доводит
 * поход до клада.
 */
case class TreasureHuntState(
  heroDao:       HeroDao,
  scheduler:     Scheduler,
  content:       SceneContent,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository
) extends State {
  import TreasureHuntState._

  private val branch = new Branch(
    routes = Map(
      "HuntDone"      -> Target.Run { (user, _, renderer) => huntDone(user, renderer) },
      "OpenCharacter" -> Target.Run { (user, _, renderer) =>
        arrivedOr(user, renderer)(CharacterMenu.open(heroDao, user.userId, StateType.TreasureHunt)) },
      "HuntLeave"     -> Target.Run { (user, _, renderer) => arrivedOr(user, renderer)(askLeave(user, renderer)) },
      "HuntLeaveYes"  -> Target.Run { (user, _, renderer) => arrivedOr(user, renderer)(turnBack(user, renderer)) },
      "HuntLeaveNo"   -> Target.Run { (user, _, renderer) => onTick(user, renderer) }
    ),
    fallback = Target.Run { (user, _, renderer) => onTick(user, renderer) }
  )

  override def targetStates: Set[StateType] = Set(StateType.Loot, StateType.TreasureHunt, StateType.HeroStats, StateType.GlobalMap)

  /** Путь: сколько осталось и кнопки. Время вышло, пока герой сидел в меню
    * персонажа (таймер тогда промолчал), — он на месте, и осталось копать. */
  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now       <- nowMs
      started   <- startedAt(user)
      remaining  = started.map(s => (HuntDurationMs - (now - s)).max(0L)).getOrElse(HuntDurationMs)
      _         <- if (started.isDefined && remaining <= 0L)
                     renderer.show(user, Screen(content.text("treasureHunt.arrived"), List(
                       Choice("HuntDone", content.text("treasureHunt.dig"), color = ChoiceColor.Positive, row = Some(0)))))
                   else renderer.show(user, Screen(
                     content.format("treasureHunt.enter.text", "duration" -> formatRemaining(remaining)), roadButtons))
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
          renderer.show(user, Screen(content.format("treasureHunt.wait.text", "remaining" -> remaining), roadButtons))
            .as(StateType.TreasureHunt)
        case None => huntDone(user, renderer)
      }
    } yield result

  /** Дошёл ли герой: время вышло (или прогресса нет) — сперва клад, иначе `f`. */
  private def arrivedOr(user: User, renderer: Renderer)(f: => Task[StateType]): Task[StateType] =
    for {
      now     <- nowMs
      started <- startedAt(user)
      res     <- if (started.forall(s => now - s >= HuntDurationMs)) huntDone(user, renderer) else f
    } yield res

  /** «Уйти»: сколько ещё идти и что будет, если повернуть. */
  private def askLeave(user: User, renderer: Renderer): Task[StateType] =
    for {
      now     <- nowMs
      started <- startedAt(user)
      left     = started.fold(0L)(s => (HuntDurationMs - (now - s)).max(0L))
      _       <- renderer.show(user, Screen(content.format("treasureHunt.confirmLeave.text", "remaining" -> formatRemaining(left)),
                   content.screen("treasureHunt.confirmLeave").choices))
    } yield StateType.TreasureHunt

  /** Повернуть назад: таймер снят, сокровища нет, карта той же зоны — снова в
    * сумке, если влезла. Герой — в городе. */
  private def turnBack(user: User, renderer: Renderer): Task[StateType] =
    for {
      progress <- heroDao.readSceneData(user.userId).map(_.flatMap(_.as[TreasureHuntProgress].toOption))
      _        <- scheduler.cancel(user.userId, TaskKind.TreasureHunt)
      hero     <- getHero(user)
      kept     <- ZIO.foreach(progress.map(p => TreasureMapGenerator.full(p.zone)))(map =>
                    itemRepo.persist(hero.id, map).flatMap(saved =>
                      inventoryRepo.addItem(hero.id, saved).fold(_ => false, _ => true)))
      _        <- heroDao.writeSceneData(user.userId, Json.Null)
      _        <- renderer.show(user, Screen(content.text(
                    if (kept.contains(false)) "treasureHunt.turnedBackNoRoom" else "treasureHunt.turnedBack"), Nil))
    } yield StateType.GlobalMap

  // Развязка похода: гарантированная добыча по уровню карты → общий экран Loot.
  private def huntDone(user: User, renderer: Renderer): Task[StateType] =
    for {
      progress <- heroDao.readSceneData(user.userId).map(_.flatMap(_.as[TreasureHuntProgress].toOption))
      zone      = progress.map(_.zone).getOrElse(MapZone.values.head)
      _        <- scheduler.cancel(user.userId, TaskKind.TreasureHunt)
      seed     <- Random.nextLong
      // Редкие травы в кладе находит только тот, кто знает цветы 2 ранга.
      lore     <- HerbLore.readLore(heroDao, user.userId)
      (reward, _) = TreasureHuntGenerator.roll(zone, Rng(seed), knowsRareHerbs = lore.knows(Knowledge.FlowersRank2))
      loot      = LootData(
                    items       = reward.items ++ reward.gems ++ reward.materials,
                    silvers     = if (reward.silver > 0L) List(reward.silver) else Nil,
                    doubloons   = reward.doubloons,
                    returnState = Some(StateType.GlobalMap))
      _        <- renderer.show(user, Screen(content.text("treasureHunt.success"), Nil))
      _        <- heroDao.writeSceneData(user.userId, loot.asJson)
    } yield StateType.Loot

  /** Кнопки в пути: меню персонажа и «Уйти». */
  private def roadButtons: List[Choice] = List(
    content.choice("OpenCharacter", "common.character").copy(row = Some(0)),
    Choice("HuntLeave", content.text("treasureHunt.leave"), color = ChoiceColor.Negative, row = Some(0))
  )

  private def startedAt(user: User): Task[Option[Long]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[TreasureHuntProgress].toOption.map(_.startedAt)))

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object TreasureHuntState {
  val HuntDurationMs: Long   = 7L * 60L * 60L * 1000L
  val HuntDoneAction: String = """{"action":"HuntDone"}"""

  /** Сколько ещё идти: «6 ч 59 мин», «12 мин 30 с», «45 с». */
  def formatRemaining(ms: Long): String = {
    val secs = (ms / 1000L).max(0L)
    val h    = secs / 3600L
    val m    = (secs % 3600L) / 60L
    val s    = secs % 60L
    if (h > 0) s"$h ч $m мин" else if (m > 0) s"$m мин $s с" else s"$s с"
  }
}
