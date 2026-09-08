package pangea.service.state.states

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, jawn}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.{ItemDetails, ItemType}
import pangea.model.schedule.TaskKind
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.schedule.Scheduler
import pangea.service.state.states.OutskirtsState._
import pangea.service.state.states.events.treasure.{TreasureHuntProgress, TreasureHuntState}
import pangea.service.state.{ItemMenu, State, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/**
 * «За городом» — точка отправки в поход за сокровищем. Показывает две кнопки
 * (зелёная «Отправиться за сокровищем» и красная «Назад» в город). Отправка
 * ведёт к выбору карты из сумки (только целые карты), затем к подтверждению
 * (бета: возврат через ~10 минут). Подтверждение расходует карту и переводит в
 * [[TreasureHuntState]] (таймер). Уровень карты определяет добычу похода.
 *
 * Список карт постраничный, как инвентарь: карт в сумке бывает сколько угодно, а
 * клавиатура ВК держит только 10 рядов — без разбивки экран просто не отправлялся
 * бы. Страница и выбранная карта живут в одной сцене ([[OutskirtsScene]]).
 */
case class OutskirtsState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  scheduler:     Scheduler,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "BackToCity"      -> Target.Goto(StateType.GlobalMap),
      "DepartTreasure"  -> Target.Run { (u, _, r) => showMapList(u, r) },
      "BackToOutskirts" -> Target.Run { (u, _, r) => enter(u, r).as(StateType.Outskirts) },
      "ConfirmDepart"   -> Target.Run { (u, _, r) => confirmDepart(u, r) },
      "CancelDepart"    -> Target.Run { (u, _, r) => showMapList(u, r) },
      "HuntPrev"        -> Target.Run { (u, _, r) => navigate(u, r, -1) },
      "HuntNext"        -> Target.Run { (u, _, r) => navigate(u, r, +1) }
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.TreasureHunt, StateType.Outskirts)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, content.screen("outskirts.enter")).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // Список целых карт клада в сумке (половинки в поход не годятся), страницами.
  private def showMapList(user: User, renderer: Renderer): Task[StateType] =
    for {
      maps  <- treasureMaps(user)
      scene <- readScene(user)
      _ <- if (maps.isEmpty)
             renderer.show(user, Screen(content.text("outskirts.noMaps"),
               List(content.choice("BackToOutskirts", "outskirts.back"))))
           else {
             val (pageMaps, totalPages, page) = ItemMenu.page(maps, scene.flatMap(_.page).getOrElse(0))
             val base   = content.text("outskirts.chooseMap")
             val header = if (totalPages > 1) s"$base (${page + 1}/$totalPages)" else base
             val btns   = ItemMenu.itemButtons(pageMaps, MapPickPrefix)
             renderer.show(user, Screen(header, btns ++ navRow(page, totalPages)))
           }
    } yield StateType.Outskirts

  /** Нав-ряд последним рядом: «Назад» всегда, стрелки — только когда есть куда
    * листать (на первой странице нет «Пред.», на последней — «След.»). */
  private def navRow(page: Int, totalPages: Int): List[Choice] = {
    val row = ItemMenu.NavRow
    List(
      Some(content.choice("BackToOutskirts", "outskirts.back").copy(row = Some(row))),
      Option.when(page > 0)(Choice("HuntPrev", content.text("common.prev"), row = Some(row))),
      Option.when(page < totalPages - 1)(Choice("HuntNext", content.text("common.next"), row = Some(row)))
    ).flatten
  }

  /** Листание: страница нормализуется по текущему числу карт, так что «След.» с
    * последней страницы никуда не уедет. Выбранная карта в сцене сохраняется. */
  private def navigate(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    for {
      maps  <- treasureMaps(user)
      scene <- readScene(user)
      cur    = scene.getOrElse(OutskirtsScene())
      (_, totalPages, _) = ItemMenu.page(maps, 0)
      np     = (cur.page.getOrElse(0) + delta).max(0).min(totalPages - 1)
      _     <- heroDao.writeSceneData(user.userId, cur.copy(page = Some(np)).asJson)
      res   <- showMapList(user, renderer)
    } yield res

  /** Целые карты клада из сумки — половинки в поход не годятся. */
  private def treasureMaps(user: User): Task[List[pangea.model.item.Item]] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
    } yield inv.items.data.filter(_.itemType == ItemType.TreasureMap)

  // Выбор карты: запоминаем её id и показываем подтверждение похода.
  private def selectMap(user: User, itemId: Long, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      scene <- readScene(user)
      res <- inv.items.data.find(i => i.id == itemId && i.itemType == ItemType.TreasureMap) match {
        case None => showMapList(user, renderer)
        case Some(_) =>
          // Страницу не теряем: «Уйти» с подтверждения вернёт на неё же.
          val kept = scene.getOrElse(OutskirtsScene()).copy(mapId = Some(itemId))
          heroDao.writeSceneData(user.userId, kept.asJson) *>
            renderer.show(user, content.screen("outskirts.confirm")).as(StateType.Outskirts)
      }
    } yield res

  // Подтверждение: расходуем карту, ставим таймер и уходим в поход.
  private def confirmDepart(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      chosen = scene.flatMap(_.mapId).flatMap(id => inv.items.data.find(i => i.id == id && i.itemType == ItemType.TreasureMap))
      chosenZone = chosen.flatMap(_.details match {
        case ItemDetails.TreasureMap(zone) => Some(zone)
        case _                             => None
      })
      res <- (chosen, chosenZone) match {
        case (Some(map), Some(zone)) =>
          for {
            now <- nowMs
            _   <- inventoryRepo.removeItem(map.id, hero.id).mapError(e => new Throwable(e.toString))
            _   <- heroDao.writeSceneData(user.userId, TreasureHuntProgress(now, zone).asJson)
            _   <- scheduler.schedule(user.userId, now + TreasureHuntState.HuntDurationMs,
                     TaskKind.TreasureHunt, StateType.TreasureHunt, TreasureHuntState.HuntDoneAction)
          } yield StateType.TreasureHunt
        case _ => showMapList(user, renderer) // карты уже нет — назад к списку
      }
    } yield res

  // Динамический id вида HuntPick_<id> — выбор карты из списка.
  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(MapPickPrefix) =>
        a.drop(MapPickPrefix.length).toLongOption.fold(showMapList(user, renderer))(selectMap(user, _, renderer))
      case _ => enter(user, renderer).as(StateType.Outskirts)
    }

  private def readScene(user: User): Task[Option[OutskirtsScene]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[OutskirtsScene].toOption))

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object OutskirtsState {
  val MapPickPrefix = "HuntPick_"

  /** scene_data экрана: выбранная карта (None — ещё не выбрана) и текущая
   *  страница списка карт (None — первая). */
  case class OutskirtsScene(mapId: Option[Long] = None, page: Option[Int] = None)
  object OutskirtsScene {
    implicit val encoder: Encoder[OutskirtsScene] = deriveEncoder[OutskirtsScene]
    implicit val decoder: Decoder[OutskirtsScene] = deriveDecoder[OutskirtsScene]
  }
}
