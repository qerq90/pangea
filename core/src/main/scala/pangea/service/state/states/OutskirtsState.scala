package pangea.service.state.states

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, jawn}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
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
      "CancelDepart"    -> Target.Run { (u, _, r) => showMapList(u, r) }
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] =
    branch.gotoTargets ++ Set(StateType.TreasureHunt, StateType.Outskirts)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, content.screen("outskirts.enter")).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // Список целых карт клада в сумке (половинки в поход не годятся).
  private def showMapList(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      maps  = inv.items.data.filter(_.itemType == ItemType.TreasureMap)
      _ <- if (maps.isEmpty)
             renderer.show(user, Screen(content.text("outskirts.noMaps"),
               List(content.choice("BackToOutskirts", "outskirts.back"))))
           else {
             val btns = ItemMenu.itemButtons(maps, MapPickPrefix)
             val back = content.choice("BackToOutskirts", "outskirts.back").copy(row = Some(btns.size))
             renderer.show(user, Screen(content.text("outskirts.chooseMap"), btns :+ back))
           }
    } yield StateType.Outskirts

  // Выбор карты: запоминаем её id и показываем подтверждение похода.
  private def selectMap(user: User, itemId: Long, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      res <- inv.items.data.find(i => i.id == itemId && i.itemType == ItemType.TreasureMap) match {
        case None => showMapList(user, renderer)
        case Some(_) =>
          heroDao.writeSceneData(user.userId, OutskirtsScene(itemId).asJson) *>
            renderer.show(user, content.screen("outskirts.confirm")).as(StateType.Outskirts)
      }
    } yield res

  // Подтверждение: расходуем карту, ставим таймер и уходим в поход.
  private def confirmDepart(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      chosen = scene.flatMap(s => inv.items.data.find(i => i.id == s.mapId && i.itemType == ItemType.TreasureMap))
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

  case class OutskirtsScene(mapId: Long)
  object OutskirtsScene {
    implicit val encoder: Encoder[OutskirtsScene] = deriveEncoder[OutskirtsScene]
    implicit val decoder: Decoder[OutskirtsScene] = deriveDecoder[OutskirtsScene]
  }
}
