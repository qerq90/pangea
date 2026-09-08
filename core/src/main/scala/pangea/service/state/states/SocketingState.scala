package pangea.service.state.states

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, jawn}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, Renderer, SceneContent, Screen, Target}
import pangea.model.battle.Element
import pangea.model.hero.{Equipment, Hero}
import pangea.model.item.{Gem, Item, ItemType}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.states.SocketingState._
import pangea.service.state.{ItemMenu, State, UserAction}
import zio.{Task, ZIO}

/** Экран вставки камня-усилителя в гнездо. Вход из инвентаря: id выбранного камня
 *  кладётся в scene_data ([[Scene]]). Показываем надетое снаряжение со свободным
 *  гнездом; выбор предмета вставляет камень в его первое свободное гнездо, тратит
 *  камень из инвентаря и возвращает в инвентарь. Извлечение камней пока не
 *  поддерживается (отдельный тикет).
 *
 *  Список постраничный, как в инвентаре: гнёзда бывают в любом из 14 надетых
 *  предметов, а клавиатура ВК держит только 10 рядов — без разбивки экран просто
 *  не отправлялся бы. Текущая страница живёт в той же сцене. */
case class SocketingState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "BackFromSocketing" -> Target.Goto(StateType.Inventory),
      "SocketPrev"        -> Target.Run { (u, _, r) => navigate(u, r, -1) },
      "SocketNext"        -> Target.Run { (u, _, r) => navigate(u, r, +1) }
    ),
    fallback = Target.Run { (user, ua, renderer) => handleFallback(user, ua, renderer) }
  )

  override def targetStates: Set[StateType] = Set(StateType.Inventory)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    showTargets(user, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // Список надетого снаряжения со свободным гнездом (кнопками), страницами.
  private def showTargets(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      ctx  <- gemContext(user, hero)
      _ <- ctx match {
        case None => renderer.show(user, Screen(content.text("socketing.gemGone"),
                       List(content.choice("BackFromSocketing", "socketing.back"))))
        case Some((scene, g)) =>
          val targets = socketableItems(hero.equipment)
          if (targets.isEmpty)
            renderer.show(user, Screen(content.text("socketing.noTargets"),
              List(content.choice("BackFromSocketing", "socketing.back"))))
          else {
            val (pageItems, totalPages, page) = ItemMenu.page(targets, scene.page.getOrElse(0))
            val base   = content.format("socketing.choose", "gem" -> g.displayName)
            val header = if (totalPages > 1) s"$base (${page + 1}/$totalPages)" else base
            val btns   = ItemMenu.itemButtons(pageItems, TargetPrefix)
            renderer.show(user, Screen(header, btns ++ navRow(page, totalPages)))
          }
      }
    } yield StateType.Socketing

  /** Нав-ряд последним рядом: «Назад» всегда, стрелки — только когда есть куда
    * листать (на первой странице нет «Пред.», на последней — «След.»). */
  private def navRow(page: Int, totalPages: Int): List[Choice] = {
    val row = ItemMenu.NavRow
    List(
      Some(content.choice("BackFromSocketing", "socketing.back").copy(row = Some(row))),
      Option.when(page > 0)(Choice("SocketPrev", content.text("common.prev"), row = Some(row))),
      Option.when(page < totalPages - 1)(Choice("SocketNext", content.text("common.next"), row = Some(row)))
    ).flatten
  }

  /** Листание: страница нормализуется по текущему числу целей, так что «След.»
    * с последней страницы никуда не уедет. */
  private def navigate(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    for {
      hero  <- getHero(user)
      scene <- readScene(user)
      _ <- scene match {
        case None => ZIO.unit
        case Some(s) =>
          val (_, totalPages, _) = ItemMenu.page(socketableItems(hero.equipment), 0)
          val np = (s.page.getOrElse(0) + delta).max(0).min(totalPages - 1)
          heroDao.writeSceneData(user.userId, s.copy(page = Some(np)).asJson)
      }
      res <- showTargets(user, renderer)
    } yield res

  // Fallback: кнопки вида SocketTarget_<itemId>.
  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(TargetPrefix) =>
        a.drop(TargetPrefix.length).toLongOption match {
          case Some(id) => insert(user, id, renderer)
          case None     => showTargets(user, renderer)
        }
      case _ => showTargets(user, renderer)
    }

  private def insert(user: User, targetId: Long, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      ctx  <- gemContext(user, hero)
      res <- ctx match {
        case None => showTargets(user, renderer)
        case Some((scene, g)) =>
          val target = hero.equipment.allItems.find(i => i.id == targetId && i.itemType != ItemType.NoItem)
          target match {
            // Огонь и Холод в одном оружии не уживаются: вставляемый камень и
            // ПРОТИВОПОЛОЖНЫЙ ему обращаются в пыль. Одинаковые стихии при этом
            // спокойно стакаются — два рубина усиливают друг друга по грейдам.
            case Some(item) if isWeapon(item) && hasOpposite(item, g) =>
              val cleaned = removeOpposite(item, g)
              heroDao.updateEquipmentAndFightStats(user.userId, withUpdatedItem(hero.equipment, cleaned), hero.fightStats) *>
                inventoryRepo.removeItem(scene.gemId, hero.id).mapError(e => new Throwable(e.toString)) *>
                renderer.show(user, Screen(content.text("socketing.annihilate"), Nil)) *>
                heroDao.writeSceneData(user.userId, io.circe.Json.Null).as(StateType.Inventory)
            case _ =>
              socketInto(hero.equipment, targetId, g) match {
                case None => showTargets(user, renderer)
                case Some((newEq, item)) =>
                  heroDao.updateEquipmentAndFightStats(user.userId, newEq, hero.fightStats) *>
                    inventoryRepo.removeItem(scene.gemId, hero.id).mapError(e => new Throwable(e.toString)) *>
                    renderer.show(user, Screen(
                      content.format("socketing.done", "gem" -> g.displayName, "item" -> item.name), Nil)) *>
                    heroDao.writeSceneData(user.userId, io.circe.Json.Null).as(StateType.Inventory)
              }
          }
      }
    } yield res

  private def isWeapon(item: Item): Boolean =
    item.itemType == ItemType.Weapon || item.itemType == ItemType.AdditionalWeapon

  /** Противоположная стихия: огонь ↔ холод. У остальных стихий пары нет. */
  private def opposite(e: Element): Option[Element] = e match {
    case Element.Fire => Some(Element.Cold)
    case Element.Cold => Some(Element.Fire)
    case _            => None
  }

  private def isOpposite(gem: Gem, to: Gem): Boolean =
    (Element.of(gem.kind), Element.of(to.kind).flatMap(opposite)) match {
      case (Some(a), Some(b)) => a == b
      case _                  => false
    }

  private def hasOpposite(item: Item, gem: Gem): Boolean =
    item.socketedGems.exists(isOpposite(_, gem))

  // Гасит первый камень противоположной стихии (гнездо становится свободным).
  private def removeOpposite(item: Item, gem: Gem): Item =
    item.sockets.indexWhere(_.exists(isOpposite(_, gem))) match {
      case -1  => item
      case idx => item.copy(sockets = item.sockets.updated(idx, None))
    }

  // Надетые предметы со свободным гнездом (кроме пустых слотов).
  private def socketableItems(eq: Equipment): List[Item] =
    eq.allItems.filter(i => i.itemType != ItemType.NoItem && i.hasFreeSocket)

  // Вставка камня в предмет `targetId`: возвращает обновлённое снаряжение и предмет.
  private def socketInto(eq: Equipment, targetId: Long, gem: Gem): Option[(Equipment, Item)] =
    eq.allItems.find(i => i.id == targetId && i.itemType != ItemType.NoItem && i.hasFreeSocket).map { item =>
      val updated = item.socketGem(gem)
      (withUpdatedItem(eq, updated), updated)
    }

  // Замена предмета в его слоте (кольца различаем по id).
  private def withUpdatedItem(eq: Equipment, item: Item): Equipment = item.itemType match {
    case ItemType.Helmet           => eq.copy(helmet = item)
    case ItemType.ShoulderPads     => eq.copy(shoulderPads = item)
    case ItemType.ChestPlate       => eq.copy(chestPlate = item)
    case ItemType.Bracelets        => eq.copy(bracelets = item)
    case ItemType.Gloves           => eq.copy(gloves = item)
    case ItemType.Pants            => eq.copy(pants = item)
    case ItemType.Leggings         => eq.copy(pants = item)
    case ItemType.Boots            => eq.copy(boots = item)
    case ItemType.Amulet           => eq.copy(amulet = item)
    case ItemType.Ring             =>
      if (eq.firstRing.id == item.id) eq.copy(firstRing = item) else eq.copy(secondRing = item)
    case ItemType.Belt             => eq.copy(belt = item)
    case ItemType.Flask            => eq.copy(flask = item)
    case ItemType.Weapon           => eq.copy(weapon = item)
    case ItemType.AdditionalWeapon => eq.copy(additionalWeapon = item)
    case ItemType.Trophy           => eq
    case ItemType.TreasureMap      => eq
    case ItemType.TreasureMapHalf  => eq
    case ItemType.Gem              => eq
    case ItemType.Material         => eq
    case ItemType.NoItem           => eq
  }

  /** Сцена экрана и сам камень, если он ещё в инвентаре. */
  private def gemContext(user: User, hero: Hero): Task[Option[(Scene, Gem)]] =
    for {
      scene <- readScene(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
    } yield scene.flatMap { s =>
      inv.items.data.find(_.id == s.gemId).flatMap(_.gem).map(g => (s, g))
    }

  private def readScene(user: User): Task[Option[Scene]] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[Scene].toOption))

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object SocketingState {
  val TargetPrefix = "SocketTarget_"

  /** scene_data экрана: id камня-предмета, выбранного в инвентаре, и текущая
   *  страница списка целей (None — первая). */
  final case class Scene(gemId: Long, page: Option[Int] = None)
  object Scene {
    implicit val encoder: Encoder[Scene] = deriveEncoder
    implicit val decoder: Decoder[Scene] = deriveDecoder
  }
}
