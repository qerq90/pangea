package pangea.service.state.states

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, jawn}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.{Equipment, Hero}
import pangea.model.item.Item
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.states.EquipmentState._
import pangea.service.state.{InventoryFeedback, ItemMenu, State, UserAction}
import zio.{Task, ZIO}

case class EquipmentState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "EquipmentList" -> Target.Run { (u, _, r) => writeScene(u, EquipmentScene(page = Some(0))) *> showList(u, r).as(StateType.Equipment) },
      "EquipmentPrev" -> Target.Run { (u, _, r) => navigate(u, r, -1) },
      "EquipmentNext" -> Target.Run { (u, _, r) => navigate(u, r, +1) },
      "Unequip"       -> Target.Run { (u, _, r) => unequipSelected(u, r) },
      "BackFromEquip" -> Target.Goto(StateType.HeroStats)
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = Set(StateType.HeroStats, StateType.Equipment)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    writeScene(user, EquipmentScene(page = Some(0))) *> showList(user, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Список слотов ──────────────────────────────────────────────────────────

  private def showList(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      scene <- readScene(user)
      // одна кнопка = один слот. Делаем индексы слотов сразу с предметами, чтобы
      // потом легко мапнуть нажатие на slot index.
      indexed = slots.zipWithIndex
      (pageSlots, totalPages, page) = ItemMenu.page(indexed, scene.page.getOrElse(0))
      header = s"🎽 ${content.text("equipment.header")}" + (if (totalPages > 1) s" (${page + 1}/$totalPages)" else "")
      slotBtns = pageSlots.zipWithIndex.map { case ((slot, slotIdx), rowIdx) =>
                   val item  = slot.get(hero.equipment)
                   val label = ItemMenu.truncate(slotLabel(slot, item))
                   Choice(s"$SlotPrefix$slotIdx", label, row = Some(rowIdx))
                 }
      nav = navRow(page, totalPages, "EquipmentPrev", "EquipmentNext", "BackFromEquip")
      _   <- renderer.show(user, Screen(header, slotBtns ++ nav))
    } yield StateType.Equipment

  private def navigate(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    for {
      scene  <- readScene(user)
      (_, totalPages, _) = ItemMenu.page(slots.zipWithIndex, 0)
      cur    = scene.page.getOrElse(0)
      np     = (cur + delta).max(0).min(totalPages - 1)
      _      <- writeScene(user, scene.copy(page = Some(np)))
      res    <- showList(user, renderer)
    } yield res

  // ── Детальный экран слота ──────────────────────────────────────────────────

  private def showSlot(user: User, slotIdx: Int, renderer: Renderer): Task[StateType] =
    if (slotIdx < 0 || slotIdx >= slots.size) showList(user, renderer)
    else for {
      hero  <- getHero(user)
      scene <- readScene(user)
      slot   = slots(slotIdx)
      item   = slot.get(hero.equipment)
      isEmpty = item.itemType == pangea.model.item.ItemType.NoItem
      itemLine = if (isEmpty) content.text("equipment.empty") else EquipmentState.itemStats(item)
      text   = s"🎽 ${slot.name}\n$itemLine"
      choices = List(
        Option.when(!isEmpty)(content.choice("Unequip", "equipment.unequipBtn").copy(row = Some(0))),
        Some(content.choice("EquipmentList", "equipment.back").copy(row = Some(1)))
      ).flatten
      _ <- writeScene(user, scene.copy(selectedSlot = Some(slotIdx)))
      _ <- renderer.show(user, Screen(text, choices))
    } yield StateType.Equipment

  // ── Снятие выбранного ──────────────────────────────────────────────────────

  private def unequipSelected(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      res <- scene.selectedSlot match {
        case None => showList(user, renderer)
        case Some(idx) => doUnequip(user, idx, renderer)
      }
    } yield res

  private def doUnequip(user: User, slotIdx: Int, renderer: Renderer): Task[StateType] =
    if (slotIdx < 0 || slotIdx >= slots.size) showList(user, renderer)
    else for {
      hero <- getHero(user)
      slot  = slots(slotIdx)
      item  = slot.get(hero.equipment)
      _ <- if (item.itemType == pangea.model.item.ItemType.NoItem)
             renderer.show(user, Screen(content.text("equipment.slotEmpty"), Nil))
           else
             inventoryRepo.addItem(hero.id, item).foldZIO(
               _ => renderer.show(user, Screen(content.text("common.inventoryFull"), Nil)),
               _ => {
                 val newEq    = slot.clear(hero.equipment)
                 val newFight = InventoryState.applyDelta(hero.fightStats, Item.NoItem, item)
                 heroDao.updateEquipmentAndFightStats(user.userId, newEq, newFight) *>
                   InventoryFeedback.freeSlotsLine(inventoryRepo, content, hero.id).flatMap(slotsLine =>
                     renderer.show(user, Screen(
                       content.format("equipment.unequipped", "name" -> item.name) + "\n" + slotsLine, Nil)))
               }
             )
      res <- showList(user, renderer)
    } yield res

  // ── Fallback: динамические id EquipSlot_<idx> ──────────────────────────────

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(SlotPrefix) =>
        a.drop(SlotPrefix.length).toIntOption.fold(showList(user, renderer))(showSlot(user, _, renderer))
      case _ => showList(user, renderer)
    }

  // ── Хелперы ─────────────────────────────────────────────────────────────────

  private def slotLabel(slot: Slot, item: Item): String =
    if (item.itemType == pangea.model.item.ItemType.NoItem) s"${slot.name}: —"
    else s"${slot.name}: ${item.name} Ур.${item.lvl}"

  private def navRow(page: Int, totalPages: Int, prevId: String, nextId: String, backId: String): List[Choice] = {
    val row = ItemMenu.NavRow
    List(
      Some(content.choice(backId, "equipment.back").copy(row = Some(row))),
      Option.when(page > 0)(Choice(prevId, content.text("common.prev"), row = Some(row))),
      Option.when(page < totalPages - 1)(Choice(nextId, content.text("common.next"), row = Some(row)))
    ).flatten
  }

  private def readScene(user: User): Task[EquipmentScene] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[EquipmentScene].toOption).getOrElse(EquipmentScene()))

  private def writeScene(user: User, scene: EquipmentScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object EquipmentState {

  val SlotPrefix = "EquipSlot_"

  case class EquipmentScene(page: Option[Int] = None, selectedSlot: Option[Int] = None)
  object EquipmentScene {
    implicit val encoder: Encoder[EquipmentScene] = deriveEncoder
    implicit val decoder: Decoder[EquipmentScene] = deriveDecoder
  }

  case class Slot(name: String, get: Equipment => Item, clear: Equipment => Equipment)

  val slots: List[Slot] = List(
    Slot("Шлем",           _.helmet,           _.copy(helmet           = Item.NoItem)),
    Slot("Наплечники",     _.shoulderPads,     _.copy(shoulderPads     = Item.NoItem)),
    Slot("Нагрудник",      _.chestPlate,       _.copy(chestPlate       = Item.NoItem)),
    Slot("Браслеты",       _.bracelets,        _.copy(bracelets        = Item.NoItem)),
    Slot("Перчатки",       _.gloves,           _.copy(gloves           = Item.NoItem)),
    Slot("Штаны",          _.pants,            _.copy(pants            = Item.NoItem)),
    Slot("Сапоги",         _.boots,            _.copy(boots            = Item.NoItem)),
    Slot("Амулет",         _.amulet,           _.copy(amulet           = Item.NoItem)),
    Slot("Кольцо 1",       _.firstRing,        _.copy(firstRing        = Item.NoItem)),
    Slot("Кольцо 2",       _.secondRing,       _.copy(secondRing       = Item.NoItem)),
    Slot("Пояс",           _.belt,             _.copy(belt             = Item.NoItem)),
    Slot("Фляга",          _.flask,            _.copy(flask            = Item.NoItem)),
    Slot("Оружие",         _.weapon,           _.copy(weapon           = Item.NoItem)),
    Slot("Доп. оружие",    _.additionalWeapon, _.copy(additionalWeapon = Item.NoItem))
  )

  def itemStats(item: Item): String = {
    val stats    = item.statsLines
    val statsStr = if (stats.isEmpty) "Нет характеристик" else stats.mkString("\n")
    s"${item.name} Ур.${item.lvl}\n$statsStr"
  }
}
