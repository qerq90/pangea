package pangea.service.state.states

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.{Equipment, Hero}
import pangea.model.item.Item
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.states.EquipmentState.EquipmentPage
import pangea.service.state.{InventoryFeedback, State, UserAction}
import zio.{Task, ZIO}

case class EquipmentState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "Prev"          -> Target.Run { (user, _, renderer) => navigate(user, renderer, -1) },
      "Next"          -> Target.Run { (user, _, renderer) => navigate(user, renderer, +1) },
      "Unequip"       -> Target.Run { (user, _, renderer) => unequip(user, renderer) },
      "BackFromEquip" -> Target.Goto(StateType.HeroStats)
    ),
    fallback = Target.Run { (user, _, renderer) => showCurrentSlot(user, renderer) }
  )

  override def targetStates: Set[StateType] = Set(StateType.HeroStats, StateType.Equipment)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      _    <- heroDao.writeSceneData(user.userId, EquipmentPage(0).asJson)
      _    <- showSlot(user, hero, 0, renderer)
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def navigate(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    for {
      hero    <- getHero(user)
      page    <- currentPage(user)
      newPage  = (page + delta).max(0).min(EquipmentState.slots.size - 1)
      _       <- heroDao.writeSceneData(user.userId, EquipmentPage(newPage).asJson)
      _       <- showSlot(user, hero, newPage, renderer)
    } yield StateType.Equipment

  private def unequip(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      page <- currentPage(user)
      slot  = EquipmentState.slots(page)
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
                   InventoryFeedback.freeSlotsLine(inventoryRepo, content, hero.id).flatMap(slots =>
                     renderer.show(user, Screen(
                       content.format("equipment.unequipped", "name" -> item.name) + "\n" + slots, Nil)))
               }
             )
      hero2 <- getHero(user)
      _     <- showSlot(user, hero2, page, renderer)
    } yield StateType.Equipment

  private def showCurrentSlot(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      page <- currentPage(user)
      _    <- showSlot(user, hero, page, renderer)
    } yield StateType.Equipment

  private def showSlot(user: User, hero: Hero, page: Int, renderer: Renderer): Task[Unit] = {
    val slot    = EquipmentState.slots(page)
    val item    = slot.get(hero.equipment)
    val isEmpty = item.itemType == pangea.model.item.ItemType.NoItem
    val itemLine = if (isEmpty) content.text("equipment.empty")
                  else EquipmentState.itemStats(item)
    val text = s"🎽 ${content.text("equipment.header")} (${page + 1}/${EquipmentState.slots.size})\n\n${slot.name}\n$itemLine"
    val choices = List(
      Option.when(page > 0)(content.choice("Prev", "common.prev")),
      Option.when(!isEmpty)(content.choice("Unequip", "equipment.unequipBtn")),
      Option.when(page < EquipmentState.slots.size - 1)(content.choice("Next", "common.next")),
      Some(content.choice("BackFromEquip", "equipment.back"))
    ).flatten
    renderer.show(user, Screen(text, choices))
  }

  private def currentPage(user: User): Task[Int] =
    heroDao.readSceneData(user.userId)
      .map(_.flatMap(_.as[EquipmentPage].toOption).map(_.page).getOrElse(0))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object EquipmentState {

  case class EquipmentPage(page: Int)
  object EquipmentPage {
    implicit val encoder: Encoder[EquipmentPage] = deriveEncoder
    implicit val decoder: Decoder[EquipmentPage] = deriveDecoder
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
    val stats = List(
      Option.when(item.attack > 0)(s"Атака: +${item.attack}"),
      Option.when(item.accuracy > 0)(s"Точность: +${item.accuracy}"),
      Option.when(item.concentration > 0)(s"Концентрация: +${item.concentration}"),
      Option.when(item.armor > 0)(s"Броня: +${item.armor}"),
      Option.when(item.defence > 0)(s"Защита: +${item.defence}"),
      Option.when(item.evasion > 0)(s"Уклонение: +${item.evasion}"),
      Option.when(item.hp > 0)(s"HP: +${item.hp}")
    ).flatten
    val statsStr = if (stats.isEmpty) "Нет характеристик" else stats.mkString("\n")
    s"${item.name} [${item.rarity}] Ур.${item.lvl}\n$statsStr"
  }
}
