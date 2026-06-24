package pangea.service.state.states

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.{Equipment, Hero}
import pangea.model.item.{Item, ItemType}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.states.InventoryState.InventoryPage
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

case class InventoryState(
  heroDao:      HeroDao,
  inventoryRepo: InventoryRepository,
  content:      SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "BackFromInventory" -> Target.Run { (user, _, _)        => ZIO.succeed(StateType.Dungeon) },
      "Next"              -> Target.Run { (user, _, renderer) => navigate(user, renderer, +1) },
      "Prev"              -> Target.Run { (user, _, renderer) => navigate(user, renderer, -1) },
      "Equip"             -> Target.Run { (user, _, renderer) => equipCurrent(user, renderer) },
      "Drop"              -> Target.Run { (user, _, renderer) => dropCurrent(user, renderer) }
    ),
    fallback = Target.Run { (user, _, renderer) => showCurrentPage(user, renderer) }
  )

  override def targetStates: Set[StateType] = Set(StateType.Dungeon, StateType.Inventory)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      items  = inv.items.data
      _ <- if (items.isEmpty)
             renderer.show(user, emptyScreen(hero))
           else
             heroDao.writeSceneData(user.userId, InventoryPage(0).asJson) *>
               showItemScreen(user, hero, items, 0, renderer)
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def navigate(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    for {
      hero    <- getHero(user)
      inv     <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      items    = inv.items.data
      page    <- currentPage(user)
      newPage  = (page + delta).max(0).min((items.size - 1).max(0))
      _       <- heroDao.writeSceneData(user.userId, InventoryPage(newPage).asJson)
      _ <- if (items.isEmpty) renderer.show(user, emptyScreen(hero))
           else showItemScreen(user, hero, items, newPage, renderer)
    } yield StateType.Inventory

  private def equipCurrent(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      inv    <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      items   = inv.items.data
      page   <- currentPage(user)
      _ <- ZIO.when(items.nonEmpty && page < items.size) {
        val item = items(page)
        if (item.itemType == ItemType.Trophy)
          renderer.show(user, Screen(content.text("inventory.notEquippable"), Nil))
        else if (item.lvl > hero.lvl)
          renderer.show(user, Screen(
            content.format("inventory.tooHighLevel",
              "required" -> item.lvl.toString,
              "current"  -> hero.lvl.toString), Nil))
        else {
          val (newEq, newFight, oldItem) = InventoryState.equip(hero, item)
          for {
            _ <- heroDao.updateEquipmentAndFightStats(user.userId, newEq, newFight)
            _ <- inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString))
            _ <- ZIO.when(oldItem.itemType != ItemType.NoItem) {
                   inventoryRepo.addItem(hero.id, oldItem).ignore
                 }
            _ <- renderer.show(user, Screen(
                   content.format("inventory.equipped",
                     "name" -> item.name,
                     "old"  -> (if (oldItem.itemType == ItemType.NoItem) "ничего" else oldItem.name)), Nil))
          } yield ()
        }
      }
      hero2  <- getHero(user)
      inv2   <- inventoryRepo.get(hero2.id).mapError(e => new Throwable(e.toString))
      items2  = inv2.items.data
      newPage = page.min((items2.size - 1).max(0))
      _      <- heroDao.writeSceneData(user.userId, InventoryPage(newPage).asJson)
      _ <- if (items2.isEmpty) renderer.show(user, emptyScreen(hero2))
           else showItemScreen(user, hero2, items2, newPage, renderer)
    } yield StateType.Inventory

  private def dropCurrent(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      inv    <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      items   = inv.items.data
      page   <- currentPage(user)
      _ <- ZIO.when(items.nonEmpty && page < items.size) {
        val item = items(page)
        inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
          renderer.show(user, Screen(content.format("inventory.dropped", "name" -> item.name), Nil))
      }
      inv2   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      items2  = inv2.items.data
      newPage = page.min((items2.size - 1).max(0))
      _      <- heroDao.writeSceneData(user.userId, InventoryPage(newPage).asJson)
      _ <- if (items2.isEmpty) renderer.show(user, emptyScreen(hero))
           else showItemScreen(user, hero, items2, newPage, renderer)
    } yield StateType.Inventory

  private def showCurrentPage(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      items  = inv.items.data
      page  <- currentPage(user)
      _     <- if (items.isEmpty) renderer.show(user, emptyScreen(hero))
               else showItemScreen(user, hero, items, page.min((items.size - 1).max(0)), renderer)
    } yield StateType.Inventory

  private def emptyScreen(hero: Hero): Screen = {
    val base = content.screen("inventory.empty")
    Screen(s"📦 Инвентарь пуст | 💰 ${hero.gold}", base.choices)
  }

  private def showItemScreen(user: User, hero: Hero, items: List[Item], page: Int, renderer: Renderer): Task[Unit] = {
    val item    = items(page)
    val text    = InventoryState.itemText(item, hero.equipment, page + 1, items.size, Some(hero.gold))
    val choices = List(
      Option.when(page > 0)(content.choice("Prev", "common.prev")),
      Some(content.choice("Equip", "inventory.equip")),
      Some(content.choice("Drop",  "inventory.drop")),
      Option.when(page < items.size - 1)(content.choice("Next", "common.next")),
      Some(content.choice("BackFromInventory", "inventory.exit"))
    ).flatten
    renderer.show(user, Screen(text, choices))
  }

  private def currentPage(user: User): Task[Int] =
    heroDao.readSceneData(user.userId)
      .map(_.flatMap(_.as[InventoryPage].toOption).map(_.page).getOrElse(0))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object InventoryState {

  case class InventoryPage(page: Int)
  object InventoryPage {
    implicit val encoder: Encoder[InventoryPage] = deriveEncoder
    implicit val decoder: Decoder[InventoryPage] = deriveDecoder
  }

  def equip(hero: Hero, item: Item): (Equipment, FightStats, Item) = {
    val oldItem  = equippedIn(hero.equipment, item.itemType)
    val newEq    = withSlot(hero.equipment, item)
    val newFight = applyDelta(hero.fightStats, item, oldItem)
    (newEq, newFight, oldItem)
  }

  def itemText(item: Item, eq: Equipment, pageNum: Int, total: Int, gold: Option[Long] = None): String = {
    val slotInfo = item.itemType match {
      case ItemType.Trophy => "\nТрофей"
      case ItemType.Ring =>
        val r1 = if (eq.firstRing.itemType  != ItemType.NoItem) s"Слот 1: ${eq.firstRing.name}"  else "Слот 1: свободен"
        val r2 = if (eq.secondRing.itemType != ItemType.NoItem) s"Слот 2: ${eq.secondRing.name}" else "Слот 2: свободен"
        s"\n$r1\n$r2"
      case _ =>
        val cur = equippedIn(eq, item.itemType)
        if (cur.itemType != ItemType.NoItem) s"\nСейчас надет: ${cur.name}" else ""
    }
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
    val goldStr  = gold.fold("")(g => s" | 💰 $g")
    val rarityStr = if (item.itemType == ItemType.Trophy) "" else s" [${item.rarity}]" // у трофеев нет редкости
    s"📦 Инвентарь ($pageNum/$total)$goldStr\n\n${item.name}$rarityStr Ур.${item.lvl}\n$statsStr$slotInfo"
  }

  // Возвращает предмет, который будет вытеснен в инвентарь при надевании
  private def equippedIn(eq: Equipment, itemType: ItemType): Item = itemType match {
    case ItemType.Helmet           => eq.helmet
    case ItemType.ShoulderPads     => eq.shoulderPads
    case ItemType.ChestPlate       => eq.chestPlate
    case ItemType.Bracelets        => eq.bracelets
    case ItemType.Gloves           => eq.gloves
    case ItemType.Pants            => eq.pants
    case ItemType.Leggings         => eq.pants
    case ItemType.Boots            => eq.boots
    case ItemType.Amulet           => eq.amulet
    // Кольцо: ничего не вытесняется пока есть свободный слот; иначе вытесняется второй
    case ItemType.Ring             =>
      if (eq.firstRing.itemType == ItemType.NoItem || eq.secondRing.itemType == ItemType.NoItem) Item.NoItem
      else eq.secondRing
    case ItemType.Belt             => eq.belt
    case ItemType.Flask            => eq.flask
    case ItemType.Weapon           => eq.weapon
    case ItemType.AdditionalWeapon => eq.additionalWeapon
    case ItemType.Trophy           => Item.NoItem // трофей не экипируется
    case ItemType.NoItem           => Item.NoItem
  }

  private def withSlot(eq: Equipment, item: Item): Equipment = item.itemType match {
    case ItemType.Helmet           => eq.copy(helmet = item)
    case ItemType.ShoulderPads     => eq.copy(shoulderPads = item)
    case ItemType.ChestPlate       => eq.copy(chestPlate = item)
    case ItemType.Bracelets        => eq.copy(bracelets = item)
    case ItemType.Gloves           => eq.copy(gloves = item)
    case ItemType.Pants            => eq.copy(pants = item)
    case ItemType.Leggings         => eq.copy(pants = item)
    case ItemType.Boots            => eq.copy(boots = item)
    case ItemType.Amulet           => eq.copy(amulet = item)
    // Кольцо: первый свободный слот; если оба заняты — второй
    case ItemType.Ring             =>
      if (eq.firstRing.itemType == ItemType.NoItem) eq.copy(firstRing = item)
      else eq.copy(secondRing = item)
    case ItemType.Belt             => eq.copy(belt = item)
    case ItemType.Flask            => eq.copy(flask = item)
    case ItemType.Weapon           => eq.copy(weapon = item)
    case ItemType.AdditionalWeapon => eq.copy(additionalWeapon = item)
    case ItemType.Trophy           => eq // трофей не экипируется
    case ItemType.NoItem           => eq
  }

  def applyDelta(base: FightStats, added: Item, removed: Item): FightStats =
    base.copy(
      atk           = (base.atk + added.attack - removed.attack).max(0L),
      armor         = (base.armor + added.armor - removed.armor).max(0L),
      defence       = (base.defence + added.defence - removed.defence).max(0L),
      evasion       = (base.evasion + added.evasion - removed.evasion).max(0L),
      accuracy      = (base.accuracy + added.accuracy - removed.accuracy).max(0L),
      concentration = (base.concentration + added.concentration - removed.concentration).max(0L)
    )
}
