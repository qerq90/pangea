package pangea.service.state.states

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, jawn}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.{Equipment, Hero}
import pangea.model.item.{Item, ItemType}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.states.InventoryState._
import pangea.service.state.{ItemMenu, State, UserAction}
import zio.{Task, ZIO}

case class InventoryState(
  heroDao:      HeroDao,
  inventoryRepo: InventoryRepository,
  content:      SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "BackFromInventory" -> Target.Goto(StateType.HeroStats),
      "InventoryList"     -> Target.Run { (u, _, r) => writeScene(u, InventoryScene(page = Some(0))) *> showList(u, r).as(StateType.Inventory) },
      "InventoryPrev"     -> Target.Run { (u, _, r) => navigate(u, r, -1) },
      "InventoryNext"     -> Target.Run { (u, _, r) => navigate(u, r, +1) },
      "Equip"             -> Target.Run { (u, _, r) => equipSelected(u, r) },
      "Drop"              -> Target.Run { (u, _, r) => dropSelected(u, r) }
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = Set(StateType.HeroStats, StateType.Inventory)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    writeScene(user, InventoryScene(page = Some(0))) *> showList(user, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Список ──────────────────────────────────────────────────────────────────

  private def showList(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      items  = inv.items.data
      scene <- readScene(user)
      _ <- if (items.isEmpty) renderer.show(user, emptyScreen(hero))
           else {
             val (pageItems, totalPages, page) = ItemMenu.page(items, scene.page.getOrElse(0))
             val header   = s"📦 Инвентарь${if (totalPages > 1) s" (${page + 1}/$totalPages)" else ""} | 💰 ${hero.gold} | 🪙 ${hero.doubloons}"
             val itemBtns = ItemMenu.itemButtons(pageItems, ItemActionPrefix)
             val nav      = navRow(page, totalPages, "InventoryPrev", "InventoryNext", "BackFromInventory")
             renderer.show(user, Screen(header, itemBtns ++ nav))
           }
    } yield StateType.Inventory

  private def navigate(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      (_, totalPages, _) = ItemMenu.page(inv.items.data, 0)
      scene <- readScene(user)
      cur    = scene.page.getOrElse(0)
      np     = (cur + delta).max(0).min(totalPages - 1)
      _     <- writeScene(user, scene.copy(page = Some(np)))
      res   <- showList(user, renderer)
    } yield res

  // ── Детальный экран выбранного предмета ────────────────────────────────────

  private def showItem(user: User, itemId: Long, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      scene <- readScene(user)
      res <- inv.items.data.find(_.id == itemId) match {
        case None => showList(user, renderer)
        case Some(item) =>
          val text    = itemDetail(item, hero.equipment, hero.gold)
          val canEquip = item.itemType != ItemType.Trophy
          val choices  = List(
            Option.when(canEquip)(content.choice("Equip", "inventory.equip").copy(row = Some(0))),
            Some(content.choice("Drop", "inventory.drop").copy(color = ChoiceColor.Negative, row = Some(0))),
            Some(content.choice("InventoryList", "inventory.exit").copy(row = Some(1), color = ChoiceColor.Negative))
          ).flatten
          writeScene(user, scene.copy(selectedId = Some(itemId))) *>
            renderer.show(user, Screen(text, choices)).as(StateType.Inventory)
      }
    } yield res

  // ── Действия с выбранным предметом ─────────────────────────────────────────

  private def equipSelected(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      res <- scene.selectedId match {
        case None => showList(user, renderer)
        case Some(id) => equipById(user, id, renderer)
      }
    } yield res

  private def equipById(user: User, itemId: Long, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      _ <- inv.items.data.find(_.id == itemId) match {
        case None => ZIO.unit
        case Some(item) =>
          if (item.itemType == ItemType.Trophy)
            renderer.show(user, Screen(content.text("inventory.notEquippable"), Nil))
          else if (item.lvl > hero.lvl)
            renderer.show(user, Screen(
              content.format("inventory.tooHighLevel",
                "required" -> item.lvl.toString,
                "current"  -> hero.lvl.toString), Nil))
          else {
            val (newEq, newFight, oldItem) = InventoryState.equip(hero, item)
            heroDao.updateEquipmentAndFightStats(user.userId, newEq, newFight) *>
              inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
              ZIO.when(oldItem.itemType != ItemType.NoItem)(inventoryRepo.addItem(hero.id, oldItem).ignore) *>
              renderer.show(user, Screen(
                content.format("inventory.equipped",
                  "name" -> item.name,
                  "old"  -> (if (oldItem.itemType == ItemType.NoItem) "ничего" else oldItem.name)), Nil))
          }
      }
      res <- showList(user, renderer)
    } yield res

  private def dropSelected(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      res <- scene.selectedId match {
        case None => showList(user, renderer)
        case Some(id) => dropById(user, id, renderer)
      }
    } yield res

  private def dropById(user: User, itemId: Long, renderer: Renderer): Task[StateType] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      _ <- inv.items.data.find(_.id == itemId) match {
        case None => ZIO.unit
        case Some(item) =>
          inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
            renderer.show(user, Screen(content.format("inventory.dropped", "name" -> item.name), Nil))
      }
      res <- showList(user, renderer)
    } yield res

  // ── Fallback: динамические id вида ItemAction_<id> ─────────────────────────

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(ItemActionPrefix) =>
        a.drop(ItemActionPrefix.length).toLongOption.fold(showList(user, renderer))(showItem(user, _, renderer))
      case _ => showList(user, renderer)
    }

  // ── Хелперы ─────────────────────────────────────────────────────────────────

  private def emptyScreen(hero: Hero): Screen = {
    val base = content.screen("inventory.empty")
    Screen(s"📦 Инвентарь пуст | 💰 ${hero.gold} | 🪙 ${hero.doubloons}", base.choices)
  }

  private def navRow(page: Int, totalPages: Int, prevId: String, nextId: String, backId: String): List[Choice] = {
    val row = ItemMenu.NavRow
    List(
      Some(content.choice(backId, "inventory.exit").copy(row = Some(row), color = ChoiceColor.Negative)),
      Option.when(page > 0)(Choice(prevId, content.text("common.prev"), row = Some(row))),
      Option.when(page < totalPages - 1)(Choice(nextId, content.text("common.next"), row = Some(row)))
    ).flatten
  }

  private def itemDetail(item: Item, eq: Equipment, gold: Long): String =
    s"💰 $gold\n\n${itemText(item, eq)}"

  private def readScene(user: User): Task[InventoryScene] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[InventoryScene].toOption).getOrElse(InventoryScene()))

  private def writeScene(user: User, scene: InventoryScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object InventoryState {

  val ItemActionPrefix = "InventoryItem_"

  case class InventoryScene(page: Option[Int] = None, selectedId: Option[Long] = None)
  object InventoryScene {
    implicit val encoder: Encoder[InventoryScene] = deriveEncoder
    implicit val decoder: Decoder[InventoryScene] = deriveDecoder
  }

  def equip(hero: Hero, item: Item): (Equipment, FightStats, Item) = {
    val oldItem  = equippedIn(hero.equipment, item.itemType)
    val newEq    = withSlot(hero.equipment, item)
    val newFight = applyDelta(hero.fightStats, item, oldItem)
    (newEq, newFight, oldItem)
  }

  /** Подробное представление предмета (статы + надетое в том же слоте). Используется
   *  и в детальном экране инвентаря, и снаружи (например, регистрация). */
  def itemText(item: Item, eq: Equipment): String = {
    def equipped(prefix: String, cur: Item): String =
      if (cur.itemType == ItemType.NoItem) s"$prefix: свободен"
      else cur.equippedComparison(prefix)
    // Разделитель между сравниваемым предметом и тем, что уже надето.
    val sep = "➖➖➖➖➖"
    val slotInfo = item.itemType match {
      case ItemType.Trophy => "\nТрофей"
      case ItemType.Ring   => s"\n$sep\n${equipped("Слот 1", eq.firstRing)}\n${equipped("Слот 2", eq.secondRing)}"
      case _ =>
        val cur = equippedIn(eq, item.itemType)
        if (cur.itemType == ItemType.NoItem) ""
        else s"\n$sep\n${equipped("Сейчас надет", cur)}"
    }
    val stats    = item.statsLines
    val statsStr = if (stats.isEmpty) "Нет характеристик" else stats.mkString("\n")
    s"${item.name} Ур.${item.lvl}\n$statsStr$slotInfo"
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
