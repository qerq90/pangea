package pangea.service.state.states

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder, jawn}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.{MaterialGenerator, TreasureMapGenerator}
import pangea.model.hero.{Equipment, Hero, WeaponDust}
import pangea.model.inventory.Inventory
import pangea.model.item.{Gem, GemBreaking, Item, ItemDetails, ItemStack, ItemType}
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.InventoryState._
import pangea.service.state.{ItemMenu, State, UiScene, UserAction}
import zio.{Task, ZIO}

case class InventoryState(
  heroDao:        HeroDao,
  inventoryRepo:  InventoryRepository,
  itemRepository: ItemRepository,
  content:        SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "BackFromInventory" -> Target.Goto(StateType.HeroStats),
      "InventoryList"     -> Target.Run { (u, _, r) => writeScene(u, InventoryScene(page = Some(0))) *> showList(u, r).as(StateType.Inventory) },
      "InventoryPrev"     -> Target.Run { (u, _, r) => navigate(u, r, -1) },
      "InventoryNext"     -> Target.Run { (u, _, r) => navigate(u, r, +1) },
      "Equip"             -> Target.Run { (u, _, r) => equipSelected(u, r) },
      "EquipRing"         -> Target.Run { (u, ua, r) => equipChosenRing(u, ua, r) },
      "Drop"              -> Target.Run { (u, _, r) => dropSelected(u, r) },
      "CombineMap"        -> Target.Run { (u, _, r) => combineSelected(u, r) },
      "SocketInsert"      -> Target.Run { (u, _, r) => startSocketing(u, r) },
      "BreakGem"          -> Target.Run { (u, _,  r) => offerBreak(u, r) },
      "BreakGemPick"      -> Target.Run { (u, ua, r) => confirmBreak(u, ua, r) },
      "BreakGemDo"        -> Target.Run { (u, ua, r) => doBreak(u, ua, r) },
      "CrushGem"          -> Target.Run { (u, _,  r) => offerCrush(u, r) },
      "CrushGemDo"        -> Target.Run { (u, _,  r) => doCrush(u, r) },
      "DustWeapon"        -> Target.Run { (u, _,  r) => sprinkleDust(u, r) }
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = Set(StateType.HeroStats, StateType.Inventory, StateType.Socketing)

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
             // Одинаковые камни, материалы и трофеи показываются одной кнопкой,
             // но места в сумке занимают по-прежнему поштучно.
             val stacks = ItemStack.grouped(items)
             val (pageItems, totalPages, page) = ItemMenu.page(stacks, scene.page.getOrElse(0))
             val header   = s"📦 Инвентарь${if (totalPages > 1) s" (${page + 1}/$totalPages)" else ""} | 🪙 ${hero.silver} | 🟡 ${hero.doubloons}"
             val itemBtns = ItemMenu.stackButtons(pageItems, ItemActionPrefix)
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
          // Сколько таких же лежит в сумке — счётчик над описанием; кнопки при
          // этом трогают ровно один предмет из стопки.
          val count    = ItemStack.countOf(inv.items.data, item)
          val text     = itemDetail(item, hero, hero.silver) + stackLine(count)
          val canEquip = ItemType.equippable.contains(item.itemType)
          val canCombine = item.itemType == ItemType.TreasureMapHalf
          val canSocket  = item.gem.isDefined
          // Ломать можно и камни в гнёздах вещи, и сам камень, лежащий в сумке.
          val canBreak   = GemBreaking.hasGems(item)
          val choices  = List(
            Option.when(canEquip)(content.choice("Equip", "inventory.equip").copy(row = Some(0))),
            Option.when(canCombine)(content.choice("CombineMap", "inventory.combineMap").copy(color = ChoiceColor.Positive, row = Some(0))),
            Option.when(canSocket)(content.choice("SocketInsert", "inventory.socket").copy(color = ChoiceColor.Positive, row = Some(0))),
            Option.when(canBreak)(content.choice("BreakGem", "inventory.breakGem").copy(color = ChoiceColor.Negative, row = Some(0))),
            Option.when(canSocket)(content.choice("CrushGem", "inventory.crushGem").copy(color = ChoiceColor.Negative, row = Some(0))),
            // Пыль сыплется на оружие: разовое покрытие на один бой.
            Option.when(item.material.exists(_.gem.isDefined))(
              content.choice("DustWeapon", "inventory.dustWeapon").copy(color = ChoiceColor.Positive, row = Some(0))),
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
        case Some(id) => equipById(user, id, renderer, ringSlot = None)
      }
    } yield res

  // Игрок выбрал, какое из двух надетых колец снять (кнопки экрана выбора).
  private def equipChosenRing(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    (payloadField(ua, "id").flatMap(_.toLongOption), payloadField(ua, "slot").flatMap(_.toIntOption)) match {
      case (Some(id), Some(slot)) => equipById(user, id, renderer, ringSlot = Some(slot))
      case _                      => showList(user, renderer)
    }

  /** Экран выбора кольца: какое из двух надетых снять ради нового. */
  private def ringChoiceScreen(newRing: Item, eq: Equipment, itemId: Long): Screen = {
    def slotButton(slot: Int, worn: Item) =
      Choice(
        id    = "EquipRing",
        label = ItemMenu.truncate(content.format("inventory.ringSlot",
                  "slot" -> slot.toString, "name" -> worn.displayTitle)),
        data  = Map("id" -> itemId.toString, "slot" -> slot.toString),
        row   = Some(slot - 1)
      )
    Screen(
      content.format("inventory.ringChoice", "name" -> newRing.displayTitle) + "\n\n" +
        eq.firstRing.equippedComparison("Слот 1") + "\n" + Item.ComparisonSeparator + "\n" +
        eq.secondRing.equippedComparison("Слот 2"),
      List(
        slotButton(1, eq.firstRing),
        slotButton(2, eq.secondRing),
        content.choice("InventoryList", "inventory.exit").copy(row = Some(2))
      )
    )
  }

  private def equipById(user: User, itemId: Long, renderer: Renderer, ringSlot: Option[Int]): Task[StateType] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      // true — остаёмся на показанном экране (выбор кольца ждёт нажатия и
      // перерисовывать поверх него список нельзя).
      keepScreen <- inv.items.data.find(_.id == itemId) match {
        case None => ZIO.succeed(false)
        case Some(item) =>
          if (!ItemType.equippable.contains(item.itemType))
            renderer.show(user, Screen(content.text("inventory.notEquippable"), Nil)).as(false)
          else if (item.lvl > hero.lvl)
            renderer.show(user, Screen(
              content.format("inventory.tooHighLevel",
                "required" -> item.lvl.toString,
                "current"  -> hero.lvl.toString), Nil)).as(false)
          // Оба слота колец заняты и слот ещё не выбран — спрашиваем, какое снять.
          else if (item.itemType == ItemType.Ring && ringSlot.isEmpty &&
                   InventoryState.ringSlotsFull(hero.equipment))
            renderer.show(user, ringChoiceScreen(item, hero.equipment, itemId)).as(true)
          else {
            val (newEq, newFight, oldItem) = InventoryState.equip(hero, item, ringSlot)
            val capDelta = InventoryState.equipmentStashDelta(hero.equipment, newEq)
            // Замена, снимающая последний «Тайник» в экипировке (делта < 0), переполнит сумку → блокируем.
            if (capDelta < 0 && !InventoryState.fitsAfterCapacityChange(inv, capDelta, returningItems = 0))
              renderer.show(user, Screen(content.text("equipment.stashBlocked"), Nil)).as(false)
            else
              (heroDao.updateEquipmentAndFightStats(user.userId, newEq, newFight) *>
                inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
                ZIO.when(oldItem.itemType != ItemType.NoItem)(inventoryRepo.addItem(hero.id, oldItem).ignore) *>
                ZIO.when(capDelta != 0)(inventoryRepo.increaseCapacity(hero.id, capDelta).orElse(ZIO.unit)) *>
                renderer.show(user, Screen(
                  content.format("inventory.equipped",
                    "name" -> item.name,
                    "old"  -> (if (oldItem.itemType == ItemType.NoItem) "ничего" else oldItem.name)), Nil))).as(false)
          }
      }
      res <- if (keepScreen) ZIO.succeed(StateType.Inventory) else showList(user, renderer)
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

  // ── Объединение половинок карты ────────────────────────────────────────────

  private def combineSelected(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      res <- scene.selectedId match {
        case None     => showList(user, renderer)
        case Some(id) => combineById(user, id, renderer)
      }
    } yield res

  // Две половинки одной зоны → одна целая карта этой зоны (уровень — больший из двух).
  private def combineById(user: User, halfId: Long, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      items  = inv.items.data
      res <- items.find(_.id == halfId) match {
        case Some(first) if first.itemType == ItemType.TreasureMapHalf =>
          val firstZone = zoneOf(first)
          val second = items.find(i =>
            i.id != first.id && i.itemType == ItemType.TreasureMapHalf && zoneOf(i) == firstZone)
          (firstZone, second) match {
            case (Some(zone), Some(other)) =>
              val full = TreasureMapGenerator.full(zone)
              for {
                _         <- inventoryRepo.removeItems(Set(first.id, other.id), hero.id).mapError(e => new Throwable(e.toString))
                persisted <- itemRepository.persist(hero.id, full)
                _         <- inventoryRepo.addItem(hero.id, persisted).mapError(e => new Throwable(e.toString))
                _         <- renderer.show(user, Screen(content.format("inventory.combined", "name" -> full.name), Nil))
                back      <- showList(user, renderer)
              } yield back
            case _ =>
              val name = firstZone.map(_.treasureName).getOrElse("")
              renderer.show(user, Screen(content.format("inventory.combineNeedSecond", "name" -> name), Nil)) *>
                showList(user, renderer)
          }
        case _ => showList(user, renderer)
      }
    } yield res

  // ── Ломка камней ───────────────────────────────────────────────────────────

  /** Первый шаг: если камень в вещи один — сразу спрашиваем подтверждение, если
    * несколько — сначала даём выбрать, какой именно ломать. */
  private def offerBreak(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, _) =>
      GemBreaking.socketed(item) match {
        case Nil             => showList(user, renderer)
        case (idx, _) :: Nil => breakConfirmScreen(user, item, idx, renderer)
        case gems =>
          val choices = gems.map { case (idx, gem) =>
            Choice(
              "BreakGemPick",
              content.format("inventory.breakGemOption", "gem" -> gem.displayName),
              data  = Map("slot" -> idx.toString),
              color = ChoiceColor.Negative,
              row   = Some(0))
          } :+ content.choice("InventoryList", "inventory.exit").copy(row = Some(1))
          renderer.show(user, Screen(
            content.format("inventory.breakGemWhich", "item" -> item.name), choices)).as(StateType.Inventory)
      }
    }

  private def confirmBreak(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    payloadField(ua, "slot").flatMap(_.toIntOption) match {
      case None      => showList(user, renderer)
      case Some(idx) => withSelected(user, renderer)((item, _) => breakConfirmScreen(user, item, idx, renderer))
    }

  /** Красное подтверждение: сколько пыли выйдет и что камень пропадёт навсегда. */
  private def breakConfirmScreen(user: User, item: Item, idx: Int, renderer: Renderer): Task[StateType] =
    item.sockets.lift(idx).flatten match {
      case None => showList(user, renderer)
      case Some(gem) =>
        renderer.show(user, Screen(
          content.format("inventory.breakGemConfirm",
            "gem"   -> gem.displayName,
            "dust"  -> gem.dust.displayName,
            "count" -> gem.dustYield.toString),
          List(
            Choice("BreakGemDo", content.text("inventory.breakGemYes"),
              data = Map("slot" -> idx.toString), color = ChoiceColor.Negative, row = Some(0)),
            Choice("InventoryList", content.text("inventory.breakGemNo"), row = Some(1))
          ))).as(StateType.Inventory)
    }

  private def doBreak(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    payloadField(ua, "slot").flatMap(_.toIntOption) match {
      case None => showList(user, renderer)
      case Some(idx) =>
        withSelected(user, renderer) { (item, hero) =>
          GemBreaking.breakSocket(item, idx) match {
            case None => showList(user, renderer)
            case Some((emptied, gem)) =>
              inventoryRepo.updateItem(hero.id, emptied).mapError(e => new Throwable(e.toString)) *>
                grantDust(user, hero, gem, renderer)
          }
        }
    }

  /** Камень, лежащий в сумке отдельным предметом: крошим его целиком. */
  private def offerCrush(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, _) =>
      item.gem match {
        case None => showList(user, renderer)
        case Some(gem) =>
          renderer.show(user, Screen(
            content.format("inventory.crushGemConfirm",
              "gem"   -> gem.displayName,
              "dust"  -> gem.dust.displayName,
              "count" -> gem.dustYield.toString),
            List(
              Choice("CrushGemDo", content.text("inventory.breakGemYes"), color = ChoiceColor.Negative, row = Some(0)),
              Choice("InventoryList", content.text("inventory.breakGemNo"), row = Some(1))
            ))).as(StateType.Inventory)
      }
    }

  private def doCrush(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      item.gem match {
        case None => showList(user, renderer)
        case Some(gem) =>
          inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
            grantDust(user, hero, gem, renderer)
      }
    }

  /** Выдаёт пыль за сломанный камень. Пыль кладётся отдельными предметами, поэтому
    * место в сумке проверяем заранее: лучше отказать, чем потерять часть пыли. */
  private def grantDust(user: User, hero: Hero, gem: Gem, renderer: Renderer): Task[StateType] =
    for {
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      count  = gem.dustYield
      free   = inv.maxItems - inv.items.data.length
      res <-
        if (free < count)
          renderer.show(user, Screen(content.format("inventory.breakGemNoRoom", "count" -> count.toString), Nil)) *>
            showList(user, renderer)
        else
          ZIO.foreachDiscard(1 to count) { _ =>
            itemRepository.persist(hero.id, MaterialGenerator.item(gem.dust))
              .flatMap(persisted => inventoryRepo.addItem(hero.id, persisted).mapError(e => new Throwable(e.toString)))
          } *>
            renderer.show(user, Screen(content.format("inventory.breakGemDone",
              "gem" -> gem.displayName, "count" -> count.toString, "dust" -> gem.dust.displayName), Nil)) *>
            showList(user, renderer)
    } yield res

  /** Общая обвязка «взять выбранный в сцене предмет и героя». */
  private def withSelected(user: User, renderer: Renderer)(
      f: (Item, Hero) => Task[StateType]): Task[StateType] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      res <- scene.selectedId.flatMap(id => inv.items.data.find(_.id == id)) match {
        case None       => showList(user, renderer)
        case Some(item) => f(item, hero)
      }
    } yield res

  // ── Пыль на оружие ─────────────────────────────────────────────────────────

  /** Посыпает надетое оружие выбранной пылью. Горсть тратится в любом случае —
    * даже когда магия даёт всполох или покрытие идёт четвёртым слоем. */
  private def sprinkleDust(user: User, renderer: Renderer): Task[StateType] =
    withSelected(user, renderer) { (item, hero) =>
      item.material.filter(_.gem.isDefined) match {
        case None => showList(user, renderer)
        case Some(_) if hero.equipment.weapon.itemType == ItemType.NoItem =>
          renderer.show(user, Screen(content.text("inventory.dustNoWeapon"), Nil)) *> showList(user, renderer)
        case Some(dust) =>
          val outcome = WeaponDust.sprinkle(dust, hero.weaponDust, hero.equipment.weapon.socketedGems)
          val line = outcome match {
            // Сколько слоёв уже на оружии, игрок держит в голове сам — не считаем за него.
            case WeaponDust.Outcome.Applied(_) =>
              content.format("inventory.dustApplied",
                "dust"   -> dust.displayName,
                "weapon" -> hero.equipment.weapon.name)
            case WeaponDust.Outcome.Clash(_) =>
              content.format("inventory.dustClash", "dust" -> dust.displayName,
                "pct" -> WeaponDust.PenaltyPct.toString)
            case WeaponDust.Outcome.Overload(_) =>
              content.format("inventory.dustOverload", "pct" -> WeaponDust.PenaltyPct.toString)
          }
          inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
            heroDao.updateWeaponDust(user.userId, outcome.next) *>
            renderer.show(user, Screen(line, Nil)) *>
            showList(user, renderer)
      }
    }

  // ── Вставка камня: уходим на экран Socketing с id выбранного камня ─────────

  private def startSocketing(user: User, renderer: Renderer): Task[StateType] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      res <- scene.selectedId.flatMap(id => inv.items.data.find(_.id == id)).flatMap(_.gem.map(_ => scene.selectedId.get)) match {
        case None => showList(user, renderer)
        case Some(gemId) =>
          UiScene.write(heroDao, user.userId, UiScene.Socketing, SocketingState.Scene(gemId))
            .as(StateType.Socketing)
      }
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
    Screen(s"📦 Инвентарь пуст | 🪙 ${hero.silver} | 🟡 ${hero.doubloons}", base.choices)
  }

  private def navRow(page: Int, totalPages: Int, prevId: String, nextId: String, backId: String): List[Choice] = {
    val row = ItemMenu.NavRow
    List(
      Some(content.choice(backId, "inventory.exit").copy(row = Some(row), color = ChoiceColor.Negative)),
      Option.when(page > 0)(Choice(prevId, content.text("common.prev"), row = Some(row))),
      Option.when(page < totalPages - 1)(Choice(nextId, content.text("common.next"), row = Some(row)))
    ).flatten
  }

  /** Приписка «в сумке: N шт» — только когда таких вещей больше одной. */
  private def stackLine(count: Int): String =
    if (count > 1) "\n\n" + content.format("inventory.stackCount", "count" -> count.toString) else ""

  private def itemDetail(item: Item, hero: Hero, silver: Long): String =
    s"🪙 $silver\n\n${itemText(item, hero.equipment, Some(hero))}"

  private def readScene(user: User): Task[InventoryScene] =
    UiScene.read(heroDao, user.userId, UiScene.Inventory, InventoryScene())

  private def writeScene(user: User, scene: InventoryScene): Task[Unit] =
    UiScene.write(heroDao, user.userId, UiScene.Inventory, scene)

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  // Произвольное поле payload кнопки (напр. выбранный слот кольца).
  private def payloadField(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

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

  /** Оба слота колец заняты — новое кольцо можно надеть только вместо одного из
    * них, поэтому слот спрашиваем у игрока (см. `ringChoiceScreen`). */
  def ringSlotsFull(eq: Equipment): Boolean =
    eq.firstRing.itemType != ItemType.NoItem && eq.secondRing.itemType != ItemType.NoItem

  /** `ringSlot` — выбранный игроком слот кольца (1 или 2); имеет смысл только
    * когда оба слота заняты. None — обычное правило слотов: первый свободный,
    * а если свободных нет, второй. */
  def equip(hero: Hero, item: Item, ringSlot: Option[Int]): (Equipment, FightStats, Item) = {
    val eq = hero.equipment
    val (newEq, oldItem) = (item.itemType, ringSlot) match {
      case (ItemType.Ring, Some(1)) => (eq.copy(firstRing = item), eq.firstRing)
      case (ItemType.Ring, Some(2)) => (eq.copy(secondRing = item), eq.secondRing)
      case _                        => (withSlot(eq, item), equippedIn(eq, item.itemType))
    }
    val newFight = applyDelta(hero.fightStats, item, oldItem)
    (newEq, newFight, oldItem)
  }

  /** Бонус вместимости сумки от пассивки «Тайник» на предмете (0, если её нет).
   *  Хелпер для одного предмета — для итогового изменения вместимости при
   *  надевании/снятии используй [[equipmentStashDelta]] (он дедуплицирует
   *  несколько «Тайников» так же, как остальные пассивки). */
  def stashBonus(item: Item): Long =
    if (item.passive.contains(pangea.model.item.PassiveKind.Stash)) pangea.model.item.PassiveKind.Stash.ExtraSlots
    else 0L

  /** Изменение вместимости сумки при переходе от `oldEq` к `newEq`: разница
   *  бонусов «Тайника», посчитанная по ПОЛНОМУ набору пассивок экипировки
   *  (`Equipment.passiveKinds` — множество, дубли схлопнуты), а не по одному
   *  заменяемому предмету. Поэтому надевание второго «Тайника», пока первый уже
   *  на герое, не даёт delta (бонус уже был учтён) — как и остальные пассивки,
   *  «работает только одна». Положительное — надет первый «Тайник» в экипировке,
   *  отрицательное — снят последний. */
  def equipmentStashDelta(oldEq: Equipment, newEq: Equipment): Long =
    pangea.model.hero.HeroPassives(newEq.passiveKinds).extraInventorySlots -
      pangea.model.hero.HeroPassives(oldEq.passiveKinds).extraInventorySlots

  /** Хватит ли места в сумке, если её вместимость изменится на `capDelta` (может
   *  быть отрицательным — например, теряем бонус «Тайника»), с учётом
   *  `returningItems` предметов, которые вернутся в сумку. Ложь → изменение
   *  переполнит сумку, и его надо блокировать. */
  def fitsAfterCapacityChange(inv: Inventory, capDelta: Long, returningItems: Int): Boolean =
    inv.items.data.length + returningItems <= inv.maxItems + capDelta

  /** Зона карты клада (для целой карты и её половинки); None у прочих предметов. */
  private def zoneOf(item: Item): Option[pangea.model.item.MapZone] = item.details match {
    case ItemDetails.TreasureMap(zone) => Some(zone)
    case _                             => None
  }

  /** Подробное представление предмета (статы + надетое в том же слоте). Используется
   *  и в детальном экране инвентаря, и снаружи (например, регистрация). */
  def itemText(item: Item, eq: Equipment, hero: Option[Hero] = None): String = item.mapDescription match {
    // Карта клада: только имя (без уровня) и текст-описание, без статов и сравнения
    // слотов — карта не надевается (см. также [[ItemType.equippable]]).
    case Some(desc) => s"${item.displayTitle}\n\n$desc"
    case None       => gearText(item, eq, hero)
  }

  private def gearText(item: Item, eq: Equipment, hero: Option[Hero]): String = {
    def equipped(prefix: String, cur: Item): String =
      if (cur.itemType == ItemType.NoItem) s"$prefix: свободен"
      else cur.equippedComparison(prefix)
    // Разделитель между сравниваемым предметом и тем, что уже надето.
    val sep = Item.ComparisonSeparator
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
    // Описание активного навыка (с подставленной стоимостью энергии от уровня героя).
    val skillStr = (hero, item.activeSkill) match {
      case (Some(h), Some(s)) => s"\n\n${s.describe(h)}"
      case _                  => ""
    }
    // Описание пассивного навыка (статичный текст — без стоимости/кулдауна).
    val passiveStr = item.passive.map(k => s"\n\n${k.describe}").getOrElse("")
    s"${item.displayTitle}\n$statsStr$skillStr$passiveStr$slotInfo"
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
    case ItemType.TreasureMap      => Item.NoItem // карта не экипируется
    case ItemType.TreasureMapHalf  => Item.NoItem // половинка карты не экипируется
    case ItemType.Gem              => Item.NoItem // камень не экипируется
    case ItemType.Material         => Item.NoItem // материал не экипируется
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
    case ItemType.TreasureMap      => eq // карта не экипируется
    case ItemType.TreasureMapHalf  => eq // половинка карты не экипируется
    case ItemType.Gem              => eq // камень не экипируется
    case ItemType.Material         => eq // материал не экипируется
    case ItemType.NoItem           => eq
  }

  def applyDelta(base: FightStats, added: Item, removed: Item): FightStats =
    base.copy(
      atk           = (base.atk + added.attack - removed.attack).max(0L),
      armor         = (base.armor + added.armor - removed.armor).max(0L),
      defence       = (base.defence + added.defence - removed.defence).max(0L),
      evasion       = (base.evasion + added.evasion - removed.evasion).max(0L),
      accuracy      = (base.accuracy + added.accuracy - removed.accuracy).max(0L),
      energy        = (base.energy + added.energy - removed.energy).max(0L)
    )
}
