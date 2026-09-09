package pangea.service.state.states.merchant

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.ItemGenerator
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemType, Rarity}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.merchant.MerchantState._
import pangea.service.state.{CharacterMenu, InventoryFeedback, ItemMenu, State, UserAction}
import zio.{Random, Task, ZIO}

import java.util.concurrent.TimeUnit

/**
 * Лавка «Торговца Ришелье». Держит 3 случайных предмета снаряжения, зафиксированных
 * до явного обновления (кнопка «Обновить», не чаще раза в час). Сток — durable
 * (`heroes.merchant_data`), переживает переходы между сценами. Транзиентная пагинация
 * продажи живёт в `scene_data`.
 */
case class MerchantState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "Buy"             -> Target.Run { (u, ua, r) => confirmBuy(u, ua, r) },
      "ConfirmBuy"      -> Target.Run { (u, ua, r) => doBuy(u, ua, r) },
      "CancelBuy"       -> Target.Run { (u, _,  r) => showMenu(u, r).as(StateType.Merchant) },
      "Refresh"         -> Target.Run { (u, _,  r) => refresh(u, r) },
      "Sell"            -> Target.Run { (u, _,  r) => showSellList(u, r, 0) },
      "SellJunk"        -> Target.Run { (u, _,  r) => sellJunk(u, r) },
      "JunkSettings"    -> Target.Run { (u, _,  r) => showJunkSettings(u, r) },
      "JunkRarity"      -> Target.Run { (u, ua, r) => toggleJunkRarity(u, ua, r) },
      "JunkPassives"    -> Target.Run { (u, _,  r) => updateJunkSettings(u, r)(s => s.copy(passives = !s.passives)) },
      "JunkActives"     -> Target.Run { (u, _,  r) => updateJunkSettings(u, r)(s => s.copy(actives = !s.actives)) },
      "JunkTrophies"    -> Target.Run { (u, _,  r) => updateJunkSettings(u, r)(s => s.copy(trophies = !s.trophies)) },
      "BackFromJunk"    -> Target.Run { (u, _,  r) => showMenu(u, r).as(StateType.Merchant) },
      "SellListPrev"    -> Target.Run { (u, _,  r) => navigateSell(u, r, -1) },
      "SellListNext"    -> Target.Run { (u, _,  r) => navigateSell(u, r, +1) },
      "ConfirmSellItem" -> Target.Run { (u, _,  r) => doSell(u, r) },
      "CancelSellItem"  -> Target.Run { (u, _,  r) => currentSellPage(u).flatMap(p => showSellList(u, r, p)) },
      "BackFromSell"    -> Target.Run { (u, _,  r) => showMenu(u, r).as(StateType.Merchant) },
      "OpenCharacter"   -> Target.Run { (u, _,  _) => CharacterMenu.open(heroDao, u.userId, StateType.Merchant) },
      "Back"            -> Target.Goto(StateType.MarketSquare)
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = Set(StateType.MarketSquare, StateType.Merchant, StateType.HeroStats)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- nowMs
      data <- loadOrInit(user, now)
      hero <- getHero(user)
      _    <- renderer.show(user, menuScreen(data, hero))
    } yield ()

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Покупка ───────────────────────────────────────────────────────────────

  private def confirmBuy(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      data <- loadOrInit(user, now)
      idx   = payloadIdx(ua)
      _ <- data.items.lift(idx.getOrElse(-1)) match {
             case Some(mi) if !mi.bought =>
               val choices = List(
                 content.choice("ConfirmBuy", "merchant.confirmYes").copy(data = Map("idx" -> idx.get.toString)),
                 content.choice("CancelBuy", "merchant.confirmNo")
               )
               renderer.show(user, Screen(
                 content.format("merchant.confirmBuy", "name" -> mi.item.name, "price" -> mi.price.toString),
                 choices, inline = true))
             case _ => showMenu(user, renderer)
           }
    } yield StateType.Merchant

  private def doBuy(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      data <- loadOrInit(user, now)
      idx   = payloadIdx(ua).getOrElse(-1)
      _ <- data.items.lift(idx) match {
             case Some(mi) if !mi.bought =>
               if (hero.silver < mi.price)
                 renderer.show(user, Screen(content.text("merchant.notEnoughSilver"), Nil)) *> showMenu(user, renderer)
               else
                 for {
                   persisted <- itemRepo.persist(hero.id, mi.item)
                   added     <- inventoryRepo.addItem(hero.id, persisted).as(true).catchAll(_ => ZIO.succeed(false))
                   _ <- if (added) {
                          val newData = data.copy(items = data.items.updated(idx, mi.copy(bought = true)))
                          heroDao.updateSilver(user.userId, hero.silver - mi.price) *>
                            heroDao.writeMerchantData(user.userId, newData.asJson) *>
                            InventoryFeedback.freeSlotsLine(inventoryRepo, content, hero.id).flatMap(slots =>
                              renderer.show(user, Screen(content.format("merchant.bought", "name" -> mi.item.name) + "\n" + slots, Nil))) *>
                            showMenu(user, renderer)
                        } else
                          renderer.show(user, Screen(content.text("common.inventoryFull"), Nil)) *> showMenu(user, renderer)
                 } yield ()
             case _ => showMenu(user, renderer)
           }
    } yield StateType.Merchant

  // ── Обновление стока ────────────────────────────────────────────────────────

  private def refresh(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      data <- loadOrInit(user, now)
      hero <- getHero(user)
      _ <- if (now - data.refreshedAt < RefreshCooldownMs) {
             val mins = ((RefreshCooldownMs - (now - data.refreshedAt)) / 60000L).max(1L)
             renderer.show(user, Screen(content.format("merchant.refreshCooldown", "mins" -> mins.toString), Nil)) *>
               renderer.show(user, menuScreen(data, hero))
           } else
             regenerate(user, now, data.junkSale).flatMap(d => renderer.show(user, menuScreen(d, hero)))
    } yield StateType.Merchant

  // ── Продажа ─────────────────────────────────────────────────────────────────

  private def showSellList(user: User, renderer: Renderer, page: Int): Task[StateType] =
    for {
      hero  <- getHero(user)
      items <- inventoryItems(hero)
      _ <- if (items.isEmpty)
             renderer.show(user, Screen(content.text("merchant.sellEmpty"),
               List(content.choice("BackFromSell", "merchant.sellBackLabel"))))
           else {
             val (pageItems, totalPages, p) = ItemMenu.page(items, page)
             val header  = content.format("merchant.sellHeader",
               "page"  -> (p + 1).toString,
               "total" -> totalPages.toString,
               "silver" -> hero.silver.toString)
             val btns    = ItemMenu.itemButtons(pageItems, SellItemPrefix)
             val nav     = sellNavRow(p, totalPages)
             heroDao.writeSceneData(user.userId, SellScene(page = p).asJson) *>
               renderer.show(user, Screen(header, btns ++ nav))
           }
    } yield StateType.Merchant

  private def showSellConfirm(user: User, itemId: Long, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      items <- inventoryItems(hero)
      _ <- items.find(_.id == itemId) match {
        case None => showSellList(user, renderer, 0).unit
        case Some(item) =>
          // Материалы Ришелье выкупает золотом — цену показываем в дублонах.
          val gold = doubloonPrice(item)
          val text =
            if (gold > 0) s"${itemDesc(item)}\n🟡 Цена продажи: $gold"
            else s"${itemDesc(item)}\n🪙 Цена продажи: ${sellPrice(item)}"
          val choices = List(
            content.choice("ConfirmSellItem", "merchant.sellItemLabel").copy(data = Map("id" -> itemId.toString), row = Some(0)),
            content.choice("CancelSellItem",  "merchant.sellBackLabel").copy(row = Some(1))
          )
          currentSellPage(user).flatMap(p => heroDao.writeSceneData(user.userId,
            SellScene(page = p, selectedId = Some(itemId)).asJson)) *>
            renderer.show(user, Screen(text, choices, inline = true))
      }
    } yield StateType.Merchant

  private def navigateSell(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    currentSellPage(user).flatMap(p => showSellList(user, renderer, p + delta))

  private def doSell(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero   <- getHero(user)
      items  <- inventoryItems(hero)
      scene  <- currentSellScene(user)
      itemId  = scene.selectedId.getOrElse(-1L)
      _ <- items.find(_.id == itemId) match {
        case Some(item) =>
          val gold  = doubloonPrice(item)
          val price = if (gold > 0) gold else sellPrice(item)
          val pay =
            if (gold > 0) heroDao.updateDoubloons(user.userId, hero.doubloons + gold)
            else heroDao.updateSilver(user.userId, hero.silver + price)
          val line = if (gold > 0) "merchant.soldDoubloons" else "merchant.sold"
          inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
            pay *>
            renderer.show(user, Screen(
              content.format(line, "name" -> item.name, "price" -> price.toString), Nil))
        case None => ZIO.unit
      }
      _ <- showSellList(user, renderer, scene.page).unit
    } yield StateType.Merchant

  /** Быстрая продажа всего «хлама» — по настройке игрока (см. [[JunkSaleSettings]],
    * по умолчанию Серая и Белая редкости). Трофеи и камни не продаются никогда. */
  private def sellJunk(user: User, renderer: Renderer): Task[StateType] =
    for {
      now   <- nowMs
      data  <- loadOrInit(user, now)
      hero  <- getHero(user)
      items <- inventoryItems(hero)
      junk   = items.filter(isJunk(_, data.junkSettings))
      total  = junk.map(sellPrice).sum
      _ <- if (junk.isEmpty)
             renderer.show(user, Screen(content.text("merchant.sellJunkEmpty"), Nil)) *> showMenu(user, renderer)
           else
             inventoryRepo.removeItems(junk.map(_.id).toSet, hero.id).mapError(e => new Throwable(e.toString)) *>
               heroDao.updateSilver(user.userId, hero.silver + total) *>
               renderer.show(user, Screen(content.format("merchant.sellJunkDone",
                 "count" -> junk.size.toString, "silver" -> total.toString), Nil)) *>
               showMenu(user, renderer)
    } yield StateType.Merchant

  // ── Настройка автопродажи ───────────────────────────────────────────────────

  private def showJunkSettings(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      data <- loadOrInit(user, now)
      _    <- renderer.show(user, junkSettingsScreen(data.junkSettings))
    } yield StateType.Merchant

  private def junkSettingsScreen(s: JunkSaleSettings): Screen = {
    def state(on: Boolean) = content.text(if (on) "merchant.junk.on" else "merchant.junk.off")
    def color(on: Boolean) = if (on) ChoiceColor.Positive else ChoiceColor.Negative

    val rarityButtons = JunkRarityGroups.zipWithIndex.map { case (g, i) =>
      val on = s.groupOn(g)
      Choice(
        id    = "JunkRarity",
        label = content.format("merchant.junk.rarity", "emoji" -> g.emoji, "state" -> state(on)),
        color = color(on),
        data  = Map("g" -> g.id),
        row   = Some(i)
      )
    }
    // Переключатели, не привязанные к редкости: защита способностей и трофеи.
    val flags = List(
      ("JunkPassives", "merchant.junk.passives", s.passives),
      ("JunkActives",  "merchant.junk.actives",  s.actives),
      ("JunkTrophies", "merchant.junk.trophies", s.trophies)
    )
    val flagButtons = flags.zipWithIndex.map { case ((id, key, on), i) =>
      Choice(id, content.format(key, "state" -> state(on)), color = color(on),
        row = Some(JunkRarityGroups.size + i))
    }
    Screen(
      content.text("merchant.junk.header"),
      rarityButtons ++ flagButtons :+
        content.choice("BackFromJunk", "merchant.junk.back")
          .copy(row = Some(JunkRarityGroups.size + flags.size))
    )
  }

  private def toggleJunkRarity(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    payloadStr(ua, "g").flatMap(id => JunkRarityGroups.find(_.id == id)) match {
      case Some(group) => updateJunkSettings(user, renderer)(_.toggleGroup(group))
      case None        => showJunkSettings(user, renderer)
    }

  // Применяет правку настройки, сохраняет её в merchant_data и перерисовывает экран.
  private def updateJunkSettings(user: User, renderer: Renderer)(
      f: JunkSaleSettings => JunkSaleSettings
  ): Task[StateType] =
    for {
      now      <- nowMs
      data     <- loadOrInit(user, now)
      updated   = f(data.junkSettings)
      _        <- heroDao.writeMerchantData(user.userId, data.copy(junkSale = Some(updated)).asJson)
      _        <- renderer.show(user, junkSettingsScreen(updated))
    } yield StateType.Merchant

  private def sellNavRow(page: Int, totalPages: Int): List[Choice] = {
    val row = ItemMenu.NavRow
    List(
      Some(content.choice("BackFromSell", "merchant.sellBackLabel").copy(row = Some(row))),
      Option.when(page > 0)(content.choice("SellListPrev", "common.prev").copy(row = Some(row))),
      Option.when(page < totalPages - 1)(content.choice("SellListNext", "common.next").copy(row = Some(row)))
    ).flatten
  }

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseFallbackAction(ua.payload) match {
      case Some(a) if a.startsWith(SellItemPrefix) =>
        a.drop(SellItemPrefix.length).toLongOption.fold[Task[StateType]](showMenu(user, renderer).as(StateType.Merchant))(
          showSellConfirm(user, _, renderer))
      case _ => showMenu(user, renderer).as(StateType.Merchant)
    }

  private def parseFallbackAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  // ── Экраны и хелперы ────────────────────────────────────────────────────────

  private def showMenu(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- nowMs
      data <- loadOrInit(user, now)
      hero <- getHero(user)
      _    <- renderer.show(user, menuScreen(data, hero))
    } yield ()

  private def menuScreen(data: MerchantData, hero: Hero): Screen = {
    val lines = data.items.zipWithIndex.map { case (mi, i) =>
      if (mi.bought) content.format("merchant.boughtLine", "n" -> (i + 1).toString, "name" -> mi.item.name)
      else saleLine(mi, i, hero)
    }
    val text =
      if (data.items.nonEmpty && data.items.forall(_.bought)) content.text("merchant.soldOut")
      else content.text("merchant.richelieu.header") + "\n\n" + lines.mkString("\n\n")
    val buyButtons = data.items.zipWithIndex.collect {
      case (mi, i) if !mi.bought =>
        content.choice("Buy", "merchant.buyLabel", "n" -> (i + 1).toString).copy(data = Map("idx" -> i.toString))
    }
    val choices = buyButtons ++ List(
      content.choice("Refresh",       "merchant.refreshLabel"),
      content.choice("Sell",          "merchant.sellLabel"),
      content.choice("SellJunk",      "merchant.sellJunkLabel"),
      content.choice("JunkSettings",  "merchant.junk.settingsLabel"),
      content.choice("OpenCharacter", "common.character"),
      content.choice("Back",          "merchant.backLabel")
    )
    Screen(text, choices)
  }

  /** Строка продаваемого предмета + сравнение с надетым в том же слоте — тот же
   *  формат и разделитель, что при находке/дропе ([[Item.ComparisonSeparator]]).
   *  Если слот пуст — сравнивать не с чем, показываем только предмет. */
  private def saleLine(mi: MerchantItem, i: Int, hero: Hero): String = {
    val base     = s"${i + 1}) ${itemDesc(mi.item)}\n🪙 Цена: ${mi.price}"
    val equipped = hero.equipment.equippedFor(mi.item.itemType).filter(_.itemType != ItemType.NoItem)
    if (equipped.isEmpty) base
    else base + "\n" + Item.ComparisonSeparator + "\n" + equipped.map(_.equippedComparison("Надето")).mkString("\n")
  }

  private def itemDesc(item: Item): String = {
    val lines = item.statsLines
    val tail  = if (lines.isEmpty) "" else "\n" + lines.mkString("\n")
    s"${item.displayTitle}$tail"
  }

  private def loadOrInit(user: User, now: Long): Task[MerchantData] =
    heroDao.readMerchantData(user.userId).flatMap {
      case Some(json) => ZIO.fromOption(json.as[MerchantData].toOption).orElse(regenerate(user, now))
      case None       => regenerate(user, now)
    }

  // Обновление стока не должно сбрасывать настройку автопродажи, поэтому она
  // прокидывается в новый MerchantData (при самом первом заходе её ещё нет).
  private def regenerate(user: User, now: Long, junkSale: Option[JunkSaleSettings] = None): Task[MerchantData] =
    for {
      hero <- getHero(user)
      seed <- Random.nextLong
      items = rollStock(hero.lvl, Rng(seed))
      data  = MerchantData(items, now, junkSale)
      _    <- heroDao.writeMerchantData(user.userId, data.asJson)
    } yield data

  private def rollStock(heroLvl: Long, rng0: Rng): List[MerchantItem] =
    (0 until 3).foldLeft((List.empty[MerchantItem], rng0)) { case ((acc, rng), _) =>
      val (rarity, r1) = rollRarity(rng)
      val (item, r2)   = ItemGenerator.createItemAtLevel(heroLvl, rarity, r1)
      val (price, r3)  = rollPrice(heroLvl, rarity, r2)
      (acc :+ MerchantItem(item, price, bought = false), r3)
    }._1

  // Зелёная 50% · Синяя 50% (Ришелье белым не торгует — их доля ушла в синие)
  private def rollRarity(rng: Rng): (Rarity, Rng) = {
    val (roll, next) = rng.between(0L, 100L)
    val rarity = if (roll < 50) Rarity.Green else Rarity.Blue
    (rarity, next)
  }

  // (lvl + 20) × 4 × R(редкость) ±20%
  private def rollPrice(heroLvl: Long, rarity: Rarity, rng: Rng): (Long, Rng) = {
    val base        = (heroLvl + 20) * 4 * rarity.factorR
    val (pct, next) = rng.between(-20L, 21L)
    val price       = (base + base * pct / 100.0).toLong.max(1L)
    (price, next)
  }

  // (lvl(снаряжения) + 5) × 1.2 × R(редкость) — продажа дешевле покупки
  private def sellPrice(item: Item): Long =
    ((item.lvl + 5) * 1.2 * item.rarity.factorR).toLong.max(1L)

  /** Сколько дублонов Ришелье платит за предмет; 0 — обычная продажа за серебро.
    * Пока золотом он выкупает только материалы (см. [[MaterialKind.doubloonPrice]]). */
  private def doubloonPrice(item: Item): Long =
    item.material.map(_.doubloonPrice).getOrElse(0L)

  private def inventoryItems(hero: Hero): Task[List[Item]] =
    inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString)).map(_.items.data)

  private def currentSellPage(user: User): Task[Int] = currentSellScene(user).map(_.page)

  private def currentSellScene(user: User): Task[SellScene] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[SellScene].toOption).getOrElse(SellScene()))

  private def payloadIdx(ua: UserAction): Option[Int] =
    payloadStr(ua, "idx").flatMap(_.toIntOption)

  private def payloadStr(ua: UserAction, key: String): Option[String] =
    ua.payload.flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get(key)))

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object MerchantState {
  val RefreshCooldownMs: Long = 60L * 60L * 1000L // раз в час

  final case class MerchantItem(item: Item, price: Long, bought: Boolean)
  object MerchantItem {
    implicit val encoder: Encoder[MerchantItem] = deriveEncoder
    implicit val decoder: Decoder[MerchantItem] = deriveDecoder
  }

  /** Настройка автопродажи «хлама»: какие редкости уходят по кнопке «Продать
    * хлам», трогать ли предметы со способностями и продавать ли трофеи. По
    * умолчанию — прежнее поведение: серое и белое, способности не берегутся,
    * трофеи не продаются (они нужны для заданий и репутации). */
  final case class JunkSaleSettings(
    rarities: Set[Rarity] = Set(Rarity.Gray, Rarity.White),
    passives: Boolean     = true,
    actives:  Boolean     = true,
    trophies: Boolean     = false
  ) {

    /** Группа включена, если продаются все её редкости (переключатель ставит их
      * только целиком, так что промежуточного состояния не бывает). */
    def groupOn(group: JunkRarityGroup): Boolean = group.rarities.forall(rarities.contains)

    def toggleGroup(group: JunkRarityGroup): JunkSaleSettings =
      if (groupOn(group)) copy(rarities = rarities -- group.rarities)
      else copy(rarities = rarities ++ group.rarities)
  }
  object JunkSaleSettings {
    implicit val encoder: Encoder[JunkSaleSettings] = deriveEncoder
    implicit val decoder: Decoder[JunkSaleSettings] = deriveDecoder

    val default: JunkSaleSettings = JunkSaleSettings()
  }

  /** Переключатель редкости в настройке. Фиолетовая группа накрывает сразу
    * Purple и Violet: у них общий значок 🟣, общий множитель цены и общий пул
    * названий — игрок их нигде не различает, поэтому и переключатель один. */
  final case class JunkRarityGroup(id: String, rarities: List[Rarity]) {
    def emoji: String = rarities.head.emoji
  }

  val JunkRarityGroups: List[JunkRarityGroup] = List(
    JunkRarityGroup("Gray",   List(Rarity.Gray)),
    JunkRarityGroup("White",  List(Rarity.White)),
    JunkRarityGroup("Green",  List(Rarity.Green)),
    JunkRarityGroup("Blue",   List(Rarity.Blue)),
    JunkRarityGroup("Purple", List(Rarity.Purple, Rarity.Violet)),
    JunkRarityGroup("Orange", List(Rarity.Orange))
  )

  /** Пойдёт ли предмет под нож при «Продать хлам». Камни-усилители не продаются
    * никогда, независимо от настроек. Переключатели способностей — именно
    * защита: выключенный «Пассивные способности» уводит предмет из продажи,
    * даже если его редкость включена.
    *
    * Трофеи идут по своему переключателю и НЕ смотрят на редкости: у них она
    * формально всегда Серая (как и у камней), так что фильтр по редкости для
    * них ничего осмысленного не значил бы. Способности у трофеев не бывают,
    * поэтому их защита трофеев тоже не касается. */
  def isJunk(item: Item, s: JunkSaleSettings): Boolean =
    if (item.itemType == ItemType.Trophy) s.trophies
    else
      // Камни и материалы крафта не хлам никогда: их редкость ничего не говорит
      // о ценности (вечно огненное железо — серое, а стоит дороже иной вещи).
      item.itemType != ItemType.Gem &&
        item.itemType != ItemType.Material &&
        s.rarities.contains(item.rarity) &&
        (s.passives || item.passive.isEmpty) &&
        (s.actives || item.activeSkill.isEmpty)

  final case class MerchantData(
    items: List[MerchantItem],
    refreshedAt: Long,
    // Option, а не значение с дефолтом: у уже сохранённых лавок этого поля в
    // JSON нет, а circe без него не соберёт объект и сбросил бы весь сток.
    junkSale: Option[JunkSaleSettings] = None
  ) {
    def junkSettings: JunkSaleSettings = junkSale.getOrElse(JunkSaleSettings.default)
  }
  object MerchantData {
    implicit val encoder: Encoder[MerchantData] = deriveEncoder
    implicit val decoder: Decoder[MerchantData] = deriveDecoder
  }

  final case class SellScene(page: Int = 0, selectedId: Option[Long] = None)
  object SellScene {
    implicit val encoder: Encoder[SellScene] = deriveEncoder
    implicit val decoder: Decoder[SellScene] = deriveDecoder
  }

  val SellItemPrefix = "SellItem_"
}
