package pangea.service.state.states.merchant

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.ItemGenerator
import pangea.model.hero.Hero
import pangea.model.item.{Item, Rarity}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.states.merchant.MerchantState._
import pangea.service.state.{CharacterMenu, InventoryFeedback, State, UserAction}
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
      "Buy"          -> Target.Run { (u, ua, r) => confirmBuy(u, ua, r) },
      "ConfirmBuy"   -> Target.Run { (u, ua, r) => doBuy(u, ua, r) },
      "CancelBuy"    -> Target.Run { (u, _,  r) => showMenu(u, r).as(StateType.Merchant) },
      "Refresh"      -> Target.Run { (u, _,  r) => refresh(u, r) },
      "Sell"         -> Target.Run { (u, _,  r) => showSell(u, r, 0) },
      "SellPrev"     -> Target.Run { (u, _,  r) => navigateSell(u, r, -1) },
      "SellNext"     -> Target.Run { (u, _,  r) => navigateSell(u, r, +1) },
      "SellItem"     -> Target.Run { (u, _,  r) => doSell(u, r) },
      "BackFromSell" -> Target.Run { (u, _,  r) => showMenu(u, r).as(StateType.Merchant) },
      "OpenCharacter"-> Target.Run { (u, _,  _) => CharacterMenu.open(heroDao, u.userId, StateType.Merchant) },
      "Back"         -> Target.Goto(StateType.GlobalMap)
    ),
    fallback = Target.Run { (u, _, r) => showMenu(u, r).as(StateType.Merchant) }
  )

  override def targetStates: Set[StateType] = Set(StateType.GlobalMap, StateType.Merchant, StateType.HeroStats)

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- nowMs
      data <- loadOrInit(user, now)
      _    <- renderer.show(user, menuScreen(data))
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
             case _ => renderer.show(user, menuScreen(data))
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
               if (hero.gold < mi.price)
                 renderer.show(user, Screen(content.text("merchant.notEnoughGold"), Nil)) *> showMenu(user, renderer)
               else
                 for {
                   persisted <- itemRepo.persist(hero.id, mi.item)
                   added     <- inventoryRepo.addItem(hero.id, persisted).as(true).catchAll(_ => ZIO.succeed(false))
                   _ <- if (added) {
                          val newData = data.copy(items = data.items.updated(idx, mi.copy(bought = true)))
                          heroDao.updateGold(user.userId, hero.gold - mi.price) *>
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
      _ <- if (now - data.refreshedAt < RefreshCooldownMs) {
             val mins = ((RefreshCooldownMs - (now - data.refreshedAt)) / 60000L).max(1L)
             renderer.show(user, Screen(content.format("merchant.refreshCooldown", "mins" -> mins.toString), Nil)) *>
               renderer.show(user, menuScreen(data))
           } else
             regenerate(user, now).flatMap(d => renderer.show(user, menuScreen(d)))
    } yield StateType.Merchant

  // ── Продажа ─────────────────────────────────────────────────────────────────

  private def showSell(user: User, renderer: Renderer, page: Int): Task[StateType] =
    for {
      hero  <- getHero(user)
      items <- inventoryItems(hero)
      _ <- if (items.isEmpty)
             renderer.show(user, Screen(content.text("merchant.sellEmpty"),
               List(content.choice("BackFromSell", "merchant.sellBackLabel"))))
           else {
             val p = page.max(0).min(items.size - 1)
             heroDao.writeSceneData(user.userId, SellPage(p).asJson) *>
               renderer.show(user, sellScreen(hero, items, p))
           }
    } yield StateType.Merchant

  private def navigateSell(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    currentSellPage(user).flatMap(p => showSell(user, renderer, p + delta))

  private def doSell(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero  <- getHero(user)
      items <- inventoryItems(hero)
      page  <- currentSellPage(user)
      _ <- if (items.nonEmpty && page < items.size) {
             val item  = items(page)
             val price = sellPrice(item)
             inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
               heroDao.updateGold(user.userId, hero.gold + price) *>
               renderer.show(user, Screen(
                 content.format("merchant.sold", "name" -> item.name, "price" -> price.toString), Nil)) *>
               showSell(user, renderer, page).unit
           } else showSell(user, renderer, page).unit
    } yield StateType.Merchant

  // ── Экраны и хелперы ────────────────────────────────────────────────────────

  private def showMenu(user: User, renderer: Renderer): Task[Unit] =
    for {
      now  <- nowMs
      data <- loadOrInit(user, now)
      _    <- renderer.show(user, menuScreen(data))
    } yield ()

  private def menuScreen(data: MerchantData): Screen = {
    val lines = data.items.zipWithIndex.map { case (mi, i) =>
      if (mi.bought) content.format("merchant.boughtLine", "n" -> (i + 1).toString, "name" -> mi.item.name)
      else s"${i + 1}) ${itemDesc(mi.item)}\n💰 Цена: ${mi.price}"
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
      content.choice("OpenCharacter", "common.character"),
      content.choice("Back",          "merchant.backLabel")
    )
    Screen(text, choices)
  }

  private def sellScreen(hero: Hero, items: List[Item], page: Int): Screen = {
    val item  = items(page)
    val price = sellPrice(item)
    val text  = content.format("merchant.sellHeader",
      "page"  -> (page + 1).toString,
      "total" -> items.size.toString,
      "gold"  -> hero.gold.toString) + s"\n\n${itemDesc(item)}\n💰 Цена продажи: $price"
    val choices = List(
      Option.when(page > 0)(content.choice("SellPrev", "common.prev")),
      Some(content.choice("SellItem", "merchant.sellItemLabel")),
      Option.when(page < items.size - 1)(content.choice("SellNext", "common.next")),
      Some(content.choice("BackFromSell", "merchant.sellBackLabel"))
    ).flatten
    Screen(text, choices)
  }

  private def itemDesc(item: Item): String = {
    val stats = List(
      Option.when(item.attack > 0)(s"Атака +${item.attack}"),
      Option.when(item.accuracy > 0)(s"Точность +${item.accuracy}"),
      Option.when(item.concentration > 0)(s"Концентрация +${item.concentration}"),
      Option.when(item.armor > 0)(s"Броня +${item.armor}"),
      Option.when(item.defence > 0)(s"Защита +${item.defence}"),
      Option.when(item.evasion > 0)(s"Уклонение +${item.evasion}"),
      Option.when(item.hp > 0)(s"HP +${item.hp}")
    ).flatten
    val tail = if (stats.isEmpty) "" else "\n" + stats.mkString(", ")
    s"${item.name} [${item.rarity}] Ур.${item.lvl}$tail"
  }

  private def loadOrInit(user: User, now: Long): Task[MerchantData] =
    heroDao.readMerchantData(user.userId).flatMap {
      case Some(json) => ZIO.fromOption(json.as[MerchantData].toOption).orElse(regenerate(user, now))
      case None       => regenerate(user, now)
    }

  private def regenerate(user: User, now: Long): Task[MerchantData] =
    for {
      hero <- getHero(user)
      seed <- Random.nextLong
      items = rollStock(hero.lvl, Rng(seed))
      data  = MerchantData(items, now)
      _    <- heroDao.writeMerchantData(user.userId, data.asJson)
    } yield data

  private def rollStock(heroLvl: Long, rng0: Rng): List[MerchantItem] =
    (0 until 3).foldLeft((List.empty[MerchantItem], rng0)) { case ((acc, rng), _) =>
      val (rarity, r1) = rollRarity(rng)
      val (item, r2)   = ItemGenerator.createItemAtLevel(heroLvl, rarity, r1)
      val (price, r3)  = rollPrice(heroLvl, rarity, r2)
      (acc :+ MerchantItem(item, price, bought = false), r3)
    }._1

  // Белая 20% · Зелёная 30% · Синяя 30% · Фиолетовая 20%
  private def rollRarity(rng: Rng): (Rarity, Rng) = {
    val (roll, next) = rng.between(0L, 100L)
    val rarity =
      if (roll < 20) Rarity.White
      else if (roll < 50) Rarity.Green
      else if (roll < 80) Rarity.Blue
      else Rarity.Purple
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

  private def inventoryItems(hero: Hero): Task[List[Item]] =
    inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString)).map(_.items.data)

  private def currentSellPage(user: User): Task[Int] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[SellPage].toOption).map(_.page).getOrElse(0))

  private def payloadIdx(ua: UserAction): Option[Int] =
    ua.payload
      .flatMap(p => io.circe.jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("idx")))
      .flatMap(_.toIntOption)

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

  final case class MerchantData(items: List[MerchantItem], refreshedAt: Long)
  object MerchantData {
    implicit val encoder: Encoder[MerchantData] = deriveEncoder
    implicit val decoder: Decoder[MerchantData] = deriveDecoder
  }

  final case class SellPage(page: Int)
  object SellPage {
    implicit val encoder: Encoder[SellPage] = deriveEncoder
    implicit val decoder: Decoder[SellPage] = deriveDecoder
  }
}
