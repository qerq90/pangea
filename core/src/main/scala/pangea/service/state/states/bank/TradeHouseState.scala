package pangea.service.state.states.bank

import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.bank.BankVault
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.{ReceiptEmail, User}
import pangea.repository.bank.BankRepository
import pangea.repository.inventory.InventoryRepository
import pangea.service.parcel.Parcels
import pangea.service.purse.Purse
import pangea.model.item.{Item, MaterialKind}
import pangea.model.quest.{DailyNpc, NpcQuest}
import pangea.service.state.{CityExit, DailyDialog, NpcQuestDialog, State, UserAction}
import zio.{Task, ZIO}

/** Торговый дом на Торговой площади: банкир Рахадим продаёт ячейки хранилища, меняет
 *  дублоны (пока нет) и держит аукцион (пока нет). Каждая ячейка даёт место под
 *  вещи и серебро, первая стоит [[BankVault.FirstCellPrice]], каждая следующая —
 *  на [[BankVault.CellPriceStep]] дороже предыдущей. */
case class TradeHouseState(
  heroDao:       HeroDao,
  bankRepo:      BankRepository,
  parcels:       Parcels,
  inventoryRepo: InventoryRepository,
  content:       SceneContent
) extends State {

  /** Ячейку тоже можно оплатить из уже выкупленных: сперва своё, потом банк. */
  private val purse = Purse(heroDao, Some(bankRepo))

  /** «Залог доверия»: кусок с элементаля — и задаток на ячейку. */
  private val quest = NpcQuestDialog(heroDao, content, NpcQuest.Rakhadim, "Rakh")

  /** Счётная книга Рахадима: сегодняшнее поручение банкира. */
  private val daily = DailyDialog(heroDao, content, DailyNpc.Rakhadim, "Rakhadim", Some(inventoryRepo))

  private val branch = new Branch(
    routes = Map(
      "TradeHouse"      -> Target.Run { (u, _, r) => showMenu(u, r).as(StateType.TradeHouse) },
      "BuyCell"         -> Target.Run { (u, _, r) => confirmCell(u, r).as(StateType.TradeHouse) },
      "BuyCellConfirm"  -> Target.Run { (u, _, r) => buyCell(u, r).as(StateType.TradeHouse) },
      "BuyDoubloons"    -> Target.Run { (u, _, r) => showDoubloons(u, r).as(StateType.TradeHouse) },
      "DepositInterest" -> Target.Run { (u, _, r) => showPage(u, r, "bank.tradeHouse.interest").as(StateType.TradeHouse) },
      "MyVault"         -> Target.Goto(StateType.BankVault),
      "Auction"         -> Target.Goto(StateType.Auction),
      "FetShop"         -> Target.Goto(StateType.FetShop),
      "Mail"            -> Target.Goto(StateType.Mail),
      "LeaveTradeHouse" -> Target.Goto(StateType.MarketSquare),
      quest.questAction   -> Target.Run { (u, _, r) => questTalk(u, r) },
      quest.acceptAction  -> Target.Run { (u, _, r) => quest.accept(u, r) *> showMenu(u, r).as(StateType.TradeHouse) },
      quest.declineAction -> Target.Run { (u, _, r) => showMenu(u, r).as(StateType.TradeHouse) },
      daily.openAction  -> Target.Run { (u, _, r) => showDaily(u, r) },
      daily.takeAction  -> Target.Run { (u, _, r) => takeDaily(u, r) },
      daily.giveAction  -> Target.Run { (u, _, r) => giveDaily(u, r) },
      daily.handAction  -> Target.Run { (u, _, r) => handDaily(u, r) },
      "RakhadimDailyBack" -> Target.Run { (u, _, r) => showMenu(u, r).as(StateType.TradeHouse) },
      CityExit.route
    ),
    fallback = Target.Run { (u, ua, r) => handleText(u, ua, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(content.text("bank.tradeHouse.greeting"), Nil)) *> showMenu(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // --- Меню Рахадима ---

  // ── Счётная книга ─────────────────────────────────────────────────────────

  private def showDaily(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      task <- daily.today(hero, now)
      _    <- daily.show(user, task, now, renderer)
    } yield StateType.TradeHouse

  private def takeDaily(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      _    <- daily.take(user, hero, now, renderer)
    } yield StateType.TradeHouse

  private def giveDaily(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      _    <- daily.give(user, hero, now, renderer)
    } yield StateType.TradeHouse

  /** Банкир платит тем, чем и живёт, — дублонами. Ставка от уровня не зависит:
    * за лот одна, за принесённое другая. */
  private def handDaily(user: User, renderer: Renderer): Task[StateType] =
    for {
      now  <- nowMs
      hero <- getHero(user)
      paid <- daily.hand(user, hero, now)
      _    <- ZIO.foreachDiscard(paid) { case (task, exp) =>
                val pay = task.kind.doubloons
                heroDao.updateDoubloons(user.userId, hero.doubloons + pay) *>
                  renderer.show(user, Screen(content.format("daily.rakhadim.reward",
                    "doubloons" -> pay.toString, "exp" -> exp.toString), Nil))
              }
      _    <- showMenu(user, renderer)
    } yield StateType.TradeHouse

  // ── «Залог доверия» ─────────────────────────────────────────

  private def questTalk(user: User, renderer: Renderer): Task[StateType] =
    quest.load(user).flatMap { quests =>
      if (quests.isDone(NpcQuest.Rakhadim)) showMenu(user, renderer)
      else if (!quests.isTaken(NpcQuest.Rakhadim)) quest.offer(user, renderer, quest.text("intro"))
      else turnIn(user, renderer)
    }.as(StateType.TradeHouse)

  /** Сдача: кусок ложится на сукно, герой получает задаток и опыт. */
  private def turnIn(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      _ <- inv.items.data.find(TradeHouseState.elementalPiece) match {
        case None => renderer.show(user, Screen(quest.text("step1Fail"), Nil)) *> showMenu(user, renderer)
        case Some(piece) =>
          for {
            _    <- inventoryRepo.removeItem(piece.id, hero.id).mapError(asThrowable)
            _    <- heroDao.updateSilver(user.userId, hero.silver + TradeHouseState.QuestSilver)
            done <- quest.complete(user, hero, identity)
            (_, expLine) = done
            _    <- renderer.show(user, Screen(quest.text("outro"), Nil))
            _    <- renderer.show(user, Screen(quest.format("reward",
                      "silver" -> TradeHouseState.QuestSilver.toString, "exp" -> expLine), Nil))
            _    <- showMenu(user, renderer)
          } yield ()
      }
    } yield ()

  private def showMenu(user: User, renderer: Renderer): Task[Unit] =
    for {
      // В меню возвращаются и со страницы дублонов — адреса там больше не ждём.
      _     <- resetScene(user)
      hero  <- getHero(user)
      vault <- bankRepo.get(hero.id).mapError(asThrowable)
      // Почта видна, только если на ней что-то лежит.
      mail  <- parcels.waitingCount(hero.id).orElse(ZIO.succeed(0L))
      now   <- nowMs
      // Счётная книга: у банкира на каждый день своя просьба.
      task  <- daily.today(hero, now)
      quests <- quest.load(user)
      _     <- renderer.show(user, menuScreen(vault, mail, daily.button(task), quest.button(quests)))
    } yield ()

  private def menuScreen(vault: BankVault, mail: Long = 0L, work: Option[Choice] = None,
                         story: Option[Choice] = None): Screen = {
    // Ряд на две кнопки: покупка ячейки и вход в хранилище (если есть что открывать).
    val buy = Choice("BuyCell",
      Choice.fit(content.format("bank.tradeHouse.buyCell", "price" -> vault.nextCellPrice.toString)),
      color = ChoiceColor.Positive, row = Some(0))
    val vaultBtn = Option.when(vault.open)(
      Choice("MyVault", content.text("bank.tradeHouse.myVault"), row = Some(0)))
    // Аукцион — только для тех, у кого есть ячейка: деньги и вещи там ходят
    // через неё.
    val auction = Option.when(vault.open)(
      Choice("Auction", content.text("bank.tradeHouse.auctionLabel"), row = Some(2)))
    val rest = List(
      Choice("BuyDoubloons",    content.text("bank.tradeHouse.doubloonsLabel"), row = Some(1)),
      Choice("DepositInterest", content.text("bank.tradeHouse.interestLabel"),  row = Some(1)),
      Choice("FetShop",         content.text("bank.tradeHouse.fetLabel"),       row = Some(2))
    ) ++ auction ++
      Option.when(mail > 0L)(Choice("Mail",
        content.format("bank.tradeHouse.mailLabel", "count" -> mail.toString),
        color = ChoiceColor.Positive, row = Some(3))) ++
      work.map(_.copy(row = Some(3))) ++
      story.map(_.copy(row = Some(3))) :+
      Choice("LeaveTradeHouse", content.text("bank.tradeHouse.leave"), color = ChoiceColor.Negative, row = Some(4)) :+
      CityExit.button(content, Some(4))
    val text = content.format("bank.tradeHouse.menu",
      "cells" -> vault.cells.toString, "price" -> vault.nextCellPrice.toString)
    Screen(text, (buy :: vaultBtn.toList) ++ rest)
  }

  /** Страница с одним текстом и «Назад» — заглушка про проценты по вкладу. */
  private def showPage(user: User, renderer: Renderer, key: String): Task[Unit] =
    renderer.show(user, Screen(content.text(key), backRow))

  private def backRow: List[Choice] =
    List(Choice("TradeHouse", content.text("bank.tradeHouse.back"), color = ChoiceColor.Negative, row = Some(0)))

  // --- Почта для чека ---

  /** Страница покупки дублонов: показывает записанный адрес для чека и ждёт
    * новый — свободным сообщением, без кнопок. */
  private def showDoubloons(user: User, renderer: Renderer): Task[Unit] =
    for {
      _     <- heroDao.writeSceneData(user.userId, TradeHouseState.TradeScene(emailPrompt = true).asJson)
      email <- heroDao.readReceiptEmail(user.userId)
      text   = content.format("bank.tradeHouse.doubloons",
                 "email" -> email.getOrElse(content.text("bank.tradeHouse.doubloonsNoEmail")))
      _     <- renderer.show(user, Screen(text, backRow))
    } yield ()

  /** Любой текст у Рахадима: на странице дублонов это адрес для чека, в
    * остальных случаях — просто возврат в меню. */
  private def handleText(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    readScene(user).flatMap { scene =>
      if (!scene.emailPrompt) showMenu(user, renderer)
      else ReceiptEmail.parse(ua.text) match {
        case Some(email) =>
          heroDao.writeReceiptEmail(user.userId, email) *>
            renderer.show(user, Screen(content.format("bank.tradeHouse.doubloonsSaved", "email" -> email), backRow))
        case None =>
          renderer.show(user, Screen(content.text("bank.tradeHouse.doubloonsBadEmail"), backRow))
      }
    }.as(StateType.TradeHouse)

  private def readScene(user: User): Task[TradeHouseState.TradeScene] =
    heroDao.readSceneData(user.userId)
      .map(_.flatMap(_.as[TradeHouseState.TradeScene].toOption).getOrElse(TradeHouseState.TradeScene()))

  private def resetScene(user: User): Task[Unit] =
    heroDao.writeSceneData(user.userId, Json.Null)

  // --- Покупка ячейки ---

  private def confirmCell(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero   <- getHero(user)
      vault  <- getVault(user)
      wallet <- purse.wallet(hero)
      price   = vault.nextCellPrice
      screen  = if (!wallet.canAfford(price))
                 Screen(content.format("bank.tradeHouse.cellNotEnough", "price" -> price.toString), backRow)
               else
                 Screen(content.format("bank.tradeHouse.cellConfirm",
                   "price"     -> price.toString,
                   "items"     -> BankVault.ItemsPerCell.toString,
                   "maxSilver" -> BankVault.SilverPerCell.toString),
                   List(
                     Choice("BuyCellConfirm", content.text("bank.tradeHouse.cellConfirmYes"), color = ChoiceColor.Positive, row = Some(0)),
                     Choice("TradeHouse",     content.text("bank.tradeHouse.cellConfirmNo"),  color = ChoiceColor.Negative, row = Some(0))
                   ))
      _ <- renderer.show(user, screen)
    } yield ()

  private def buyCell(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      vault <- getVault(user)
      price  = vault.nextCellPrice
      // Платим своим серебром, а нехватку добираем из уже купленных ячеек.
      paid  <- purse.charge(user.userId, hero, price)
      _ <- paid match {
        case None =>
          renderer.show(user, Screen(content.format("bank.tradeHouse.cellNotEnough", "price" -> price.toString), backRow))
        case Some(_) =>
          bankRepo.buyCell(hero.id).mapError(asThrowable).flatMap { grown =>
            renderer.show(user, Screen(content.format("bank.tradeHouse.cellBought",
              "cells"  -> grown.cells.toString,
              "items"  -> grown.maxItems.toString,
              "silver" -> grown.maxSilver.toString), Nil)) *>
              renderer.show(user, menuScreen(grown))
          }
      }
    } yield ()

  // --- Вспомогательное ---

  private def nowMs: Task[Long] =
    ZIO.clockWith(_.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def getVault(user: User): Task[BankVault] =
    getHero(user).flatMap(h => bankRepo.get(h.id).mapError(asThrowable))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object TradeHouseState {

  /** Задаток Рахадима — половина первой ячейки. */
  val QuestSilver: Long = BankVault.FirstCellPrice / 2L

  /** Чего банкир ждёт от игрока текстом: на странице дублонов — адрес для
    * чека. Живёт в `heroes.scene_data`, как и прочие режимы ввода. */
  case class TradeScene(emailPrompt: Boolean = false)
  object TradeScene {
    implicit val encoder: Encoder[TradeScene] = deriveEncoder
    implicit val decoder: Decoder[TradeScene] = deriveDecoder
  }

  /** Что остаётся от элементалей: железо, которое не остывает, и камень,
    * тянущий к себе другие. Банкир примет любой из двух. */
  def elementalPiece(item: Item): Boolean =
    item.material.exists(m => m == MaterialKind.EverburningIron || m == MaterialKind.MagicStone)
}
