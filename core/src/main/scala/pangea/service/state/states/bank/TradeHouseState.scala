package pangea.service.state.states.bank

import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.bank.BankVault
import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.bank.BankRepository
import pangea.service.parcel.Parcels
import pangea.service.purse.Purse
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

/** Торговый дом на Торговой площади: банкир Рахадим продаёт ячейки хранилища, меняет
 *  дублоны (пока нет) и держит аукцион (пока нет). Каждая ячейка даёт место под
 *  вещи и серебро, первая стоит [[BankVault.FirstCellPrice]], каждая следующая —
 *  на [[BankVault.CellPriceStep]] дороже предыдущей. */
case class TradeHouseState(
  heroDao:  HeroDao,
  bankRepo: BankRepository,
  parcels:  Parcels,
  content:  SceneContent
) extends State {

  /** Ячейку тоже можно оплатить из уже выкупленных: сперва своё, потом банк. */
  private val purse = Purse(heroDao, Some(bankRepo))

  private val branch = new Branch(
    routes = Map(
      "TradeHouse"      -> Target.Run { (u, _, r) => showMenu(u, r).as(StateType.TradeHouse) },
      "BuyCell"         -> Target.Run { (u, _, r) => confirmCell(u, r).as(StateType.TradeHouse) },
      "BuyCellConfirm"  -> Target.Run { (u, _, r) => buyCell(u, r).as(StateType.TradeHouse) },
      "BuyDoubloons"    -> Target.Run { (u, _, r) => showPage(u, r, "bank.tradeHouse.doubloons").as(StateType.TradeHouse) },
      "DepositInterest" -> Target.Run { (u, _, r) => showPage(u, r, "bank.tradeHouse.interest").as(StateType.TradeHouse) },
      "MyVault"         -> Target.Goto(StateType.BankVault),
      "Auction"         -> Target.Goto(StateType.Auction),
      "FetShop"         -> Target.Goto(StateType.FetShop),
      "Mail"            -> Target.Goto(StateType.Mail),
      "LeaveTradeHouse" -> Target.Goto(StateType.MarketSquare)
    ),
    fallback = Target.Run { (u, _, r) => showMenu(u, r).as(StateType.TradeHouse) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, Screen(content.text("bank.tradeHouse.greeting"), Nil)) *> showMenu(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // --- Меню Рахадима ---

  private def showMenu(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      vault <- bankRepo.get(hero.id).mapError(asThrowable)
      // Почта видна, только если на ней что-то лежит.
      mail  <- parcels.waitingCount(hero.id).orElse(ZIO.succeed(0L))
      _     <- renderer.show(user, menuScreen(vault, mail))
    } yield ()

  private def menuScreen(vault: BankVault, mail: Long = 0L): Screen = {
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
        color = ChoiceColor.Positive, row = Some(3))) :+
      Choice("LeaveTradeHouse", content.text("bank.tradeHouse.leave"), color = ChoiceColor.Negative, row = Some(4))
    val text = content.format("bank.tradeHouse.menu",
      "cells" -> vault.cells.toString, "price" -> vault.nextCellPrice.toString)
    Screen(text, (buy :: vaultBtn.toList) ++ rest)
  }

  /** Страница с одним текстом и «Назад» — заглушки про дублоны и проценты. */
  private def showPage(user: User, renderer: Renderer, key: String): Task[Unit] =
    renderer.show(user, Screen(content.text(key), backRow))

  private def backRow: List[Choice] =
    List(Choice("TradeHouse", content.text("bank.tradeHouse.back"), color = ChoiceColor.Negative, row = Some(0)))

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

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def getVault(user: User): Task[BankVault] =
    getHero(user).flatMap(h => bankRepo.get(h.id).mapError(asThrowable))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}
