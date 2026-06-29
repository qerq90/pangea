package pangea.service.state.states

import io.circe.{Decoder, Encoder, Json, jawn}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.barrel.Barrel
import pangea.model.hero.Hero
import pangea.model.item.Item
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.barrel.{BarrelRepoError, BarrelRepository}
import pangea.repository.inventory.{InventoryRepoError, InventoryRepository}
import pangea.service.state.ItemMenu
import pangea.service.state.states.UnassumingBarrelState._
import pangea.service.state.{State, UserAction}
import zio.{Task, ZIO}

/** Неприметная бочка в Портовом квартале — личное хранилище игрока: до
 *  [[Barrel.MaxItems]] предметов и до [[Barrel.MaxGold]] золота, отдельно от
 *  инвентаря и кошелька. Текстовый ввод суммы золота: режим (`deposit` /
 *  `withdraw`) хранится в `heroes.scene_data`, fallback Branch'а парсит число. */
case class UnassumingBarrelState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  barrelRepo:    BarrelRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "BarrelMenu"        -> Target.Run { (u, _, r) => resetScene(u) *> showMenu(u, r).as(StateType.UnassumingBarrel) },
      "DepositItemsMenu"  -> Target.Run { (u, _, r) => writeScene(u, BarrelScene(depositPage = Some(0))) *> showDepositItems(u, r).as(StateType.UnassumingBarrel) },
      "DepositItemsPrev"  -> Target.Run { (u, _, r) => navigateDeposit(u, r, -1).as(StateType.UnassumingBarrel) },
      "DepositItemsNext"  -> Target.Run { (u, _, r) => navigateDeposit(u, r, +1).as(StateType.UnassumingBarrel) },
      "WithdrawItemsMenu" -> Target.Run { (u, _, r) => writeScene(u, BarrelScene(withdrawPage = Some(0))) *> showWithdrawItems(u, r).as(StateType.UnassumingBarrel) },
      "WithdrawItemsPrev" -> Target.Run { (u, _, r) => navigateWithdraw(u, r, -1).as(StateType.UnassumingBarrel) },
      "WithdrawItemsNext" -> Target.Run { (u, _, r) => navigateWithdraw(u, r, +1).as(StateType.UnassumingBarrel) },
      "DepositGoldMenu"   -> Target.Run { (u, _, r) => writeScene(u, BarrelScene(barrelMode = Some(ModeDepositGold))) *> showDepositGold(u, r).as(StateType.UnassumingBarrel) },
      "WithdrawGoldMenu"  -> Target.Run { (u, _, r) => writeScene(u, BarrelScene(barrelMode = Some(ModeWithdrawGold))) *> showWithdrawGold(u, r).as(StateType.UnassumingBarrel) },
      "LeaveBarrel"       -> Target.Goto(StateType.HarborQuarter)
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    resetScene(user) *> showMenu(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // --- Меню бочки ---

  private def showMenu(user: User, renderer: Renderer): Task[Unit] =
    for {
      barrel <- getBarrel(user)
      text    = content.format("barrel.menu.text",
                  "items"    -> barrel.items.data.length.toString,
                  "maxItems" -> Barrel.MaxItems.toString,
                  "gold"     -> barrel.gold.toString,
                  "maxGold"  -> Barrel.MaxGold.toString)
      choices = List(
        Choice("DepositItemsMenu",  content.text("barrel.menu.depositItems"),  row = Some(0)),
        Choice("WithdrawItemsMenu", content.text("barrel.menu.withdrawItems"), row = Some(0)),
        Choice("DepositGoldMenu",   content.text("barrel.menu.depositGold"),   row = Some(1)),
        Choice("WithdrawGoldMenu",  content.text("barrel.menu.withdrawGold"),  row = Some(1)),
        Choice("LeaveBarrel",       content.text("barrel.menu.leave"),         color = ChoiceColor.Negative, row = Some(2))
      )
      _ <- renderer.show(user, Screen(text, choices))
    } yield ()

  // --- Списки предметов ---

  private def showDepositItems(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero   <- getHero(user)
      inv    <- inventoryRepo.get(hero.id).mapError(asThrowable)
      barrel <- getBarrel(user)
      scene  <- readScene(user)
      items   = inv.items.data
      _ <- if (items.isEmpty)
             renderer.show(user, Screen(content.text("barrel.emptyInventory"), backRow))
           else {
             val (pageItems, totalPages, page) = ItemMenu.page(items, scene.depositPage.getOrElse(0))
             val header     = content.format("barrel.depositItemsHeader",
                                "free"  -> barrel.freeSlots.toString,
                                "page"  -> (page + 1).toString,
                                "total" -> totalPages.toString)
             val itemBtns   = ItemMenu.itemButtons(pageItems, DepositItemPrefix)
             val nav        = navRow(
                                back  = Some(Choice("BarrelMenu", content.text("barrel.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow))),
                                prev  = Option.when(page > 0)(Choice("DepositItemsPrev", content.text("common.prev"), row = Some(ItemMenu.NavRow))),
                                next  = Option.when(page < totalPages - 1)(Choice("DepositItemsNext", content.text("common.next"), row = Some(ItemMenu.NavRow))))
             renderer.show(user, Screen(header, itemBtns ++ nav))
           }
    } yield ()

  private def navigateDeposit(user: User, renderer: Renderer, delta: Int): Task[Unit] =
    for {
      scene  <- readScene(user)
      hero   <- getHero(user)
      inv    <- inventoryRepo.get(hero.id).mapError(asThrowable)
      (_, totalPages, _) = ItemMenu.page(inv.items.data, 0)
      curPage    = scene.depositPage.getOrElse(0)
      newPage    = (curPage + delta).max(0).min(totalPages - 1)
      _      <- writeScene(user, scene.copy(depositPage = Some(newPage)))
      _      <- showDepositItems(user, renderer)
    } yield ()

  private def showWithdrawItems(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero   <- getHero(user)
      inv    <- inventoryRepo.get(hero.id).mapError(asThrowable)
      barrel <- getBarrel(user)
      scene  <- readScene(user)
      items   = barrel.items.data
      _ <- if (items.isEmpty)
             renderer.show(user, Screen(content.text("barrel.emptyBarrel"), backRow))
           else {
             val (pageItems, totalPages, page) = ItemMenu.page(items, scene.withdrawPage.getOrElse(0))
             val header  = content.format("barrel.withdrawItemsHeader", "free" -> inv.freeSlots.toString) +
                           (if (totalPages > 1) s" (${page + 1}/$totalPages)" else "")
             val buttons = ItemMenu.itemButtons(pageItems, WithdrawItemPrefix)
             val nav     = navRow(
                             back = Some(Choice("BarrelMenu", content.text("barrel.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow))),
                             prev = Option.when(page > 0)(Choice("WithdrawItemsPrev", content.text("common.prev"), row = Some(ItemMenu.NavRow))),
                             next = Option.when(page < totalPages - 1)(Choice("WithdrawItemsNext", content.text("common.next"), row = Some(ItemMenu.NavRow))))
             renderer.show(user, Screen(header, buttons ++ nav))
           }
    } yield ()

  private def navigateWithdraw(user: User, renderer: Renderer, delta: Int): Task[Unit] =
    for {
      scene  <- readScene(user)
      hero   <- getHero(user)
      barrel <- barrelRepo.get(hero.id).mapError(asThrowable)
      (_, totalPages, _) = ItemMenu.page(barrel.items.data, 0)
      curPage = scene.withdrawPage.getOrElse(0)
      newPage = (curPage + delta).max(0).min(totalPages - 1)
      _      <- writeScene(user, scene.copy(withdrawPage = Some(newPage)))
      _      <- showWithdrawItems(user, renderer)
    } yield ()

  private def backRow: List[Choice] =
    List(Choice("BarrelMenu", content.text("barrel.back"), color = ChoiceColor.Negative, row = Some(0)))

  /** Сборка единого ряда `[Назад][Предыдущий][Следующий]` (ВК-стиль — последний
   *  ряд экрана, до трёх кнопок). Назад всегда красная. */
  private def navRow(back: Option[Choice], prev: Option[Choice], next: Option[Choice]): List[Choice] =
    List(back, prev, next).flatten

  // --- Ввод золота ---

  private def showDepositGold(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero   <- getHero(user)
      barrel <- getBarrel(user)
      text    = content.format("barrel.depositGoldPrompt",
                  "heroGold" -> hero.gold.toString,
                  "free"     -> barrel.freeGoldSpace.toString)
      _ <- renderer.show(user, Screen(text, cancelGoldRow))
    } yield ()

  private def showWithdrawGold(user: User, renderer: Renderer): Task[Unit] =
    for {
      barrel <- getBarrel(user)
      text    = content.format("barrel.withdrawGoldPrompt", "barrelGold" -> barrel.gold.toString)
      _ <- renderer.show(user, Screen(text, cancelGoldRow))
    } yield ()

  private def cancelGoldRow: List[Choice] =
    List(Choice("BarrelMenu", content.text("barrel.cancelGold"), color = ChoiceColor.Negative, row = Some(0)))

  // --- Fallback: динамические id предметов и текстовый ввод суммы ---

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(DepositItemPrefix)  =>
        a.drop(DepositItemPrefix.length).toLongOption.fold(showMenu(user, renderer))(depositItem(user, _, renderer))
          .as(StateType.UnassumingBarrel)
      case Some(a) if a.startsWith(WithdrawItemPrefix) =>
        a.drop(WithdrawItemPrefix.length).toLongOption.fold(showMenu(user, renderer))(withdrawItem(user, _, renderer))
          .as(StateType.UnassumingBarrel)
      case _ =>
        readScene(user).flatMap { scene =>
          scene.barrelMode match {
            case Some(mode) => handleGoldText(user, mode, ua.text.trim, renderer).as(StateType.UnassumingBarrel)
            case None       => showMenu(user, renderer).as(StateType.UnassumingBarrel)
          }
        }
    }

  private def handleGoldText(user: User, mode: String, text: String, renderer: Renderer): Task[Unit] =
    text.toLongOption match {
      case None =>
        renderer.show(user, Screen(content.text("barrel.goldNotANumber"), cancelGoldRow))
      case Some(n) if n <= 0 =>
        renderer.show(user, Screen(content.text("barrel.goldNonPositive"), cancelGoldRow))
      case Some(amount) =>
        if (mode == ModeDepositGold) doDepositGold(user, amount, renderer)
        else                         doWithdrawGold(user, amount, renderer)
    }

  // --- Транзакции ---

  private def depositItem(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero   <- getHero(user)
      inv    <- inventoryRepo.get(hero.id).mapError(asThrowable)
      _ <- inv.items.data.find(_.id == itemId) match {
        case None => showDepositItems(user, renderer)
        case Some(item) =>
          barrelRepo.deposit(hero.id, item).foldZIO(
            {
              case BarrelRepoError.BarrelFull => renderer.show(user, Screen(content.text("barrel.barrelFull"), Nil)) *> showDepositItems(user, renderer)
              case e                          => ZIO.fail(asThrowable(e))
            },
            _ => inventoryRepo.removeItem(item.id, hero.id).mapError(asThrowable) *>
                 renderer.show(user, Screen(content.format("barrel.itemDeposited", "name" -> item.name), Nil)) *>
                 showDepositItems(user, renderer)
          )
      }
    } yield ()

  private def withdrawItem(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      _ <- if (inv.freeSlots <= 0)
             renderer.show(user, Screen(content.text("barrel.inventoryFull"), Nil)) *> showWithdrawItems(user, renderer)
           else
             barrelRepo.withdraw(hero.id, itemId).foldZIO(
               {
                 case BarrelRepoError.CantFindItemToRemove => showWithdrawItems(user, renderer)
                 case e                                    => ZIO.fail(asThrowable(e))
               },
               item => inventoryRepo.addItem(hero.id, item).foldZIO(
                 {
                   // редкая гонка: пока проверяли — кто-то заполнил. Возвращаем предмет в бочку.
                   case InventoryRepoError.NoMorePlaceForItems =>
                     barrelRepo.deposit(hero.id, item).mapError(asThrowable) *>
                       renderer.show(user, Screen(content.text("barrel.inventoryFull"), Nil)) *>
                       showWithdrawItems(user, renderer)
                   case e => ZIO.fail(asThrowable(e))
                 },
                 _ => renderer.show(user, Screen(content.format("barrel.itemWithdrawn", "name" -> item.name), Nil)) *>
                      showWithdrawItems(user, renderer)
               )
             )
    } yield ()

  private def doDepositGold(user: User, amount: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      _ <- if (amount > hero.gold)
             renderer.show(user, Screen(content.text("barrel.goldNotEnoughHero"), cancelGoldRow))
           else
             barrelRepo.depositGold(hero.id, amount).foldZIO(
               {
                 case BarrelRepoError.GoldOverflow      => renderer.show(user, Screen(content.text("barrel.goldOverflow"), cancelGoldRow))
                 case BarrelRepoError.NonPositiveAmount => renderer.show(user, Screen(content.text("barrel.goldNonPositive"), cancelGoldRow))
                 case e                                 => ZIO.fail(asThrowable(e))
               },
               _ => heroDao.updateGold(user.userId, hero.gold - amount) *>
                    renderer.show(user, Screen(content.format("barrel.goldDeposited", "amount" -> amount.toString), Nil)) *>
                    resetScene(user) *>
                    showMenu(user, renderer)
             )
    } yield ()

  private def doWithdrawGold(user: User, amount: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      _ <- barrelRepo.withdrawGold(hero.id, amount).foldZIO(
             {
               case BarrelRepoError.NotEnoughGold     => renderer.show(user, Screen(content.text("barrel.goldNotEnoughBarrel"), cancelGoldRow))
               case BarrelRepoError.NonPositiveAmount => renderer.show(user, Screen(content.text("barrel.goldNonPositive"), cancelGoldRow))
               case e                                 => ZIO.fail(asThrowable(e))
             },
             _ => heroDao.updateGold(user.userId, hero.gold + amount) *>
                  renderer.show(user, Screen(content.format("barrel.goldWithdrawn", "amount" -> amount.toString), Nil)) *>
                  resetScene(user) *>
                  showMenu(user, renderer)
           )
    } yield ()

  // --- Вспомогательное ---

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def getBarrel(user: User): Task[Barrel] =
    getHero(user).flatMap(h => barrelRepo.get(h.id).mapError(asThrowable))

  private def readScene(user: User): Task[BarrelScene] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[BarrelScene].toOption).getOrElse(BarrelScene()))

  private def writeScene(user: User, scene: BarrelScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def resetScene(user: User): Task[Unit] =
    heroDao.writeSceneData(user.userId, Json.Null)

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object UnassumingBarrelState {
  val DepositItemPrefix  = "DepositItem_"
  val WithdrawItemPrefix = "WithdrawItem_"
  val ModeDepositGold    = "depositGold"
  val ModeWithdrawGold   = "withdrawGold"

  case class BarrelScene(
    barrelMode:   Option[String] = None,
    depositPage:  Option[Int]    = None,
    withdrawPage: Option[Int]    = None
  )
  object BarrelScene {
    implicit val encoder: Encoder[BarrelScene] = deriveEncoder
    implicit val decoder: Decoder[BarrelScene] = deriveDecoder
  }
}
