package pangea.service.state.states.temple

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, jawn}
import pangea.dao.hero.HeroDao
import pangea.domain.Rng
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.generator.item.CubeCraft
import pangea.model.hero.{AzatState, Hero}
import pangea.model.item.Item
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.state.ItemMenu
import pangea.service.state.states.temple.CubeState._
import pangea.service.state.{State, UserAction}
import zio.{Random, Task, ZIO}

/** Крафт в кубе Азата: по образцу «Неприметной бочки» игрок кладёт в куб до
 *  [[AzatState.CubeCapacity]] предметов и забирает их. Кнопка «Активировать»
 *  просчитывает рецепты ([[CubeCraft]]); успешные применения тратят заряды.
 *  Содержимое куба и заряды живут в `azat_data`; пагинация — в `scene_data`. */
case class CubeState(
  heroDao:        HeroDao,
  inventoryRepo:  InventoryRepository,
  itemRepository: ItemRepository,
  content:        SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "CubeMenu"         -> Target.Run { (u, _, r) => resetScene(u) *> showMenu(u, r).as(StateType.Cube) },
      "CubeDeposit"      -> Target.Run { (u, _, r) => writeScene(u, CubeScene(depositPage = Some(0))) *> showDeposit(u, r).as(StateType.Cube) },
      "CubeDepositPrev"  -> Target.Run { (u, _, r) => navigateDeposit(u, r, -1).as(StateType.Cube) },
      "CubeDepositNext"  -> Target.Run { (u, _, r) => navigateDeposit(u, r, +1).as(StateType.Cube) },
      "CubeWithdraw"     -> Target.Run { (u, _, r) => writeScene(u, CubeScene(withdrawPage = Some(0))) *> showWithdraw(u, r).as(StateType.Cube) },
      "CubeWithdrawPrev" -> Target.Run { (u, _, r) => navigateWithdraw(u, r, -1).as(StateType.Cube) },
      "CubeWithdrawNext" -> Target.Run { (u, _, r) => navigateWithdraw(u, r, +1).as(StateType.Cube) },
      "CubeActivate"     -> Target.Run { (u, _, r) => activate(u, r).as(StateType.Cube) },
      "LeaveCube"        -> Target.Goto(StateType.HallAzat)
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    resetScene(user) *> showMenu(user, renderer).unit

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // --- Меню куба ---

  private def showMenu(user: User, renderer: Renderer): Task[Unit] =
    for {
      azat <- loadAzat(user)
      text  = content.format("cube.menu.text",
                "items"    -> azat.cubeItems.length.toString,
                "capacity" -> AzatState.CubeCapacity.toString,
                "charges"  -> azat.cubeCharges.toString)
      choices = List(
        Choice("CubeDeposit",  content.text("cube.menu.deposit"),  row = Some(0)),
        Choice("CubeWithdraw", content.text("cube.menu.withdraw"), row = Some(0)),
        Choice("CubeActivate", content.text("cube.menu.activate"), color = ChoiceColor.Positive, row = Some(1)),
        Choice("LeaveCube",    content.text("cube.menu.leave"),     color = ChoiceColor.Negative, row = Some(2))
      )
      _ <- renderer.show(user, Screen(text, choices))
    } yield ()

  // --- Списки ---

  private def showDeposit(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      azat  <- loadAzat(user)
      scene <- readScene(user)
      items  = inv.items.data
      free   = AzatState.CubeCapacity - azat.cubeItems.length
      _ <- if (items.isEmpty)
             renderer.show(user, Screen(content.text("cube.emptyInventory"), backRow))
           else {
             val (pageItems, totalPages, page) = ItemMenu.page(items, scene.depositPage.getOrElse(0))
             val header  = content.format("cube.depositHeader", "free" -> free.toString) +
                           (if (totalPages > 1) s" (${page + 1}/$totalPages)" else "")
             val btns    = ItemMenu.itemButtons(pageItems, DepositPrefix)
             val nav     = navRow("CubeMenu",
                             Option.when(page > 0)(Choice("CubeDepositPrev", content.text("common.prev"), row = Some(ItemMenu.NavRow))),
                             Option.when(page < totalPages - 1)(Choice("CubeDepositNext", content.text("common.next"), row = Some(ItemMenu.NavRow))))
             renderer.show(user, Screen(header, btns ++ nav))
           }
    } yield ()

  private def showWithdraw(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      azat  <- loadAzat(user)
      scene <- readScene(user)
      items  = azat.cubeItems
      _ <- if (items.isEmpty)
             renderer.show(user, Screen(content.text("cube.emptyCube"), backRow))
           else {
             val (pageItems, totalPages, page) = ItemMenu.page(items, scene.withdrawPage.getOrElse(0))
             val header  = content.format("cube.withdrawHeader", "free" -> inv.freeSlots.toString) +
                           (if (totalPages > 1) s" (${page + 1}/$totalPages)" else "")
             val btns    = ItemMenu.itemButtons(pageItems, WithdrawPrefix)
             val nav     = navRow("CubeMenu",
                             Option.when(page > 0)(Choice("CubeWithdrawPrev", content.text("common.prev"), row = Some(ItemMenu.NavRow))),
                             Option.when(page < totalPages - 1)(Choice("CubeWithdrawNext", content.text("common.next"), row = Some(ItemMenu.NavRow))))
             renderer.show(user, Screen(header, btns ++ nav))
           }
    } yield ()

  private def navigateDeposit(user: User, renderer: Renderer, delta: Int): Task[Unit] =
    for {
      scene <- readScene(user)
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      (_, totalPages, _) = ItemMenu.page(inv.items.data, 0)
      newPage = (scene.depositPage.getOrElse(0) + delta).max(0).min(totalPages - 1)
      _ <- writeScene(user, scene.copy(depositPage = Some(newPage)))
      _ <- showDeposit(user, renderer)
    } yield ()

  private def navigateWithdraw(user: User, renderer: Renderer, delta: Int): Task[Unit] =
    for {
      scene <- readScene(user)
      azat  <- loadAzat(user)
      (_, totalPages, _) = ItemMenu.page(azat.cubeItems, 0)
      newPage = (scene.withdrawPage.getOrElse(0) + delta).max(0).min(totalPages - 1)
      _ <- writeScene(user, scene.copy(withdrawPage = Some(newPage)))
      _ <- showWithdraw(user, renderer)
    } yield ()

  private def backRow: List[Choice] =
    List(Choice("CubeMenu", content.text("cube.back"), color = ChoiceColor.Negative, row = Some(0)))

  private def navRow(backAction: String, prev: Option[Choice], next: Option[Choice]): List[Choice] =
    List(Some(Choice(backAction, content.text("cube.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow))), prev, next).flatten

  // --- Транзакции ---

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(DepositPrefix) =>
        a.drop(DepositPrefix.length).toLongOption.fold(showMenu(user, renderer))(depositItem(user, _, renderer)).as(StateType.Cube)
      case Some(a) if a.startsWith(WithdrawPrefix) =>
        a.drop(WithdrawPrefix.length).toLongOption.fold(showMenu(user, renderer))(withdrawItem(user, _, renderer)).as(StateType.Cube)
      case _ => showMenu(user, renderer).as(StateType.Cube)
    }

  private def depositItem(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      azat <- loadAzat(user)
      _ <- inv.items.data.find(_.id == itemId) match {
        case None => showDeposit(user, renderer)
        case Some(item) =>
          if (azat.cubeItems.length >= AzatState.CubeCapacity)
            renderer.show(user, Screen(content.text("cube.full"), Nil)) *> showDeposit(user, renderer)
          else
            inventoryRepo.removeItem(item.id, hero.id).mapError(asThrowable) *>
              saveAzat(user, azat.copy(cubeItems = azat.cubeItems :+ item)) *>
              renderer.show(user, Screen(content.format("cube.deposited", "name" -> item.name), Nil)) *>
              showDeposit(user, renderer)
      }
    } yield ()

  private def withdrawItem(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      azat <- loadAzat(user)
      _ <- azat.cubeItems.find(_.id == itemId) match {
        case None => showWithdraw(user, renderer)
        case Some(item) =>
          if (inv.freeSlots <= 0)
            renderer.show(user, Screen(content.text("cube.inventoryFull"), Nil)) *> showWithdraw(user, renderer)
          else
            inventoryRepo.addItem(hero.id, item).mapError(asThrowable) *>
              saveAzat(user, azat.copy(cubeItems = removeFirst(azat.cubeItems, itemId))) *>
              renderer.show(user, Screen(content.format("cube.withdrawn", "name" -> item.name), Nil)) *>
              showWithdraw(user, renderer)
      }
    } yield ()

  // --- Активация: просчёт рецептов ---

  private def activate(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      azat <- loadAzat(user)
      seed <- Random.nextLong
      result = CubeCraft.craft(azat.cubeItems, azat.cubeCharges, Rng(seed))
      // Результаты рецептов (id <= 0) персистим — им нужен id для инвентаря.
      persisted <- ZIO.foreach(result.items) { it =>
        if (it.id > 0L) ZIO.succeed(it)
        else itemRepository.persist(hero.id, it)
      }
      _ <- saveAzat(user, azat.copy(cubeItems = persisted, cubeCharges = azat.cubeCharges - result.chargesUsed))
      msgKey = if (result.anyApplied) "cube.activated" else "cube.nothing"
      _ <- renderer.show(user, Screen(content.text(msgKey), Nil))
      _ <- showMenu(user, renderer)
    } yield ()

  // --- Вспомогательное ---

  private def removeFirst(items: List[Item], itemId: Long): List[Item] =
    items.indexWhere(_.id == itemId) match {
      case -1 => items
      case i  => items.patch(i, Nil, 1)
    }

  private def loadAzat(user: User): Task[AzatState] =
    heroDao.readAzatData(user.userId).map(_.flatMap(_.as[AzatState].toOption).getOrElse(AzatState.empty))

  private def saveAzat(user: User, azat: AzatState): Task[Unit] =
    heroDao.writeAzatData(user.userId, azat.asJson)

  private def readScene(user: User): Task[CubeScene] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[CubeScene].toOption).getOrElse(CubeScene()))

  private def writeScene(user: User, scene: CubeScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def resetScene(user: User): Task[Unit] =
    heroDao.writeSceneData(user.userId, Json.Null)

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId).flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object CubeState {
  val DepositPrefix  = "CubeDeposit_"
  val WithdrawPrefix = "CubeWithdraw_"

  case class CubeScene(depositPage: Option[Int] = None, withdrawPage: Option[Int] = None)
  object CubeScene {
    implicit val encoder: Encoder[CubeScene] = deriveEncoder
    implicit val decoder: Decoder[CubeScene] = deriveDecoder
  }
}
