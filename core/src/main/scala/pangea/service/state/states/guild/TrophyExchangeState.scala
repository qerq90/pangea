package pangea.service.state.states.guild

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, jawn}
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.item.{Item, ItemDetails, ItemType}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.state.states.guild.TrophyExchangeState._
import pangea.service.state.{ItemMenu, State, UserAction}
import zio.{Task, ZIO}

/**
 * Приём трофеев в Гильдии Искателей. Два режима: «сдать все» (батч — одна
 * кнопка, моментально) и «сдать по одному» (список-кнопками, нажатие сдаёт
 * выбранный трофей и обновляет список). За каждый трофей `ceil(5 + lvl × coef)`
 * репутации, где `coef` — из [[pangea.model.item.TrophyKind]].
 */
case class TrophyExchangeState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "SubmitTrophies"      -> Target.Run { (u, _, r) => submitAll(u, r) },
      "TrophyListMenu"      -> Target.Run { (u, _, r) => writeScene(u, TrophyScene(page = Some(0))) *> showList(u, r) },
      "TrophyListPrev"      -> Target.Run { (u, _, r) => navigate(u, r, -1) },
      "TrophyListNext"      -> Target.Run { (u, _, r) => navigate(u, r, +1) },
      "LeaveTrophyExchange" -> Target.Goto(StateType.Guild)
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    renderer.show(user, content.screen("guild.trophyExchange"))

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Сдать все ───────────────────────────────────────────────────────────────

  private def submitAll(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero      <- getHero(user)
      inventory <- inventoryRepo.get(hero.id).orElseFail(new Throwable("Inventory unavailable"))
      trophies   = inventory.items.data.filter(i => i.id != 0L && i.itemType == ItemType.Trophy)
      gained     = trophies.map(TrophyExchangeState.reputationFor).sum
      _         <- ZIO.foreachDiscard(trophies)(t => inventoryRepo.removeItem(t.id, hero.id).orElse(ZIO.unit))
      _         <- ZIO.when(gained > 0)(heroDao.updateGuildReputation(user.userId, hero.guildReputation + gained))
      msg        = if (trophies.isEmpty) content.text("guild.noTrophies")
                   else content.format("guild.trophiesSubmitted",
                     "count"  -> trophies.length.toString,
                     "gained" -> gained.toString,
                     "total"  -> (hero.guildReputation + gained).toString)
      _         <- renderer.show(user, Screen(msg, Nil))
      _         <- enter(user, renderer)
    } yield StateType.TrophyExchange

  // ── Список трофеев → нажатие сдаёт штучно ──────────────────────────────────

  private def showList(user: User, renderer: Renderer): Task[StateType] =
    for {
      hero      <- getHero(user)
      inventory <- inventoryRepo.get(hero.id).orElseFail(new Throwable("Inventory unavailable"))
      trophies   = inventory.items.data.filter(i => i.id != 0L && i.itemType == ItemType.Trophy)
      scene     <- readScene(user)
      _ <- if (trophies.isEmpty)
             renderer.show(user, Screen(content.text("guild.noTrophies"), navRow(0, 1)))
           else {
             val (pageItems, totalPages, page) = ItemMenu.page(trophies, scene.page.getOrElse(0))
             val header  = content.format("guild.trophyListHeader",
               "reputation" -> hero.guildReputation.toString) +
               (if (totalPages > 1) s" (${page + 1}/$totalPages)" else "")
             val itemBtns = pageItems.zipWithIndex.map { case (it, idx) =>
                              Choice(s"$TrophyItemPrefix${it.id}",
                                ItemMenu.truncate(trophyLabel(it)), row = Some(idx))
                            }
             val nav = navRow(page, totalPages)
             renderer.show(user, Screen(header, itemBtns ++ nav))
           }
    } yield StateType.TrophyExchange

  private def navigate(user: User, renderer: Renderer, delta: Int): Task[StateType] =
    for {
      hero      <- getHero(user)
      inventory <- inventoryRepo.get(hero.id).orElseFail(new Throwable("Inventory unavailable"))
      trophies   = inventory.items.data.filter(i => i.id != 0L && i.itemType == ItemType.Trophy)
      scene     <- readScene(user)
      (_, totalPages, _) = ItemMenu.page(trophies, 0)
      cur        = scene.page.getOrElse(0)
      np         = (cur + delta).max(0).min(totalPages - 1)
      _         <- writeScene(user, scene.copy(page = Some(np)))
      res       <- showList(user, renderer)
    } yield res

  private def submitOne(user: User, itemId: Long, renderer: Renderer): Task[StateType] =
    for {
      hero      <- getHero(user)
      inventory <- inventoryRepo.get(hero.id).orElseFail(new Throwable("Inventory unavailable"))
      res <- inventory.items.data.find(i => i.id == itemId && i.itemType == ItemType.Trophy) match {
        case None => showList(user, renderer)
        case Some(trophy) =>
          val gained = TrophyExchangeState.reputationFor(trophy)
          val total  = hero.guildReputation + gained
          inventoryRepo.removeItem(trophy.id, hero.id).orElse(ZIO.unit) *>
            heroDao.updateGuildReputation(user.userId, total) *>
            renderer.show(user, Screen(content.format("guild.trophySubmitted",
              "name"   -> trophy.name,
              "gained" -> gained.toString,
              "total"  -> total.toString), Nil)) *>
            showList(user, renderer)
      }
    } yield res

  // ── Fallback: динамические id TrophyItem_<id> ──────────────────────────────

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(TrophyItemPrefix) =>
        a.drop(TrophyItemPrefix.length).toLongOption.fold[Task[StateType]](showList(user, renderer))(
          submitOne(user, _, renderer))
      case _ => enter(user, renderer).as(StateType.TrophyExchange)
    }

  // ── Хелперы ─────────────────────────────────────────────────────────────────

  private def trophyLabel(item: Item): String =
    s"${item.name} Ур.${item.lvl} (+${TrophyExchangeState.reputationFor(item)})"

  private def navRow(page: Int, totalPages: Int): List[Choice] = {
    val row = ItemMenu.NavRow
    List(
      Some(Choice("LeaveTrophyExchange", "↩ Назад", color = ChoiceColor.Negative, row = Some(row))),
      Option.when(page > 0)(Choice("TrophyListPrev", content.text("common.prev"), row = Some(row))),
      Option.when(page < totalPages - 1)(Choice("TrophyListNext", content.text("common.next"), row = Some(row)))
    ).flatten
  }

  private def readScene(user: User): Task[TrophyScene] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[TrophyScene].toOption).getOrElse(TrophyScene()))

  private def writeScene(user: User, scene: TrophyScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))
}

object TrophyExchangeState {
  val TrophyItemPrefix = "TrophyItem_"

  case class TrophyScene(page: Option[Int] = None)
  object TrophyScene {
    implicit val encoder: Encoder[TrophyScene] = deriveEncoder
    implicit val decoder: Decoder[TrophyScene] = deriveDecoder
  }

  /** Репутация за один трофей: `ceil(5 + lvl × coef)`, где `coef` — у вида трофея. */
  def reputationFor(item: Item): Long = {
    val coef = item.details match {
      case ItemDetails.Trophy(_, kind) => kind.coef
      case _                           => 0.0
    }
    math.ceil(5.0 + item.lvl.toDouble * coef).toLong
  }
}
