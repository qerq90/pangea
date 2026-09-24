package pangea.service.state.states.artifact

import io.circe.{Decoder, Encoder, Json, jawn}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.domain.Rng
import pangea.generator.item.CubeCraft
import pangea.model.artifact.{Artifact, ArtifactKind, HeroArtifacts}
import pangea.model.hero.{Achievement, Hero}
import pangea.model.item.{BrewKind, ItemStack}
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.artifact.{ArtifactRepoError, ArtifactRepository}
import pangea.repository.inventory.{InventoryRepoError, InventoryRepository}
import pangea.repository.item.ItemRepository
import pangea.service.state.states.artifact.ArtifactState._
import pangea.service.state.{HerbLore, InventoryFeedback, ItemMenu, MarisaQuest, State, UserAction}
import zio.{Random, Task, ZIO}

/** Экран сборного артефакта: Ларца Азата или Живой сумки (вид задаётся при
 *  сборке состояния). Внутрь можно заглянуть, забрать своё и доложить новое, а
 *  «магия Азата» за заряд делает то, ради чего артефакт и держат: плавит три
 *  одинаковых камня в один категорией выше или варит отвар из трав. */
case class ArtifactState(
  kind:          ArtifactKind,
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  itemRepo:      ItemRepository,
  artifacts:     ArtifactRepository,
  content:       SceneContent
) extends State {

  private val self = kind.state

  private val branch = new Branch(
    routes = Map(
      "ArtifactMenu"  -> Target.Run { (u, _, r) => resetScene(u) *> showMenu(u, r).as(self) },
      "ArtifactPut"   -> Target.Run { (u, _, r) => writeScene(u, ArtifactScene(putPage = Some(0))) *> showPutList(u, r).as(self) },
      "ArtifactPutPrev"  -> Target.Run { (u, _, r) => turnPut(u, r, -1).as(self) },
      "ArtifactPutNext"  -> Target.Run { (u, _, r) => turnPut(u, r, +1).as(self) },
      "ArtifactTake"  -> Target.Run { (u, _, r) => writeScene(u, ArtifactScene(takePage = Some(0))) *> showTakeList(u, r).as(self) },
      "ArtifactTakePrev" -> Target.Run { (u, _, r) => turnTake(u, r, -1).as(self) },
      "ArtifactTakeNext" -> Target.Run { (u, _, r) => turnTake(u, r, +1).as(self) },
      "ArtifactMagic" -> Target.Run { (u, _, r) => magic(u, r).as(self) },
      "LeaveArtifact" -> Target.Goto(StateType.Backpack)
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] =
    resetScene(user) *> showMenu(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Меню артефакта ─────────────────────────────────────────────────────────

  private def showMenu(user: User, renderer: Renderer): Task[Unit] =
    mine(user).flatMap { a =>
      if (!a.owned) renderer.show(user, Screen(content.text(s"artifact.$key.notOwned"), leaveRow))
      else renderer.show(user, Screen(menuText(a), List(
        Some(Choice("ArtifactPut",   content.text("artifact.putLabel"),  row = Some(0))),
        Some(Choice("ArtifactTake",  content.text("artifact.takeLabel"), row = Some(0))),
        Option.when(kind.hasMagic)(Choice("ArtifactMagic", content.text(s"artifact.$key.magicLabel"),
          color = ChoiceColor.Positive, row = Some(1))),
        Some(Choice("LeaveArtifact", content.text("artifact.back"), color = ChoiceColor.Negative, row = Some(2)))
      ).flatten))
    }

  private def menuText(a: Artifact): String =
    // У шкафа зарядов нет — и строки про них тоже.
    content.format(if (kind.hasMagic) "artifact.menu" else "artifact.menuPlain",
      "title"      -> content.text(s"artifact.$key.title"),
      "tier"       -> a.tier.toString,
      "maxTier"    -> HeroArtifacts.MaxTier.toString,
      "items"      -> a.occupied.toString,
      "capacity"   -> a.capacity.toString,
      "charges"    -> a.charges.toString,
      "maxCharges" -> HeroArtifacts.MaxCharges.toString)

  // ── Положить / забрать ─────────────────────────────────────────────────────

  private def showPutList(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      a     <- mine(user)
      scene <- readScene(user)
      items  = inv.items.data.filter(i => kind.accepts(i) && !i.isQuestItem)
      _ <- if (items.isEmpty) renderer.show(user, Screen(content.text(s"artifact.$key.nothingToPut"), backRow))
           else {
             val (pageItems, pages, p) = ItemMenu.page(ItemStack.grouped(items), scene.putPage.getOrElse(0))
             val header = content.format("artifact.putHeader",
               "free" -> a.freeSlots.toString, "page" -> (p + 1).toString, "total" -> pages.toString)
             renderer.show(user, Screen(header,
               ItemMenu.stackButtons(pageItems, PutPrefix) ++
                 navRow("ArtifactPutPrev", "ArtifactPutNext", p, pages)))
           }
    } yield ()

  private def showTakeList(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      a     <- mine(user)
      scene <- readScene(user)
      _ <- if (a.items.data.isEmpty) renderer.show(user, Screen(content.text(s"artifact.$key.empty"), backRow))
           else {
             val (pageItems, pages, p) = ItemMenu.page(ItemStack.grouped(a.items.data), scene.takePage.getOrElse(0))
             val header = content.format("artifact.takeHeader",
               "free" -> inv.freeSlots.toString, "page" -> (p + 1).toString, "total" -> pages.toString)
             renderer.show(user, Screen(header,
               ItemMenu.stackButtons(pageItems, TakePrefix) ++
                 navRow("ArtifactTakePrev", "ArtifactTakeNext", p, pages)))
           }
    } yield ()

  private def turnPut(user: User, renderer: Renderer, delta: Int): Task[Unit] =
    readScene(user).flatMap(s => writeScene(user, s.copy(putPage = Some((s.putPage.getOrElse(0) + delta).max(0))))) *>
      showPutList(user, renderer)

  private def turnTake(user: User, renderer: Renderer, delta: Int): Task[Unit] =
    readScene(user).flatMap(s => writeScene(user, s.copy(takePage = Some((s.takePage.getOrElse(0) + delta).max(0))))) *>
      showTakeList(user, renderer)

  private def putItem(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      _ <- inv.items.data.find(i => i.id == itemId && kind.accepts(i)) match {
        case None => showPutList(user, renderer)
        case Some(item) =>
          artifacts.put(hero.id, kind, item).foldZIO(
            {
              case ArtifactRepoError.NoRoom => renderer.show(user, Screen(content.text(s"artifact.$key.full"), Nil)) *> showPutList(user, renderer)
              case e                        => ZIO.fail(asThrowable(e))
            },
            a => inventoryRepo.removeItem(item.id, hero.id).mapError(asThrowable) *>
                   renderer.show(user, Screen(content.format("artifact.putDone",
                     "name" -> item.displayTitle, "free" -> a.freeSlots.toString), Nil)) *>
                   showPutList(user, renderer)
          )
      }
    } yield ()

  private def takeItem(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      inv  <- inventoryRepo.get(hero.id).mapError(asThrowable)
      a    <- mine(user)
      item  = a.items.data.find(_.id == itemId)
      _ <- if (item.isEmpty) showTakeList(user, renderer)
           else if (inv.freeSlots <= 0)
             renderer.show(user, Screen(content.text("common.inventoryFull"), Nil)) *> showTakeList(user, renderer)
           else
             artifacts.take(hero.id, kind, itemId).foldZIO(
               _ => showTakeList(user, renderer),
               got => inventoryRepo.addItem(hero.id, got).foldZIO(
                 {
                   // Сумку успели забить — возвращаем вещь артефакту.
                   case InventoryRepoError.NoMorePlaceForItems | InventoryRepoError.DustLimitReached =>
                     artifacts.put(hero.id, kind, got).mapError(asThrowable) *>
                       renderer.show(user, Screen(InventoryFeedback.refusalLine(content, got), Nil)) *>
                       showTakeList(user, renderer)
                   case e => ZIO.fail(asThrowable(e))
                 },
                 _ => renderer.show(user, Screen(content.format("artifact.takeDone",
                        "name" -> got.displayTitle), Nil)) *> showTakeList(user, renderer)
               )
             )
    } yield ()

  // ── Магия Азата ────────────────────────────────────────────────────────────

  private def magic(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero <- getHero(user)
      a    <- mine(user)
      _ <- if (!a.owned || !kind.hasMagic) renderer.show(user, Screen(content.text(s"artifact.$key.notOwned"), leaveRow))
           else if (a.charges <= 0)
             renderer.show(user, Screen(content.format("artifact.noCharges",
               "cost" -> HeroArtifacts.RechargeSilver.toString), Nil)) *> showMenu(user, renderer)
           else runMagic(user, hero, a, renderer)
    } yield ()

  private def runMagic(user: User, hero: Hero, a: Artifact, renderer: Renderer): Task[Unit] =
    for {
      seed  <- Random.nextLong
      result = kind match {
                 case ArtifactKind.Casket    => CubeCraft.upgradeGems(a.items.data, a.charges, Rng(seed))
                 case ArtifactKind.LivingBag => CubeCraft.brewHerbs(a.items.data, a.charges, Rng(seed))
                 // У шкафа магии нет — сюда он не попадает (кнопки тоже нет).
                 case ArtifactKind.Wardrobe  => CubeCraft.Result(a.items.data, 0, Rng(seed))
               }
      // Свежесозданному (id <= 0) нужен свой id — как и результатам куба.
      persisted <- ZIO.foreach(result.items)(i => if (i.id > 0L) ZIO.succeed(i) else itemRepo.persist(hero.id, i))
      _ <- if (!result.anyApplied)
             renderer.show(user, Screen(content.text(s"artifact.$key.magicNothing"), Nil)) *> showMenu(user, renderer)
           else
             artifacts.applyMagic(hero.id, kind, persisted, result.chargesUsed).mapError(asThrowable) *>
               renderer.show(user, Screen(content.format(s"artifact.$key.magicDone",
                 "count" -> result.chargesUsed.toString), Nil)) *>
               brewedAchievement(user, hero, result.items, renderer) *>
               showMenu(user, renderer)
    } yield ()

  /** Сваренное в сумке идёт в тот же счёт «Зельевара I», что и сваренное в кубе. */
  private def brewedAchievement(user: User, hero: Hero, made: List[pangea.model.item.Item], renderer: Renderer): Task[Unit] = {
    val brewed = made.filter(_.id <= 0L).flatMap(_.brew).map(_.entryName)
    ZIO.when(kind == ArtifactKind.LivingBag && brewed.nonEmpty) {
      for {
        lore   <- HerbLore.readLore(heroDao, user.userId)
        updated = lore.brewedAlso(brewed)
        _      <- HerbLore.writeLore(heroDao, user.userId, updated)
        _      <- ZIO.when(BrewKind.rank1.forall(k => updated.brewed.contains(k.entryName)))(
                    MarisaQuest.grant(heroDao, content, user, hero, Achievement.Brewer1, renderer))
      } yield ()
    }.unit
  }

  // ── Вспомогательное ────────────────────────────────────────────────────────

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(PutPrefix) =>
        a.drop(PutPrefix.length).toLongOption.fold(showMenu(user, renderer))(putItem(user, _, renderer)).as(self)
      case Some(a) if a.startsWith(TakePrefix) =>
        a.drop(TakePrefix.length).toLongOption.fold(showMenu(user, renderer))(takeItem(user, _, renderer)).as(self)
      case _ => showMenu(user, renderer).as(self)
    }

  private def navRow(prevId: String, nextId: String, page: Int, pages: Int): List[Choice] =
    List(
      Some(Choice("ArtifactMenu", content.text("artifact.back"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow))),
      Option.when(page > 0)(Choice(prevId, content.text("common.prev"), row = Some(ItemMenu.NavRow))),
      Option.when(page < pages - 1)(Choice(nextId, content.text("common.next"), row = Some(ItemMenu.NavRow)))
    ).flatten

  private def backRow: List[Choice] =
    List(Choice("ArtifactMenu", content.text("artifact.back"), color = ChoiceColor.Negative, row = Some(0)))

  private def leaveRow: List[Choice] =
    List(Choice("LeaveArtifact", content.text("artifact.back"), color = ChoiceColor.Negative, row = Some(0)))

  private def key: String = kind.key

  private def mine(user: User): Task[Artifact] =
    getHero(user).flatMap(h => artifacts.get(h.id).mapError(asThrowable)).map(_.of(kind))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def readScene(user: User): Task[ArtifactScene] =
    heroDao.readSceneData(user.userId).map(_.flatMap(_.as[ArtifactScene].toOption).getOrElse(ArtifactScene()))

  private def writeScene(user: User, scene: ArtifactScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def resetScene(user: User): Task[Unit] =
    heroDao.writeSceneData(user.userId, Json.Null)

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object ArtifactState {
  val PutPrefix  = "ArtPut_"
  val TakePrefix = "ArtTake_"

  case class ArtifactScene(putPage: Option[Int] = None, takePage: Option[Int] = None)
  object ArtifactScene {
    implicit val encoder: Encoder[ArtifactScene] = deriveEncoder
    implicit val decoder: Decoder[ArtifactScene] = deriveDecoder
  }
}
