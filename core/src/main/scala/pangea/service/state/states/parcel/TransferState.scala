package pangea.service.state.states.parcel

import io.circe.{Decoder, Encoder, Json, jawn}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.{Hero, HeroId}
import pangea.model.item.{Item, ItemStack}
import pangea.model.state.StateType
import pangea.model.user.{User, UserId}
import pangea.repository.inventory.InventoryRepository
import pangea.service.parcel.{TransferTarget, Transfers}
import pangea.service.state.states.parcel.TransferState._
import pangea.service.state.{ItemMenu, State, UserAction}
import zio.{Task, ZIO}

import java.util.concurrent.TimeUnit

/** Уточнение, что именно передать. Сюда игрока приводит команда «Передать» из
 *  общей беседы, но только когда без уточнения не обойтись: экипировку выбирают
 *  всегда (два меча с одним именем и уровнем различаются характеристиками), а
 *  всё остальное с понятным названием уходит сразу, без этого экрана.
 *
 *  Вещь идёт получателю посылкой, поэтому чужую сумку мы не трогаем и передача
 *  работает, где бы он ни был. */
case class TransferState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  transfers:     Transfers,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "TransferList" -> Target.Run { (u, _, r) => showList(u, r).as(StateType.Transfer) },
      "TransferPrev" -> Target.Run { (u, _, r) => turnPage(u, r, -1).as(StateType.Transfer) },
      "TransferNext" -> Target.Run { (u, _, r) => turnPage(u, r, +1).as(StateType.Transfer) },
      "TransferYes"  -> Target.Run { (u, _, r) => send(u, r) },
      "TransferNo"   -> Target.Run { (u, _, r) => leave(u, r) }
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets + StateType.GlobalMap

  override def enter(user: User, renderer: Renderer): Task[Unit] = showList(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  // ── Список подходящих вещей ────────────────────────────────────────────────

  private def showList(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      scene <- readScene(user)
      items  = transfers.matching(inv.items.data, scene.query)
      _ <- if (items.isEmpty)
             renderer.show(user, Screen(content.format("transfer.nothing", "query" -> scene.query), leaveRow))
           else {
             val (pageItems, pages, p) = ItemMenu.page(ItemStack.grouped(items), scene.page)
             val header = content.format("transfer.pick",
               "to"    -> scene.toName,
               "count" -> scene.count.toString,
               "page"  -> (p + 1).toString,
               "total" -> pages.toString)
             val buttons = pageItems.zipWithIndex.map { case ((item, count), i) =>
               Choice(s"$PickPrefix${item.id}", label(item, count), row = Some(i))
             }
             val nav = List(
               Some(Choice("TransferNo", content.text("transfer.cancel"), color = ChoiceColor.Negative, row = Some(ItemMenu.NavRow))),
               Option.when(p > 0)(Choice("TransferPrev", content.text("common.prev"), row = Some(ItemMenu.NavRow))),
               Option.when(p < pages - 1)(Choice("TransferNext", content.text("common.next"), row = Some(ItemMenu.NavRow)))
             ).flatten
             renderer.show(user, Screen(header, buttons ++ nav))
           }
    } yield ()

  /** Подпись кнопки: заголовок и числовые характеристики. Именно они и
    * отличают друг от друга две одинаковые с виду вещи. */
  private def label(item: Item, count: Int): String =
    ItemMenu.truncate(
      (item.displayTitle +: item.statsLines.filter(isNumeric).take(3)).mkString(" ") +
        ItemStack.countSuffix(count))

  private def isNumeric(line: String): Boolean =
    line.headOption.exists(c => "⚔🎯⚡🧥🛡💨❤".contains(c))

  private def turnPage(user: User, renderer: Renderer, delta: Int): Task[Unit] =
    readScene(user).flatMap(s => writeScene(user, s.copy(page = (s.page + delta).max(0)))) *>
      showList(user, renderer)

  // ── Подтверждение и отправка ───────────────────────────────────────────────

  private def confirm(user: User, itemId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      scene <- readScene(user)
      chosen = inv.items.data.find(i => i.id == itemId && transfers.sendable(i))
      _ <- chosen match {
        case None => showList(user, renderer)
        case Some(item) =>
          val stats = item.statsLines
          val text  = content.format("transfer.confirm",
            "name"  -> item.displayTitle,
            "count" -> batch(inv.items.data, item, scene.count).size.toString,
            "to"    -> scene.toName) + (if (stats.isEmpty) "" else "\n" + stats.mkString("\n"))
          writeScene(user, scene.copy(itemId = Some(item.id))) *>
            renderer.show(user, Screen(text, List(
              Choice("TransferYes", content.text("transfer.confirmYes"), color = ChoiceColor.Positive, row = Some(0)),
              Choice("TransferNo",  content.text("transfer.confirmNo"),  color = ChoiceColor.Negative, row = Some(0))
            ), inline = true))
      }
    } yield ()

  /** Сколько штук уходит: выбранная вещь и такие же, сколько просили. */
  private def batch(items: List[Item], item: Item, count: Int): List[Item] =
    items.filter(transfers.sameStack(item, _)).take(count.max(1))

  private def send(user: User, renderer: Renderer): Task[StateType] =
    for {
      now   <- nowMs
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      scene <- readScene(user)
      chosen = scene.itemId.flatMap(id => inv.items.data.find(i => i.id == id && transfers.sendable(i)))
      res <- chosen match {
        case None => showList(user, renderer).as(StateType.Transfer)
        case Some(item) =>
          val target = TransferTarget(HeroId(scene.toHeroId), UserId(scene.toUserId), scene.toName)
          for {
            sent <- transfers.send(user, hero, target, batch(inv.items.data, item, scene.count), now)
            _    <- renderer.show(user, Screen(content.format("transfer.sent",
                      "name" -> item.displayTitle, "count" -> sent.size.toString, "to" -> scene.toName), Nil))
            next <- leave(user, renderer)
          } yield next
      }
    } yield res

  /** Уйти туда, откуда игрока забрала команда из беседы. */
  private def leave(user: User, renderer: Renderer): Task[StateType] =
    for {
      back <- heroDao.readReturnState(user.userId).map(_.getOrElse(StateType.GlobalMap))
      _    <- resetScene(user)
      to    = if (back == StateType.Transfer) StateType.GlobalMap else back
      _    <- renderer.show(user, Screen(content.text("transfer.back"), Nil))
    } yield to

  // ── Вспомогательное ────────────────────────────────────────────────────────

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    parseAction(ua.payload) match {
      case Some(a) if a.startsWith(PickPrefix) =>
        a.drop(PickPrefix.length).toLongOption
          .fold(showList(user, renderer))(confirm(user, _, renderer)).as(StateType.Transfer)
      case _ => showList(user, renderer).as(StateType.Transfer)
    }

  private def leaveRow: List[Choice] =
    List(Choice("TransferNo", content.text("transfer.cancel"), color = ChoiceColor.Negative, row = Some(0)))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def readScene(user: User): Task[TransferScene] =
    heroDao.readSceneData(user.userId)
      .map(_.flatMap(_.as[TransferScene].toOption).getOrElse(TransferScene()))

  private def writeScene(user: User, scene: TransferScene): Task[Unit] =
    heroDao.writeSceneData(user.userId, scene.asJson)

  private def resetScene(user: User): Task[Unit] =
    heroDao.writeSceneData(user.userId, Json.Null)

  private def parseAction(payload: Option[String]): Option[String] =
    payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))

  private def nowMs: Task[Long] = ZIO.clockWith(_.currentTime(TimeUnit.MILLISECONDS))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}

object TransferState {
  val PickPrefix = "Give_"

  /** Кому и что передают: сцена живёт от команды в беседе до подтверждения. */
  case class TransferScene(
    toHeroId: Long         = 0L,
    toUserId: Long         = 0L,
    toName:   String       = "",
    query:    String       = "",
    count:    Int          = 1,
    page:     Int          = 0,
    itemId:   Option[Long] = None
  )
  object TransferScene {
    implicit val encoder: Encoder[TransferScene] = deriveEncoder
    implicit val decoder: Decoder[TransferScene] = deriveDecoder
  }
}
