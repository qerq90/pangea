package pangea.service.state.states.parcel

import io.circe.jawn
import pangea.dao.hero.HeroDao
import pangea.engine.{Branch, Choice, ChoiceColor, Renderer, SceneContent, Screen, Target}
import pangea.model.hero.Hero
import pangea.model.parcel.Parcel
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.repository.inventory.InventoryRepository
import pangea.service.parcel.Parcels
import pangea.service.state.{ItemMenu, State, UserAction}
import zio.{Task, ZIO}

/** Почта Торгового дома: сюда попадает переданное, что не влезло в банковскую
 *  ячейку. Вещи отсюда забирают в сумку — по одной или всё, что поместится. */
case class MailState(
  heroDao:       HeroDao,
  inventoryRepo: InventoryRepository,
  parcels:       Parcels,
  content:       SceneContent
) extends State {

  private val branch = new Branch(
    routes = Map(
      "MailList"  -> Target.Run { (u, _, r) => showMail(u, r).as(StateType.Mail) },
      "MailAll"   -> Target.Run { (u, _, r) => takeAll(u, r).as(StateType.Mail) },
      "LeaveMail" -> Target.Goto(StateType.TradeHouse)
    ),
    fallback = Target.Run { (u, ua, r) => handleFallback(u, ua, r) }
  )

  override def targetStates: Set[StateType] = branch.gotoTargets

  override def enter(user: User, renderer: Renderer): Task[Unit] = showMail(user, renderer)

  override def action(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    branch.act(user, ua, renderer)

  private def showMail(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      items <- parcels.waiting(hero.id)
      _ <- if (items.isEmpty) renderer.show(user, Screen(content.text("mail.empty"), leaveRow))
           else {
             val page = items.take(ItemMenu.DefaultPageSize)
             val buttons = page.zipWithIndex.map { case (p, i) =>
               Choice("MailTake", ItemMenu.truncate(content.format("mail.takeOne",
                 "name" -> p.item.displayTitle, "from" -> p.fromName)),
                 data = Map("id" -> p.id.toString), row = Some(i))
             }
             val text = content.format("mail.header", "free" -> inv.freeSlots.toString) + "\n" +
               page.map(line).mkString("\n")
             renderer.show(user, Screen(text, buttons ++ List(
               Choice("MailAll",   content.text("mail.takeAll"), row = Some(buttons.size)),
               Choice("LeaveMail", content.text("mail.back"), color = ChoiceColor.Negative, row = Some(buttons.size + 1))
             )))
           }
    } yield ()

  private def line(p: Parcel): String =
    content.format("mail.line", "name" -> p.item.displayTitle, "from" -> p.fromName)

  /** Забрать одну посылку в сумку. */
  private def take(user: User, parcelId: Long, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      items <- parcels.waiting(hero.id)
      _ <- items.find(_.id == parcelId) match {
        case None => showMail(user, renderer)
        case Some(_) if inv.freeSlots <= 0 =>
          renderer.show(user, Screen(content.text("common.inventoryFull"), Nil)) *> showMail(user, renderer)
        case Some(parcel) =>
          parcels.claim(hero.id, parcel).flatMap {
            case false => showMail(user, renderer)
            case true =>
              inventoryRepo.addItem(hero.id, parcel.item).mapError(asThrowable) *>
                renderer.show(user, Screen(content.format("mail.taken",
                  "name" -> parcel.item.displayTitle), Nil)) *>
                showMail(user, renderer)
          }
      }
    } yield ()

  /** Забрать всё, что влезет в сумку. */
  private def takeAll(user: User, renderer: Renderer): Task[Unit] =
    for {
      hero  <- getHero(user)
      inv   <- inventoryRepo.get(hero.id).mapError(asThrowable)
      items <- parcels.waiting(hero.id)
      fit    = items.take(inv.freeSlots.toInt.max(0))
      taken <- ZIO.foreach(fit) { parcel =>
                 parcels.claim(hero.id, parcel).flatMap {
                   case false => ZIO.none
                   case true  => inventoryRepo.addItem(hero.id, parcel.item).mapError(asThrowable).as(Some(parcel))
                 }
               }.map(_.flatten)
      _ <- if (taken.isEmpty) renderer.show(user, Screen(content.text("common.inventoryFull"), Nil))
           else renderer.show(user, Screen(content.format("mail.tookAll",
             "items" -> taken.map(_.item.displayTitle).mkString(", ")), Nil))
      _ <- showMail(user, renderer)
    } yield ()

  private def handleFallback(user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    ua.payload.flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("id")))
      .flatMap(_.toLongOption) match {
        case Some(id) => take(user, id, renderer).as(StateType.Mail)
        case None     => showMail(user, renderer).as(StateType.Mail)
      }

  private def leaveRow: List[Choice] =
    List(Choice("LeaveMail", content.text("mail.back"), color = ChoiceColor.Negative, row = Some(0)))

  private def getHero(user: User): Task[Hero] =
    heroDao.getHeroByUserId(user.userId)
      .flatMap(ZIO.fromOption(_))
      .orElseFail(new Throwable(s"No hero for user ${user.userId}"))

  private def asThrowable(e: Any): Throwable = new Throwable(e.toString)
}
