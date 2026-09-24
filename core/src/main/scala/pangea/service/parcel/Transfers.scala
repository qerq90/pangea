package pangea.service.parcel

import pangea.engine.{Players, SceneContent}
import pangea.model.hero.{Hero, HeroId}
import pangea.model.item.{Item, ItemStack, ItemType}
import pangea.model.user.{User, UserId}
import pangea.repository.inventory.InventoryRepository
import pangea.repository.user.UserRepository
import pangea.service.chat.ChatCommand
import zio.{Task, ZIO, ZLayer}

/** Кому и что передают. Имя получателя берём из ВК один раз и носим с собой:
 *  игроку важно видеть, кому он отдаёт. */
final case class TransferTarget(heroId: HeroId, userId: UserId, name: String)

/** Общая часть передачи вещей: что подходит под запрос, можно ли отдать сразу
 *  и как, собственно, отдать. Этим пользуются и команда из беседы, и экран
 *  выбора в личке — правила у них одни. */
final case class Transfers(
  inventoryRepo: InventoryRepository,
  userRepo:      UserRepository,
  parcels:       Parcels,
  players:       Players,
  content:       SceneContent
) {

  /** Что вообще можно передать: вещь занимает место, не сюжетная и не трофей.
    * Трофеи — личная добыча: их сдают в Гильдии, а не передают друг другу. */
  def sendable(item: Item): Boolean =
    !item.isQuestItem && !item.weightless && item.itemType != ItemType.Trophy

  /** Под запрос подошли только трофеи — о них стоит сказать отдельно, иначе
    * «ничего не нашлось» сбивает с толку. */
  def onlyTrophies(items: List[Item], query: String): Boolean =
    items.exists(i => i.itemType == ItemType.Trophy && ChatCommand.matches(query, i.name)) &&
      matching(items, query).isEmpty

  /** Подходящее под запрос. Точное совпадение по названию бьёт частичное:
    * «надколотый череп» отдаст именно череп, а не всё, где встретилось слово.
    * Уровень в запросе не нужен — по названию находятся вещи всех уровней. */
  def matching(items: List[Item], query: String): List[Item] = {
    val fit   = items.filter(i => sendable(i) && ChatCommand.matches(query, i.name))
    val exact = fit.filter(i => ChatCommand.sameName(query, i.name))
    if (exact.nonEmpty) exact else fit
  }

  /** Можно ли отдать без уточнения: когда спутать не с чем. Одна-единственная
    * подходящая вещь уходит сразу, даже если это экипировка. Уточняем, когда
    * подходящих несколько: у экипировки они различаются характеристиками, а у
    * прочего — тем, что это вообще разные вещи. */
  def unambiguous(items: List[Item]): Boolean =
    items match {
      case Nil            => false
      case _ :: Nil       => true
      case first :: _     =>
        !items.exists(i => ItemType.equippable.contains(i.itemType)) &&
          items.forall(i => sameStack(first, i))
    }

  /** Вещи той же «стопки», что и выбранная: по ним считается «2 штуки». */
  def sameStack(a: Item, b: Item): Boolean =
    a.name == b.name && ItemStack.key(a) == ItemStack.key(b)

  /** Попытка отдать без уточнения — с этого начинается любая передача.
    * Экипировку всегда выбирают кнопками, всё остальное с точным названием
    * уходит сразу. */
  def quickSend(
    sender: User,
    hero:   Hero,
    target: TransferTarget,
    query:  String,
    count:  Int,
    now:    Long
  ): Task[Transfers.Outcome] =
    for {
      inv <- inventoryRepo.get(hero.id).mapError(e => new Throwable(e.toString))
      fit  = matching(inv.items.data, query)
      res <- if (onlyTrophies(inv.items.data, query))
               ZIO.succeed(Transfers.Outcome.Forbidden(content.text("transfer.noTrophies")))
             else if (fit.isEmpty) ZIO.succeed(Transfers.Outcome.Empty)
             else if (!unambiguous(fit)) ZIO.succeed(Transfers.Outcome.NeedPick)
             else send(sender, hero, target, fit.take(count.max(1)), now).map { sent =>
               Transfers.Outcome.Sent(content.format("transfer.sent",
                 "name"  -> sent.head.displayTitle,
                 "count" -> sent.size.toString,
                 "to"    -> target.name))
             }
    } yield res

  /** Отдать вещи: из сумки отправителя — в посылки получателю, плюс письмо
    * получателю. Возвращает, что ушло. */
  def send(
    sender: User,
    hero:   Hero,
    target: TransferTarget,
    items:  List[Item],
    now:    Long
  ): Task[List[Item]] =
    for {
      name <- players.getDisplayName(sender).orElse(ZIO.succeed(content.text("transfer.someone")))
      _    <- ZIO.foreachDiscard(items) { item =>
                inventoryRepo.removeItem(item.id, hero.id).mapError(e => new Throwable(e.toString)) *>
                  parcels.send(target.heroId, name, item, now)
              }
      _    <- ZIO.when(items.nonEmpty)(tell(sender, name, target, items))
    } yield items

  /** Письмо получателю и строка в общий чат: передача — дело публичное. */
  private def tell(sender: User, fromName: String, target: TransferTarget, items: List[Item]): Task[Unit] =
    for {
      receiver <- userRepo.getUserById(target.userId).option.map(_.flatten)
      _        <- ZIO.foreachDiscard(receiver)(r => players.notify(r, content.format("transfer.notice",
                    "from"  -> fromName,
                    "name"  -> items.head.displayTitle,
                    "count" -> items.size.toString)).ignore)
      _        <- players.announce(content.format("transfer.announce",
                    "from"  -> mention(sender.vkId.value, fromName),
                    "to"    -> receiver.map(r => mention(r.vkId.value, target.name)).getOrElse(target.name),
                    "name"  -> items.head.displayTitle,
                    "count" -> items.size.toString)).ignore
    } yield ()

  /** Кликабельное упоминание игрока в ВК. */
  private def mention(vkId: String, name: String): String = s"[id$vkId|$name]"
}

object Transfers {

  /** Чем кончилась попытка передать по одной фразе из беседы. */
  sealed trait Outcome
  object Outcome {
    /** Всё однозначно — вещи уже ушли, отправителю остаётся это сообщение. */
    final case class Sent(message: String) extends Outcome
    /** В сумке нет ничего похожего. */
    case object Empty extends Outcome
    /** Нашлось, но такое не передают (трофеи). */
    final case class Forbidden(message: String) extends Outcome
    /** Надо уточнить кнопками: экипировка или несколько разных вещей. */
    case object NeedPick extends Outcome
  }

  val live: ZLayer[InventoryRepository with UserRepository with Parcels with Players with SceneContent, Nothing, Transfers] =
    ZLayer.fromFunction(Transfers(_, _, _, _, _))
}
