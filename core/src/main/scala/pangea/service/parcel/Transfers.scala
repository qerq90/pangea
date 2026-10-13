package pangea.service.parcel

import pangea.dao.hero.HeroDao
import pangea.engine.{Players, SceneContent}
import pangea.model.hero.{Hero, HeroId}
import pangea.model.item.{Item, ItemStack, ItemType}
import pangea.model.user.{User, UserId}
import pangea.repository.bank.BankRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.user.UserRepository
import pangea.service.chat.ChatCommand
import pangea.service.chat.ChatCommand.Money
import pangea.service.payout.Payouts
import pangea.service.purse.Purse
import zio.{Task, ZIO, ZLayer}

/** Кому и что передают. Имя получателя берём из ВК один раз и носим с собой:
 *  игроку важно видеть, кому он отдаёт. */
final case class TransferTarget(heroId: HeroId, userId: UserId, name: String)

/** Общая часть передачи вещей: что подходит под запрос, можно ли отдать сразу
 *  и как, собственно, отдать. Этим пользуются и команда из беседы, и экран
 *  выбора в личке — правила у них одни. Деньги передаются здесь же
 *  ([[sendMoney]]), но своим путём: в сумке их нет. */
final case class Transfers(
  inventoryRepo: InventoryRepository,
  userRepo:      UserRepository,
  parcels:       Parcels,
  players:       Players,
  content:       SceneContent,
  heroDao:       HeroDao,
  bank:          BankRepository,
  payouts:       Payouts
) {

  private val purse = Purse(heroDao, Some(bank))

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

  /** Передать деньги. Отправитель платит сразу: серебро — сперва с рук,
    * нехватку из ячейки, как любые траты (см. [[Purse]]); дублоны — с рук.
    * Получателю серебро ложится в банковскую ячейку, а не приняла она — ждёт
    * его в городе, как выручка с аукциона (см. [[Payouts]]); дублоны ждут в
    * городе всегда: ячейка держит только серебро. Самого героя получателя здесь
    * не трогаем — его пишут только под его собственным локом. Зачислить не
    * вышло — деньги возвращаются отправителю. */
  def sendMoney(
    sender: User,
    hero:   Hero,
    target: TransferTarget,
    money:  Money,
    amount: Long
  ): Task[Transfers.Outcome] = {
    val line = Transfers.moneyLine(money, amount)
    charge(sender, hero, money, amount).flatMap {
      case None => notEnough(hero, money).map(Transfers.Outcome.Forbidden(_))
      case Some(refund) =>
        for {
          intoVault <- credit(target, money, amount).tapError(_ => refund)
          _         <- tellMoney(sender, target, line, intoVault)
        } yield Transfers.Outcome.Sent(content.format("transfer.money.sent",
          "money" -> line, "to" -> target.name))
    }
  }

  /** Списать с отправителя. Возвращает, как вернуть деньги назад, если
    * зачислить их не выйдет; `None` — столько у него нет, и ничего не списано. */
  private def charge(sender: User, hero: Hero, money: Money, amount: Long): Task[Option[Task[Unit]]] =
    money match {
      case Money.Silver =>
        purse.charge(sender.userId, hero, amount).map(_.map(paid =>
          heroDao.updateSilver(sender.userId, paid.silver + amount)))
      case Money.Doubloons =>
        if (hero.doubloons < amount) ZIO.none
        else heroDao.updateDoubloons(sender.userId, hero.doubloons - amount)
               .as(Some(heroDao.updateDoubloons(sender.userId, hero.doubloons)))
    }

  /** Зачислить получателю: `true` — легло в ячейку, `false` — ждёт в городе. */
  private def credit(target: TransferTarget, money: Money, amount: Long): Task[Boolean] =
    money match {
      case Money.Silver =>
        bank.depositSilver(target.heroId, amount).either.flatMap {
          case Right(_) => ZIO.succeed(true)
          case Left(_)  => payouts.queue(target.heroId, silver = amount).as(false)
        }
      case Money.Doubloons => payouts.queue(target.heroId, doubloons = amount).as(false)
    }

  /** Отказ, когда денег не хватает: сколько есть на самом деле. Серебро
    * считается вместе с ячейкой — его оттуда и доберут. */
  private def notEnough(hero: Hero, money: Money): Task[String] =
    money match {
      case Money.Silver =>
        purse.total(hero).map(have => content.format("transfer.money.notEnoughSilver", "have" -> have.toString))
      case Money.Doubloons =>
        ZIO.succeed(content.format("transfer.money.notEnoughDoubloons", "have" -> hero.doubloons.toString))
    }

  /** Письмо получателю — где теперь его деньги, — и строка в общий чат. */
  private def tellMoney(sender: User, target: TransferTarget, line: String, intoVault: Boolean): Task[Unit] =
    for {
      fromName <- players.getDisplayName(sender).orElse(ZIO.succeed(content.text("transfer.someone")))
      receiver <- userRepo.getUserById(target.userId).option.map(_.flatten)
      key       = if (intoVault) "transfer.money.noticeVault" else "transfer.money.noticeCity"
      _        <- ZIO.foreachDiscard(receiver)(r => players.notify(r, content.format(key,
                    "from" -> fromName, "money" -> line)).ignore)
      _        <- players.announce(content.format("transfer.money.announce",
                    "from"  -> mention(sender.vkId.value, fromName),
                    "to"    -> receiver.map(r => mention(r.vkId.value, target.name)).getOrElse(target.name),
                    "money" -> line)).ignore
    } yield ()

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
    /** Так не передать: трофеи из рук в руки не ходят, денег не хватает или
      * сумму не назвали. Отправителю остаётся это сообщение. */
    final case class Forbidden(message: String) extends Outcome
    /** Надо уточнить кнопками: экипировка или несколько разных вещей. */
    case object NeedPick extends Outcome
  }

  /** Сумма со значком валюты: 🪙 серебро, 🟡 дублоны — как везде в игре. */
  def moneyLine(money: Money, amount: Long): String = money match {
    case Money.Silver    => s"🪙 $amount"
    case Money.Doubloons => s"🟡 $amount"
  }

  val live: ZLayer[InventoryRepository with UserRepository with Parcels with Players with SceneContent
                     with HeroDao with BankRepository with Payouts, Nothing, Transfers] =
    ZLayer.fromFunction(Transfers(_, _, _, _, _, _, _, _))
}
