package pangea.model.quest

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.model.item.{BrewKind, GemKind, Item, ItemDetails, ItemType, MaterialKind, Rarity, TrophyKind}
import pangea.model.monster.Race

/** Горожанин, у которого есть ежедневное поручение. Ключ — имя секции в
  * `scenes.yaml` (`daily.<key>`) и ключ в `heroes.daily_quests`. */
sealed abstract class DailyNpc(val key: String) extends EnumEntry
object DailyNpc extends Enum[DailyNpc] {
  val values: IndexedSeq[DailyNpc] = findValues

  /** Банкир Рахадим: ему важно, чтобы торги шли, а редкое не пылилось по сумкам. */
  case object Rakhadim extends DailyNpc("rakhadim")

  /** Ришелье: ему важен оборот на прилавке — и заказ городской стражи. */
  case object Richelieu extends DailyNpc("richelieu")

  /** Мастер Горн: ему важно, чтобы железо работало, а не пылилось. */
  case object Horn extends DailyNpc("horn")

  /** Густаво: ему важны травы, и чем точнее, тем лучше. */
  case object Gustavo extends DailyNpc("gustavo")

  implicit val encoder: Encoder[DailyNpc] = (n: DailyNpc) => n.key.asJson
  implicit val decoder: Decoder[DailyNpc] = (c: HCursor) =>
    c.as[String].flatMap(k => values.find(_.key == k)
      .toRight(io.circe.DecodingFailure(s"Unknown daily npc '$k'", Nil)))
}

/** Само поручение: чего именно просят и сколько раз это надо сделать.
  *
  * Считается тремя путями. Что видно по самому герою (убитые, репутация) —
  * сверяется со снимком, снятым при взятии ([[snap]]). Что делается в городе —
  * прибавляется там, где дело и делается: у прилавка, в кузне, на торгах. А
  * поручения «принеси» ([[DailyBring]]) вовсе ничего не копят: у них прогресс
  * и есть то, что лежит в сумке.
  *
  * @param key   имя в `scenes.yaml`: `daily.<npc>.tasks.<key>`
  * @param goal  сколько нужно
  * @param snap  задание считает по счётчику героя, а не по прибавкам
  */
sealed abstract class DailyKind(
  val key:  String,
  val npc:  DailyNpc,
  val goal: Long,
  val snap: Boolean = false
) extends EnumEntry

/** Поручение «принеси»: горожанин называет вещь, герой достаёт её из сумки и
  * отдаёт. Прогресс нигде не хранится — он и есть содержимое сумки, а при
  * сдаче названное остаётся у горожанина.
  *
  * Часть таких поручений решается не до конца: чья реликвия, какой камень,
  * какая трава. Из чего выбирать — [[picks]], что выпало — `pick` у
  * [[DailyTask]]. */
sealed trait DailyBring { self: DailyKind =>

  /** Из чего выбирается уточнение; пусто — уточнять нечего. */
  def picks: List[String] = Nil

  /** Как назвать уточнение в тексте поручения. */
  def pickName(pick: String): String = pick

  /** Годится ли вещь из сумки. */
  def accepts(item: Item, pick: Option[String]): Boolean
}

object DailyKind extends Enum[DailyKind] {
  val values: IndexedSeq[DailyKind] = findValues

  // ── Рахадим: торги должны идти, редкое — лежать на виду ───────────────────
  /** Выставить лот на аукцион: пустой зал банкиру дороже пустой ячейки. */
  case object BankLot extends DailyKind("lot", DailyNpc.Rakhadim, 1L)

  /** Принести реликвию названной расы: такие вещи Рахадим держит отдельно. */
  case object BankRelic extends DailyKind("relic", DailyNpc.Rakhadim, 1L) with DailyBring {
    override def picks: List[String] = Race.mortals.map(_.entryName).toList
    override def pickName(pick: String): String =
      Race.withNameOption(pick).map(_.genitivePlural).getOrElse(pick)
    def accepts(item: Item, pick: Option[String]): Boolean = item.details match {
      case ItemDetails.Trophy(race, TrophyKind.Relic, _) => pick.forall(_ == race)
      case _                                             => false
    }
  }

  /** Принести названный самоцвет — грейд не важен, важна порода. Череп сюда не
    * идёт: это не камень, а то, что от черепа осталось. */
  case object BankGem extends DailyKind("gem", DailyNpc.Rakhadim, 1L) with DailyBring {
    override def picks: List[String] =
      GemKind.values.filterNot(_ == GemKind.Skull).map(_.entryName).toList
    override def pickName(pick: String): String =
      GemKind.withNameOption(pick).map(_.baseName.toLowerCase).getOrElse(pick)
    def accepts(item: Item, pick: Option[String]): Boolean =
      item.gem.exists(g => pick.forall(_ == g.kind.entryName))
  }

  // ── Ришелье: оборот на прилавке и заказ городской стражи ──────────────────
  /** Продать ему вещи поштучно. */
  case object SellItems extends DailyKind("sell", DailyNpc.Richelieu, 3L)

  /** Сдать хлам разом — то, что он и просил в своё время. */
  case object SellJunk extends DailyKind("junk", DailyNpc.Richelieu, 5L)

  /** Купить у него хоть что-нибудь: прилавок должен пустеть. */
  case object BuyFromMerchant extends DailyKind("buy", DailyNpc.Richelieu, 1L)

  /** Оружие для стражи: любое, лишь бы било. Качество не при чём — хорошее на
    * них переводить Ришелье и сам не даст. */
  case object GuardWeapons extends DailyKind("weapons", DailyNpc.Richelieu, 3L) with DailyBring {
    def accepts(item: Item, pick: Option[String]): Boolean = {
      val _ = pick
      item.itemType == ItemType.Weapon || item.itemType == ItemType.AdditionalWeapon
    }
  }

  /** Доспехи туда же: страже всё равно, чем прикрываться. */
  case object GuardArmor extends DailyKind("armor", DailyNpc.Richelieu, 3L) with DailyBring {
    def accepts(item: Item, pick: Option[String]): Boolean = {
      val _ = pick
      ItemType.defenceItems.contains(item.itemType)
    }
  }

  // ── Горн: железо должно работать ───────────────────────────────────────────
  /** Убить мобов — Горн считает это проверкой стали. */
  case object HornKills extends DailyKind("kills", DailyNpc.Horn, 20L, snap = true)

  /** Набрать репутации в гильдии за день. */
  case object HornReputation extends DailyKind("reputation", DailyNpc.Horn, 50L, snap = true)

  /** Заказать у него улучшение: молот не должен стынуть. */
  case object HornUpgrade extends DailyKind("upgrade", DailyNpc.Horn, 1L)

  // ── Густаво: всё, что растёт ──────────────────────────────────────────────
  /** Травы, любые: котёл не разбирает. */
  case object HerbsAny extends DailyKind("herbs", DailyNpc.Gustavo, 6L) with DailyBring {
    def accepts(item: Item, pick: Option[String]): Boolean = {
      val _ = pick
      item.material.exists(_.isHerb)
    }
  }

  /** Названная трава первого ранга — ровно та, что нужна под рецепт. */
  case object HerbNamed extends DailyKind("herb", DailyNpc.Gustavo, 2L) with DailyBring {
    override def picks: List[String] = MaterialKind.herbsOfRank(1).map(_.entryName).toList
    override def pickName(pick: String): String =
      MaterialKind.withNameOption(pick).map(_.displayName).getOrElse(pick)
    def accepts(item: Item, pick: Option[String]): Boolean =
      item.material.exists(m => m.herbRank == 1 && pick.forall(_ == m.entryName))
  }

  /** Редкая трава второго ранга, одна. За такую и платит по-другому. */
  case object HerbRare extends DailyKind("rare", DailyNpc.Gustavo, 1L) with DailyBring {
    def accepts(item: Item, pick: Option[String]): Boolean = {
      val _ = pick
      item.material.exists(_.herbRank >= 2)
    }
  }

  def of(npc: DailyNpc): List[DailyKind] = values.filter(_.npc == npc).toList

  def byKey(npc: DailyNpc, key: String): Option[DailyKind] =
    values.find(k => k.npc == npc && k.key == key)

  implicit val encoder: Encoder[DailyKind] = (k: DailyKind) => k.entryName.asJson
  implicit val decoder: Decoder[DailyKind] = (c: HCursor) =>
    c.as[String].flatMap(n => values.find(_.entryName == n)
      .toRight(io.circe.DecodingFailure(s"Unknown daily kind '$n'", Nil)))
}

/** Поручение одного горожанина на сегодня: что выпало, сколько сделано и
  * сдано ли уже. `day` — номер суток по Москве, по нему и видно, что пора
  * катать новое. `from` — снимок счётчика героя для заданий, которые считаются
  * по нему (убитые, репутация). `pick` — уточнение для «принеси»: чья
  * реликвия, какой камень, какая трава. */
final case class DailyTask(
  kind:  DailyKind,
  day:   Long,
  count: Long           = 0L,
  from:  Long           = 0L,
  pick:  Option[String] = None,
  taken: Boolean        = false,
  done:  Boolean        = false
) {
  def progress: Long = count.min(kind.goal)

  def ready: Boolean = !done && count >= kind.goal

  def plus(n: Long): DailyTask = if (done) this else copy(count = count + n)

  /** Счётчиковое задание: прогресс — разница с тем, что было при взятии. */
  def withCounter(now: Long): DailyTask =
    if (!kind.snap || done) this else copy(count = (now - from).max(0L))

  /** Поручение «принеси», если это оно. */
  def bring: Option[DailyBring] = kind match {
    case b: DailyBring => Some(b)
    case _             => None
  }

  /** Как названо то, что просят: раса реликвии, камень, трава. */
  def what: String = (bring, pick) match {
    case (Some(b), Some(p)) => b.pickName(p)
    case _                  => ""
  }

  /** Прогресс «принеси» — это просто то, что лежит в сумке. */
  def inBag(items: List[Item]): DailyTask =
    bring.fold(this)(b => copy(count = items.count(b.accepts(_, pick)).toLong))

  /** Что именно уйдёт горожанину при сдаче. Отдаём худшее из подходящего:
    * просят «любое», и лишаться из-за этого лучшего клинка герою незачем. */
  def toGive(items: List[Item]): List[Item] =
    bring.fold(List.empty[Item])(b =>
      items.filter(b.accepts(_, pick))
        .sortBy(i => (Rarity.values.indexOf(i.rarity), i.lvl, i.gem.map(_.grade).getOrElse(0)))
        .take(kind.goal.toInt))
}

object DailyTask {
  implicit val encoder: Encoder[DailyTask] = (t: DailyTask) =>
    Json.obj("kind" -> t.kind.asJson, "day" -> t.day.asJson, "count" -> t.count.asJson,
      "from" -> t.from.asJson, "pick" -> t.pick.asJson,
      "taken" -> t.taken.asJson, "done" -> t.done.asJson)

  implicit val decoder: Decoder[DailyTask] = (c: HCursor) =>
    for {
      kind  <- c.get[DailyKind]("kind")
      day   <- c.getOrElse[Long]("day")(0L)
      count <- c.getOrElse[Long]("count")(0L)
      from  <- c.getOrElse[Long]("from")(0L)
      pick  <- c.getOrElse[Option[String]]("pick")(None)
      taken <- c.getOrElse[Boolean]("taken")(false)
      done  <- c.getOrElse[Boolean]("done")(false)
    } yield DailyTask(kind, day, count, from, pick, taken, done)
}

/** Все сегодняшние поручения героя: по одному на горожанина. Живёт в
  * `heroes.daily_quests`. */
final case class DailyQuests(tasks: Map[String, DailyTask] = Map.empty) {
  def of(npc: DailyNpc): Option[DailyTask] = tasks.get(npc.key)

  def updated(npc: DailyNpc, task: DailyTask): DailyQuests =
    copy(tasks = tasks.updated(npc.key, task))

  /** Поручение на сегодня, если оно от сегодняшнего дня. */
  def today(npc: DailyNpc, day: Long): Option[DailyTask] = of(npc).filter(_.day == day)
}

object DailyQuests {
  val empty: DailyQuests = DailyQuests()

  implicit val encoder: Encoder[DailyQuests] = (q: DailyQuests) => q.tasks.asJson
  implicit val decoder: Decoder[DailyQuests] = (c: HCursor) =>
    Right(DailyQuests(c.as[Map[String, DailyTask]].getOrElse(Map.empty)))
}

/** Числа ежедневок. */
object DailyRates {

  /** Сутки считаем по Москве: день кончается в полночь по ней, а не по UTC. */
  val MoscowOffsetMs: Long = 3L * 60L * 60L * 1000L
  val DayMs: Long          = 24L * 60L * 60L * 1000L

  /** Номер суток по Москве — по нему видно, пора ли катать новое поручение. */
  def dayOf(nowMs: Long): Long = (nowMs + MoscowOffsetMs) / DayMs

  /** Сколько миллисекунд до полуночи по Москве. */
  def untilNextDay(nowMs: Long): Long = (dayOf(nowMs) + 1) * DayMs - MoscowOffsetMs - nowMs

  /** Опыт за поручение: по уровню героя, чтобы и на поздних этажах не зря. */
  def exp(heroLvl: Long): Long = 50L + heroLvl * 25L

  /** Серебро Ришелье — по уровню героя. */
  def silver(heroLvl: Long): Long = 200L * (heroLvl + 5L)

  /** Дублоны Рахадима. С уровнем не растут: банкир платит за вещь, а не за
    * заслуги, — за лот поменьше, за принесённое побольше. */
  val lotDoubloons: Long   = 2L
  val bringDoubloons: Long = 5L

  def doubloons(kind: DailyKind): Long =
    if (kind == DailyKind.BankLot) lotDoubloons else bringDoubloons

  /** Чем платит Густаво. За простой заказ — склянка из простых трав, за редкую
    * траву — из тех, что варятся на редких: чем платить, он решает по дню. */
  val plainBrews: IndexedSeq[BrewKind] = BrewKind.rank1
  val rareBrews: IndexedSeq[BrewKind] =
    BrewKind.values.filter(k => k.base.isEmpty && k.recipe.exists(_.herbRank >= 2))

  def brew(kind: DailyKind, seed: Long): BrewKind = {
    val pool = if (kind == DailyKind.HerbRare) rareBrews else plainBrews
    pool((((seed % pool.size.toLong).toInt + pool.size) % pool.size).toInt)
  }

  /** Репутация от Горна. */
  val reputation: Long = 40L
}
