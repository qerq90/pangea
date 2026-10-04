package pangea.model.quest

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.model.item.{BrewKind, Gem, GemKind, Item, ItemDetails, ItemType, MaterialKind, Rarity, TrophyKind}
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
  */
sealed abstract class DailyKind(
  val key:  String,
  val npc:  DailyNpc,
  val goal: Long
) extends EnumEntry {

  /** Задание считает по счётчику героя, а не по прибавкам.
    *
    * Метод, а не параметр с умолчанием, и это важно: умолчание конструктора
    * живёт в компаньоне, и каждый вариант, который его опускал, залезал в `DailyKind$`
    * прямо во время того, как тот себя собирал (`findValues`). На одном потоке это
    * проходит, на двух — в `values` появляются null и всё падает в случайном месте. */
  def snap: Boolean = false

  /** Насколько часто поручение выпадает против прочих у того же горожанина.
    * По умолчанию поровну; больше единицы ставится там, где заказ должен быть
    * редким гостем. Метод, а не параметр с умолчанием: умолчания конструктора
    * живут в компаньоне, а варианту enum туда лучше не заглядывать. */
  def weight: Int = 1

  /** Сколько просят у этого героя. По умолчанию — [[goal]] для всех одинаково;
    * считается один раз, когда поручение выдано, и дальше живёт в самой записи:
    * выросший за день уровень не должен двигать уже начатое дело. */
  def goalFor(heroLvl: Long): Long = {
    val _ = heroLvl
    goal
  }

  /** Во сколько раз опыт за поручение больше обычного. Редкому заказу —
    * редкая плата: такую траву и найти труднее. */
  def expFactor: Int = 1

  /** Сколько дублонов платит за это Рахадим; у прочих горожан плата своя. */
  def doubloons: Long = 0L

  /** Платят ли за него из редких запасов (склянка Густаво). */
  def rareReward: Boolean = false
}

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
  case object BankLot extends DailyKind("lot", DailyNpc.Rakhadim, 1L) {
    override def doubloons: Long = DailyRates.LotDoubloons
  }

  /** Принести реликвию названной расы: такие вещи Рахадим держит отдельно. */
  case object BankRelic extends DailyKind("relic", DailyNpc.Rakhadim, 1L) with DailyBring {
    override def doubloons: Long = DailyRates.BringDoubloons
    override def picks: List[String] = Race.mortals.map(_.entryName).toList
    override def pickName(pick: String): String =
      Race.withNameOption(pick).map(_.genitivePlural).getOrElse(pick)
    def accepts(item: Item, pick: Option[String]): Boolean = item.details match {
      case ItemDetails.Trophy(race, TrophyKind.Relic, _) => pick.forall(_ == race)
      case _                                             => false
    }
  }

  /** Принести названный самоцвет, и непременно надколотый: целые к банкиру
    * приходят и без героя. Череп сюда не идёт — это не камень, а то, что от
    * черепа осталось. */
  case object BankGem extends DailyKind("gem", DailyNpc.Rakhadim, 1L) with DailyBring {
    override def doubloons: Long = DailyRates.BringDoubloons
    override def picks: List[String] =
      GemKind.values.filterNot(_ == GemKind.Skull).map(_.entryName).toList
    override def pickName(pick: String): String =
      GemKind.withNameOption(pick).map(k => Gem.gradeName(k, Gem.MinGrade)).getOrElse(pick)
    def accepts(item: Item, pick: Option[String]): Boolean =
      item.gem.exists(g => g.grade == Gem.MinGrade && pick.forall(_ == g.kind.entryName))
  }

  // ── Ришелье: оборот на прилавке и заказ городской стражи ──────────────────
  /** Продать ему вещи поштучно. */
  case object SellItems extends DailyKind("sell", DailyNpc.Richelieu, 3L)

  /** Сдать хлам разом — то, что он и просил в своё время. */
  case object SellJunk extends DailyKind("junk", DailyNpc.Richelieu, 5L)

  /** Купить у него хоть что-нибудь: прилавок должен пустеть. */
  case object BuyFromMerchant extends DailyKind("buy", DailyNpc.Richelieu, 1L)

  /** Оружие для стражи — простое: серое, белое или зелёное. Дороже Ришелье не
    * возьмёт, на караул хорошую сталь он переводить не станет. */
  case object GuardWeapons extends DailyKind("weapons", DailyNpc.Richelieu, 3L) with DailyBring {
    def accepts(item: Item, pick: Option[String]): Boolean = {
      val _ = pick
      (item.itemType == ItemType.Weapon || item.itemType == ItemType.AdditionalWeapon) &&
        Rarity.atMost(item.rarity, DailyRates.GuardRarity)
    }
  }

  /** Нагрудники туда же и с тем же потолком: стражу прикрывают по груди,
    * остальное её дело. */
  case object GuardArmor extends DailyKind("armor", DailyNpc.Richelieu, 3L) with DailyBring {
    def accepts(item: Item, pick: Option[String]): Boolean = {
      val _ = pick
      item.itemType == ItemType.ChestPlate && Rarity.atMost(item.rarity, DailyRates.GuardRarity)
    }
  }

  // ── Горн: железо должно работать ───────────────────────────────────────────
  /** Убить мобов — Горн считает это проверкой стали. */
  case object HornKills extends DailyKind("kills", DailyNpc.Horn, 20L) {
    override def snap: Boolean = true
  }

  /** Набрать репутации в гильдии за день — столько, сколько дали бы десять
    * мешков с пожитками по уровню героя. Число растёт вместе с героем: статичные
    * полсотни для сотого уровня — не наказ, а недоразумение. */
  case object HornReputation extends DailyKind("reputation", DailyNpc.Horn, 50L) {
    override def snap: Boolean = true
    override def goalFor(heroLvl: Long): Long =
      DailyRates.sacks(DailyRates.HornGoalSacks, heroLvl)
  }

  /** Заказать у него улучшение: молот не должен стынуть. */
  case object HornUpgrade extends DailyKind("upgrade", DailyNpc.Horn, 1L)

  // ── Густаво: всё, что растёт ──────────────────────────────────────────────
  /** Травы, любые: котёл не разбирает. */
  case object HerbsAny extends DailyKind("herbs", DailyNpc.Gustavo, 6L) with DailyBring {
    override def weight: Int = DailyRates.GustavoPlainWeight
    def accepts(item: Item, pick: Option[String]): Boolean = {
      val _ = pick
      item.material.exists(_.isHerb)
    }
  }

  /** Названная трава первого ранга — ровно та, что нужна под рецепт. */
  case object HerbNamed extends DailyKind("herb", DailyNpc.Gustavo, 2L) with DailyBring {
    override def weight: Int = DailyRates.GustavoPlainWeight
    override def picks: List[String] = MaterialKind.herbsOfRank(1).map(_.entryName).toList
    override def pickName(pick: String): String =
      MaterialKind.withNameOption(pick).map(_.displayName).getOrElse(pick)
    def accepts(item: Item, pick: Option[String]): Boolean =
      item.material.exists(m => m.herbRank == 1 && pick.forall(_ == m.entryName))
  }

  /** Редкая трава второго ранга, одна. Заказ нечастый — примерно раз в десять
    * дней, — и платит он за него по-другому. */
  case object HerbRare extends DailyKind("rare", DailyNpc.Gustavo, 1L) with DailyBring {
    override def weight: Int        = DailyRates.GustavoRareWeight
    override def expFactor: Int     = DailyRates.RareExpFactor
    override def rareReward: Boolean = true
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
  goal:  Long           = 0L,
  taken: Boolean        = false,
  done:  Boolean        = false
) {
  /** Сколько просят именно по этой записи. Ноль — цель не записали (старая
    * строка в jsonb), тогда берём общую для поручения. */
  def need: Long = if (goal > 0L) goal else kind.goal

  def progress: Long = count.min(need)

  def ready: Boolean = !done && count >= need

  def plus(n: Long): DailyTask = if (done) this else copy(count = count + n)

  /** Счётчиковое задание: прогресс — разница с тем, что было при взятии, но
    * счёт только растёт. Репутацию тратят там же, в гильдии, и без этого
    * прокачка у Горна съедала бы его же наказ. */
  def withCounter(now: Long): DailyTask =
    if (!kind.snap || done) this else copy(count = count.max((now - from).max(0L)))

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

  /** Сколько ещё просят сверх уже сданного. */
  def left: Long = (need - count).max(0L)

  /** Что из сумки уйдёт горожанину за одну сдачу: сколько не хватает, не
    * больше. Отдаём худшее из подходящего — просят «любое», и лишаться из-за
    * этого лучшего клинка герою незачем. */
  def toGive(items: List[Item]): List[Item] =
    bring.fold(List.empty[Item])(b =>
      items.filter(b.accepts(_, pick))
        .sortBy(i => (Rarity.order(i.rarity), i.lvl, i.gem.map(_.grade).getOrElse(0)))
        .take(left.toInt))
}

object DailyTask {
  implicit val encoder: Encoder[DailyTask] = (t: DailyTask) =>
    Json.obj("kind" -> t.kind.asJson, "day" -> t.day.asJson, "count" -> t.count.asJson,
      "from" -> t.from.asJson, "pick" -> t.pick.asJson, "goal" -> t.goal.asJson,
      "taken" -> t.taken.asJson, "done" -> t.done.asJson)

  implicit val decoder: Decoder[DailyTask] = (c: HCursor) =>
    for {
      kind  <- c.get[DailyKind]("kind")
      day   <- c.getOrElse[Long]("day")(0L)
      count <- c.getOrElse[Long]("count")(0L)
      from  <- c.getOrElse[Long]("from")(0L)
      pick  <- c.getOrElse[Option[String]]("pick")(None)
      goal  <- c.getOrElse[Long]("goal")(0L)
      taken <- c.getOrElse[Boolean]("taken")(false)
      done  <- c.getOrElse[Boolean]("done")(false)
    } yield DailyTask(kind, day, count, from, pick, goal, taken, done)
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

  /** Опыт за поручение — доля уровня, а не отдельная линейка: пятидесятая
    * часть порога, то есть два процента уровня. Так заказ стоит одинаково что на
    * третьем уровне, что на сотом, и не перекашивает лестницу ни там, ни там.
    * На первых двух уровнях эта доля совсем мала — держит пол. */
  def exp(heroLvl: Long): Long = (pangea.model.hero.Hero.neededExpForLevel(heroLvl) / 50L).max(5L)

  /** То же с поправкой на само поручение: за редкое платят вдвое. */
  def exp(heroLvl: Long, kind: DailyKind): Long = exp(heroLvl) * kind.expFactor.toLong

  /** Серебро Ришелье — по уровню героя, и мерено его же прилавком: заказ стоит
    * примерно шесть зелёных вещей своего уровня (зелёная уходит ему за
    * `(ур + 5) × 1.2 × 4`). Не доход, а плата за работу. */
  def silver(heroLvl: Long): Long = 30L * (heroLvl + 5L)

  /** Дублоны Рахадима. С уровнем не растут: банкир платит за вещь, а не за
    * заслуги, — за лот поменьше, за принесённое побольше. */
  val LotDoubloons: Long   = 2L
  val BringDoubloons: Long = 5L

  /** Во сколько раз редкий заказ Густаво щедрее прочих на опыт. */
  val RareExpFactor: Int = 2

  /** Как часто Густаво просит редкую траву: девять, девять и два — примерно
    * один такой заказ на десять дней. */
  val GustavoPlainWeight: Int = 9
  val GustavoRareWeight: Int  = 2

  /** Чем платит Густаво. За простой заказ — склянка из простых трав, за редкую
    * траву — из тех, что варятся на редких: чем платить, он решает по дню. */
  val plainBrews: IndexedSeq[BrewKind] = BrewKind.rank1
  val rareBrews: IndexedSeq[BrewKind] =
    BrewKind.values.filter(k => k.base.isEmpty && k.recipe.exists(_.herbRank >= 2))

  def brew(rare: Boolean, seed: Long): BrewKind = {
    val pool = if (rare) rareBrews else plainBrews
    pool((((seed % pool.size.toLong).toInt + pool.size) % pool.size).toInt)
  }

  /** У Горна всё мерено мешками с пожитками по уровню героя: десять он
    * просит, шесть отдаёт. Ставка та же, по которой Гильдия платит за трофей. */
  val HornGoalSacks: Long   = 10L
  val HornRewardSacks: Long = 6L

  /** Сколько репутации в стольких мешках по этому уровню. */
  def sacks(count: Long, heroLvl: Long): Long =
    count * TrophyKind.reputationFor(TrophyKind.Sack.coef, heroLvl)

  /** Репутация от Горна за сданный наказ. */
  def reputation(heroLvl: Long): Long = sacks(HornRewardSacks, heroLvl)

  /** Выше этой редкости Ришелье на городскую стражу ничего не берёт. */
  val GuardRarity: pangea.model.item.Rarity = pangea.model.item.Rarity.Green
}
