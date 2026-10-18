package pangea.model.quest

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.model.hero.Hero

/** Раздел доски заданий: гильдия держит отдельную доску на каждые двадцать пять
  * уровней. Герою открыты своя и предыдущая ([[BoardTier.open]]); старшие видны,
  * но закрыты, и у каждой свой отказ (`questBoard.locked.<key>`), а на младших
  * отвечают, что пора уступить дорогу молодым (`questBoard.tooOld`).
  *
  * @param from первый уровень раздела
  * @param to   последний уровень раздела
  */
sealed abstract class BoardTier(val key: String, val from: Long, val to: Long) extends EnumEntry {
  def holds(lvl: Long): Boolean = lvl >= from && lvl <= to
  def title: String             = s"$from–$to"
}

object BoardTier extends Enum[BoardTier] {
  val values: IndexedSeq[BoardTier] = findValues

  case object Novice   extends BoardTier("novice",   1L,   25L)
  case object Seasoned extends BoardTier("seasoned", 26L,  50L)
  case object Veteran  extends BoardTier("veteran",  51L,  75L)
  case object Master   extends BoardTier("master",   76L,  100L)
  case object Elder    extends BoardTier("elder",    101L, 125L)
  case object Legend   extends BoardTier("legend",   126L, 150L)

  /** Доска по уровню героя: выше последней не бывает — потолок закрывает её. */
  def of(lvl: Long): BoardTier = values.find(_.holds(lvl)).getOrElse(Legend)

  def byKey(key: String): Option[BoardTier] = values.find(_.key == key)

  /** Какие доски открыты герою этого уровня: своя и предыдущая (у новичков
    * предыдущей нет). Двадцать шестой берёт и с 1–25, и с 26–50; пятьдесят
    * первый — с 26–50 и 51–75. */
  def open(lvl: Long): List[BoardTier] = {
    val mine = of(lvl)
    values.lift(values.indexOf(mine) - 1).toList :+ mine
  }
}

/** Вид задания с доски. Сложность показывается знаками [[Difficulty.render]]. */
sealed abstract class BoardKind(val key: String, val difficulty: Int) extends EnumEntry {
  /** Называет ли задание расу: у трофейного она в самом тексте. */
  def needsRace: Boolean = false

  /** Называет ли задание отвар: гномам нужен один определённый, и какой
    * именно — решается, когда объявление вывешивают. */
  def needsBrew: Boolean = false

  /** Выездное: герой уходит на него прямо от доски и добирается по дороге
    * ([[pangea.model.state.StateType.QuestRoad]]). Прочие он делает попутно, в
    * лабиринте, когда они ему встретятся. */
  def away: Boolean = false

  /** Своя сложность у каждого объявления: у выездных она катается вместе с
    * уровнем задания, у остальных одна на весь вид. */
  def rolledLvl: Boolean = false

  /** Сдаётся прямо на доске — трофей, ящик склянок. Прочие отмечаются там, где
    * дело и делается (см. `BoardProgress`), — по виду, на той доске, где взяты. */
  def handedIn: Boolean = false
}

object BoardKind extends Enum[BoardKind] {
  val values: IndexedSeq[BoardKind] = findValues

  /** «Принести трофей расы X» — то, что доска просила и раньше. */
  case object Trophy extends BoardKind("trophy", 1) {
    override def needsRace: Boolean = true
    override def handedIn: Boolean  = true
  }

  /** «Найти караван и перебить охрану» — тихая кража не в счёт. */
  case object CaravanRout extends BoardKind("caravan", 15)

  /** «Найти пещеру и выбить всех до последнего» — раса пещеры не важна. */
  case object CaveClear extends BoardKind("cave", 15)

  /** «Припасы в дальний поход»: гномья дружина берёт десять склянок одного
    * толка — и только все разом. Сдаётся на доске, ходить никуда не надо. */
  case object DwarfSupply extends BoardKind("dwarves", 5) {
    override def needsBrew: Boolean = true
    override def handedIn: Boolean  = true
  }

  /** «Разбойники в городе» — выездное: герой уходит ждать их в подворотне.
    * Сложность, как и у канализации, своя у каждого объявления. */
  case object Thieves extends BoardKind("thieves", 1) {
    override def away: Boolean      = true
    override def rolledLvl: Boolean = true
  }

  /** «Крысы в канализации» — выездное: герой уходит туда прямо от доски.
    * Сложность у каждого объявления своя, по уровню задания, поэтому ставка
    * вида здесь служебная (см. [[BoardSlot.difficulty]]). */
  case object SewerRats extends BoardKind("sewer", 1) {
    override def away: Boolean      = true
    override def rolledLvl: Boolean = true
  }

  /** «Уничтожение некроманта» — выездное, как и канализация: пещера, где вся
    * родня одной расы давно поднята, а сам Некромант ждёт на нижнем ярусе.
    * Сложность у каждого объявления своя, по уровню задания. */
  case object NecromancerHunt extends BoardKind("necro", 1) {
    override def away: Boolean      = true
    override def rolledLvl: Boolean = true
  }

  def byKey(key: String): Option[BoardKind] = values.find(_.key == key)

  implicit val encoder: Encoder[BoardKind] = (k: BoardKind) => k.key.asJson
  implicit val decoder: Decoder[BoardKind] = (c: HCursor) =>
    c.as[String].flatMap(k => byKey(k).toRight(io.circe.DecodingFailure(s"Unknown board kind '$k'", Nil)))
}

/** Одно объявление на доске: что просят, взято ли, сделано ли.
  *
  * `race` — раса трофея у трофейного задания. `done` ставится там, где дело и
  * делается (караван — после разгрома охраны, пещера — после зачистки), а
  * награду герой забирает сам, вернувшись к доске. */
final case class BoardSlot(
  kind:  BoardKind,
  race:  Option[String] = None,
  taken: Boolean        = false,
  done:  Boolean        = false,
  lvl:   Long           = 0L,
  // Какой отвар просят и чьё имя стоит на заказе. Имя — бросок, по которому
  // выбирается строка из списка дружин (`questBoard.bands`): так список можно
  // править, не трогая уже вывешенные объявления.
  brew:  Option[String] = None,
  band:  Int            = 0
) {
  /** Взято и сделано, но ещё не оплачено. */
  def ready: Boolean = taken && done

  /** Сложность этого объявления в знаках: у выездных она своя, по уровню
    * задания, у остальных — одна на весь вид. */
  def difficulty: Int = if (kind.rolledLvl && lvl > 0L) lvl.toInt else kind.difficulty
}

object BoardSlot {
  implicit val encoder: Encoder[BoardSlot] = (s: BoardSlot) => Json.obj(
    "kind" -> s.kind.asJson, "race" -> s.race.asJson,
    "taken" -> s.taken.asJson, "done" -> s.done.asJson, "lvl" -> s.lvl.asJson,
    "brew" -> s.brew.asJson, "band" -> s.band.asJson)

  implicit val decoder: Decoder[BoardSlot] = (c: HCursor) =>
    for {
      kind  <- c.get[BoardKind]("kind")
      race  <- c.getOrElse[Option[String]]("race")(None)
      taken <- c.getOrElse[Boolean]("taken")(false)
      done  <- c.getOrElse[Boolean]("done")(false)
      lvl   <- c.getOrElse[Long]("lvl")(0L)
      brew  <- c.getOrElse[Option[String]]("brew")(None)
      band  <- c.getOrElse[Int]("band")(0)
    } yield BoardSlot(kind, race, taken, done, lvl, brew, band)
}

/** Доска одного раздела на эту неделю: номер недели, раздел и восемь
  * объявлений. Доски героя лежат вместе ([[BoardDesk]]).
  *
  * Неделя кончилась — доска переписывается целиком, и взятое, но не сданное
  * сгорает вместе с ней: неделя и есть срок. Герой ушёл на два раздела вперёд —
  * доска ему больше не открыта и уходит со стола. */
final case class BoardData(
  week:  Long            = -1L,
  tier:  String          = BoardTier.Novice.key,
  slots: List[BoardSlot] = Nil
) {
  def board: BoardTier = BoardTier.byKey(tier).getOrElse(BoardTier.Novice)

  def fresh(nowMs: Long, lvl: Long): Boolean =
    week == BoardRates.weekOf(nowMs) && BoardTier.open(lvl).contains(board)

  def slot(idx: Int): Option[BoardSlot] = slots.lift(idx)

  def updated(idx: Int)(f: BoardSlot => BoardSlot): BoardData =
    copy(slots = slots.zipWithIndex.map { case (s, i) => if (i == idx) f(s) else s })

  /** Отметить сделанным первое взятое и незакрытое задание такого вида: по нему
    * герой и ходил. Ничего не взято — ничего не меняем. */
  def markDone(kind: BoardKind): BoardData =
    slots.indexWhere(s => s.kind == kind && s.taken && !s.done) match {
      case -1  => this
      case idx => updated(idx)(_.copy(done = true))
    }

  /** Убрать с доски первое взятое и незакрытое задание такого вида: цель
    * потеряна, второй раз её не найти. Ничего не взято — ничего не меняем. */
  def markFailed(kind: BoardKind): BoardData =
    slots.indexWhere(s => s.kind == kind && s.taken && !s.done) match {
      case -1  => this
      case idx => copy(slots = slots.patch(idx, Nil, 1))
    }

  /** Взято ли сейчас задание такого вида (и ещё не закрыто). */
  def hunting(kind: BoardKind): Boolean =
    slots.exists(s => s.kind == kind && s.taken && !s.done)
}

object BoardData {
  val empty: BoardData = BoardData()

  implicit val encoder: Encoder[BoardData] = (d: BoardData) => Json.obj(
    "week" -> d.week.asJson, "tier" -> d.tier.asJson, "slots" -> d.slots.asJson)

  /** Декодер рукописный, с запасными значениями: старая доска (трофей, срок в
    * двадцать часов) читается как пустая и переписывается при первом же заходе. */
  implicit val decoder: Decoder[BoardData] = (c: HCursor) =>
    for {
      week  <- c.getOrElse[Long]("week")(-1L)
      tier  <- c.getOrElse[String]("tier")(BoardTier.Novice.key)
      slots <- c.getOrElse[List[BoardSlot]]("slots")(Nil)
    } yield BoardData(week, tier, slots)
}

/** Доски героя (`heroes.quest_data`): своя и предыдущая, у каждой свои восемь
  * объявлений и своя неделя. Брать можно с обеих разом.
  *
  * Отметки из дела (караван разбит, пещера зачищена) ищут взятое объявление
  * такого вида на любой из досок: поэтому одно и то же такое дело на двух
  * досках разом не берут (см. `QuestBoardState.take`). */
final case class BoardDesk(boards: List[BoardData] = Nil) {

  def of(tier: BoardTier): Option[BoardData] = boards.find(_.board == tier)

  /** Положить доску на стол вместо прежней доски того же раздела. */
  def withBoard(data: BoardData): BoardDesk =
    copy(boards = (boards.filterNot(_.board == data.board) :+ data).sortBy(b => BoardTier.values.indexOf(b.board)))

  /** Только то, что ещё в ходу: доски этой недели и открытые герою. */
  def fresh(nowMs: Long, lvl: Long): BoardDesk = copy(boards = boards.filter(_.fresh(nowMs, lvl)))

  /** Взято ли сейчас задание такого вида (и ещё не закрыто) — на любой доске. */
  def hunting(kind: BoardKind): Boolean = boards.exists(_.hunting(kind))

  /** Отметить сделанным взятое задание такого вида — на той доске, где оно взято. */
  def markDone(kind: BoardKind): BoardDesk =
    boards.find(_.hunting(kind)).fold(this)(b => withBoard(b.markDone(kind)))

  /** Убрать с доски взятое задание такого вида: цель потеряна. */
  def markFailed(kind: BoardKind): BoardDesk =
    boards.find(_.hunting(kind)).fold(this)(b => withBoard(b.markFailed(kind)))
}

object BoardDesk {
  val empty: BoardDesk = BoardDesk()

  implicit val encoder: Encoder[BoardDesk] = (d: BoardDesk) => Json.obj("boards" -> d.boards.asJson)

  /** Запись прежнего вида — одна доска без обёртки — читается как стол с этой
    * одной доской. */
  implicit val decoder: Decoder[BoardDesk] = (c: HCursor) =>
    if (c.downField("boards").succeeded) c.getOrElse[List[BoardData]]("boards")(Nil).map(BoardDesk(_))
    else c.as[BoardData].map(d => BoardDesk(List(d)))
}

/** Трофеи в счёт доски. */
object BoardTrophy {

  /** Коэффициент вида трофея: по нему считается и репутация в гильдии, и опыт
    * за трофейное задание. У не-трофея его нет. */
  def coef(item: pangea.model.item.Item): Double = item.details match {
    case t: pangea.model.item.ItemDetails.Trophy => t.coefValue
    case _                                       => 0.0
  }

  /** Какой трофей уйдёт в счёт задания по расе: из подходящих берём с
    * наибольшим коэффициентом вида, при равных — старший по уровню. Он же
    * принесёт больше опыта (`5 + Ур. × коэффициент`). */
  def bestFor(items: List[pangea.model.item.Item], raceName: String): Option[pangea.model.item.Item] =
    items
      .filter(_.details match {
        case pangea.model.item.ItemDetails.Trophy(race, _, _) => race == raceName
        case _                                                => false
      })
      .sortBy(i => (-coef(i), -i.lvl))
      .headOption
}

/** Числа доски. */
object BoardRates {

  /** Больше стольких объявлений на доске не висит. */
  val Slots: Int = 8

  /** Трофейные задания — только на доске новичков, и там их всегда три. На
    * старших досках трофеев не просят вовсе. */
  val NoviceTrophies: Int = 3

  /** Пул заданий: из него доска добирает объявления на свободные места —
    * каждый вид не больше одного раза, какие именно, решает бросок. Новичкам
    * после трёх трофеев остаётся пять мест, старшим доскам — все восемь; видов
    * в пуле меньше, чем мест, — объявлений будет меньше. */
  val Pool: List[BoardKind] =
    List(BoardKind.CaravanRout, BoardKind.CaveClear, BoardKind.SewerRats,
         BoardKind.Thieves, BoardKind.DwarfSupply, BoardKind.NecromancerHunt)

  /** Сколько трофейных на доске этого раздела. */
  def trophiesFor(tier: BoardTier): Int = if (tier == BoardTier.Novice) NoviceTrophies else 0

  /** Номер недели по Москве, считая с понедельника: нулевой день эпохи —
    * четверг, поэтому к нему прибавляются три дня до ближайшего понедельника. */
  def weekOf(nowMs: Long): Long = (DailyRates.dayOf(nowMs) + 3L) / 7L

  /** Когда доску перепишут: ближайший понедельник, 00:00 по Москве. */
  def nextWeekAt(nowMs: Long): Long =
    ((weekOf(nowMs) + 1L) * 7L - 3L) * DailyRates.DayMs - DailyRates.MoscowOffsetMs

  def untilNextWeek(nowMs: Long): Long = (nextWeekAt(nowMs) - nowMs).max(0L)

  /** Награда за тяжёлое задание: пять процентов уровня опытом и три дублона.
    * Трофейные платят по-прежнему — их считает сама доска по трофею. */
  def exp(heroLvl: Long): Long = (Hero.neededExpForLevel(heroLvl) / 20L).max(5L)
  val Doubloons: Long = 3L

  /** Опыт за сданный трофей — та же ставка, по которой за него платили в
    * таверне: `5 + Ур.трофея × коэффициент вида`. */
  def trophyExp(lvl: Long, coef: Double): Long = math.ceil(5.0 + lvl.toDouble * coef).toLong

  /** Дублон за трофейное задание — как и был, один. */
  val TrophyDoubloons: Long = 1L

  /** Сколько склянок одного толка берёт гномья дружина. По одной не берут —
    * только весь ящик разом. */
  val BrewsWanted: Int = 10
}
