package pangea.model.quest

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import pangea.model.hero.Hero

/** Раздел доски заданий: гильдия держит отдельную доску на каждые двадцать пять
  * уровней. Герой берёт только со своей; остальные видны, но закрыты, и у
  * каждой свой отказ (`questBoard.locked.<key>`).
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
}

/** Вид задания с доски. Сложность показывается знаками [[Difficulty.render]]. */
sealed abstract class BoardKind(val key: String, val difficulty: Int) extends EnumEntry {
  /** Называет ли задание расу: у трофейного она в самом тексте. */
  def needsRace: Boolean = false

  /** Выездное: герой уходит на него прямо от доски и добирается по дороге
    * ([[pangea.model.state.StateType.QuestRoad]]). Прочие он делает попутно, в
    * лабиринте, когда они ему встретятся. */
  def away: Boolean = false

  /** Своя сложность у каждого объявления: у выездных она катается вместе с
    * уровнем задания, у остальных одна на весь вид. */
  def rolledLvl: Boolean = false
}

object BoardKind extends Enum[BoardKind] {
  val values: IndexedSeq[BoardKind] = findValues

  /** «Принести трофей расы X» — то, что доска просила и раньше. */
  case object Trophy extends BoardKind("trophy", 1) {
    override def needsRace: Boolean = true
  }

  /** «Найти караван и перебить охрану» — тихая кража не в счёт. */
  case object CaravanRout extends BoardKind("caravan", 15)

  /** «Найти пещеру и выбить всех до последнего» — раса пещеры не важна. */
  case object CaveClear extends BoardKind("cave", 15)

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
  lvl:   Long           = 0L
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
    "taken" -> s.taken.asJson, "done" -> s.done.asJson, "lvl" -> s.lvl.asJson)

  implicit val decoder: Decoder[BoardSlot] = (c: HCursor) =>
    for {
      kind  <- c.get[BoardKind]("kind")
      race  <- c.getOrElse[Option[String]]("race")(None)
      taken <- c.getOrElse[Boolean]("taken")(false)
      done  <- c.getOrElse[Boolean]("done")(false)
      lvl   <- c.getOrElse[Long]("lvl")(0L)
    } yield BoardSlot(kind, race, taken, done, lvl)
}

/** Доска героя на эту неделю: номер недели, раздел и восемь объявлений.
  * Живёт в `heroes.quest_data`.
  *
  * Неделя кончилась или герой перерос раздел — доска переписывается целиком, и
  * взятое, но не сданное сгорает вместе с ней: неделя и есть срок. */
final case class BoardData(
  week:  Long            = -1L,
  tier:  String          = BoardTier.Novice.key,
  slots: List[BoardSlot] = Nil
) {
  def board: BoardTier = BoardTier.byKey(tier).getOrElse(BoardTier.Novice)

  def fresh(nowMs: Long, lvl: Long): Boolean =
    week == BoardRates.weekOf(nowMs) && board == BoardTier.of(lvl)

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

  /** Сколько объявлений висит на доске неделю. */
  val Slots: Int = 8

  /** Состав доски: столько-то объявлений каждого вида. Здесь и только здесь —
    * весь расклад, его ещё предстоит пересобрать вместе с новыми заданиями. */
  val Layout: List[(BoardKind, Int)] =
    List(BoardKind.Trophy -> 4, BoardKind.CaravanRout -> 1, BoardKind.CaveClear -> 1,
         BoardKind.SewerRats -> 1, BoardKind.Thieves -> 1)

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
}
