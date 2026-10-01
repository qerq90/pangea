package pangea.model.cave

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import pangea.domain.Rng
import pangea.model.item.MapZone
import pangea.model.monster.Rarity
import pangea.model.squad.UndeadForm

/** Что найдётся в комнате пещеры, когда её мобы полягут. */
sealed trait RoomKind extends EnumEntry
object RoomKind extends Enum[RoomKind] {
  val values: IndexedSeq[RoomKind] = findValues

  case object Empty extends RoomKind // голые стены
  case object Herb  extends RoomKind // трава в трещине
  case object Chest extends RoomKind // окованный сундук
  case object Stash extends RoomKind // чей-то схрон
  case object Rest  extends RoomKind // сухой угол, где можно перевести дух
  case object Altar extends RoomKind // алтарь тёмных сил: поднимает добычу обратно
  case object Treasure extends RoomKind // клад по карте: она и привела сюда

  implicit val encoder: Encoder[RoomKind] = (k: RoomKind) => k.entryName.asJson
  implicit val decoder: Decoder[RoomKind] = (c: HCursor) => c.as[String].map(RoomKind.withName)
}

/** Комната пещеры: место на сетке, сколько мобов там ждёт и что в ней есть.
  * `done` — находка комнаты уже разыграна (или её и не было). */
final case class CaveRoom(x: Int, y: Int, monsters: Int, kind: RoomKind, done: Boolean = false)

object CaveRoom {
  implicit val encoder: Encoder[CaveRoom] = (r: CaveRoom) =>
    io.circe.Json.obj(
      "x"        -> r.x.asJson,
      "y"        -> r.y.asJson,
      "monsters" -> r.monsters.asJson,
      "kind"     -> r.kind.asJson,
      "done"     -> r.done.asJson)

  implicit val decoder: Decoder[CaveRoom] = (c: HCursor) =>
    for {
      x        <- c.get[Int]("x")
      y        <- c.get[Int]("y")
      monsters <- c.getOrElse[Int]("monsters")(0)
      kind     <- c.getOrElse[RoomKind]("kind")(RoomKind.Empty)
      done     <- c.getOrElse[Boolean]("done")(false)
    } yield CaveRoom(x, y, monsters, kind, done)
}

/** Сторона света, она же кнопка направления: вперёд, назад, влево, вправо. */
sealed abstract class CaveDir(val dx: Int, val dy: Int) extends EnumEntry
object CaveDir extends Enum[CaveDir] {
  val values: IndexedSeq[CaveDir] = findValues

  case object Forward extends CaveDir(0, 1)
  case object Back    extends CaveDir(0, -1)
  case object Left    extends CaveDir(-1, 0)
  case object Right   extends CaveDir(1, 0)
}

/** Пещера целиком — всё, что нужно, чтобы её пройти. Живёт в `scene_data`,
  * поэтому декодеры рукописные: новое поле не должно стирать начатую пещеру
  * у тех, кто уже внутри.
  *
  * @param race      раса, чья это пещера (все мобы в ней одной расы)
  * @param rooms     комнаты; `at` — та, где герой сейчас
  * @param inside    герой уже вошёл (иначе он на пороге и может уйти)
  * @param page      страница списка вещей на пороге
  * @param weakened  мобы одурманены: атака и энергия срезаны (см. [[CaveRates.DopeCutPct]])
  * @param poisoned  мобы входят в бой отравленными
  * @param expEarned сумма опыта за уже убитых здесь — за зачистку выдаётся доля от неё
  * @param restUsed  привал в пещере уже устроен, второго не будет
  * @param restUntil момент, когда привал закончится (0 — герой не спит)
  * @param rewarded  награда за зачистку уже выдана
  * @param altarSpent алтарь отдал свою силу: поднимать больше некого
  * @param pending   кто ждёт места в отряде, пока герой решает, кем пожертвовать
  * @param pendingTrophy id трофея, за который этот поднятый встанет
  * @param treasure  зона карты клада, если карту пустили в дело на пороге: по
  *                  ней и катается добыча комнаты с кладом
  */
final case class CaveScene(
  race:      String,
  rooms:     List[CaveRoom],
  at:        Int     = 0,
  inside:    Boolean = false,
  page:      Int     = 0,
  weakened:  Boolean = false,
  poisoned:  Boolean = false,
  expEarned: Long    = 0L,
  restUsed:  Boolean = false,
  restUntil: Long    = 0L,
  rewarded:  Boolean = false,
  altarSpent: Boolean = false,
  pending:    Option[UndeadForm] = None,
  pendingTrophy: Long = 0L,
  treasure:   Option[MapZone] = None
) {

  def room: CaveRoom = rooms.lift(at).getOrElse(rooms.head)

  /** Комната в этом направлении от текущей, если туда есть ход. */
  def neighbour(dir: CaveDir): Option[Int] = {
    val here = room
    val idx  = rooms.indexWhere(r => r.x == here.x + dir.dx && r.y == here.y + dir.dy)
    Option.when(idx >= 0)(idx)
  }

  def withRoom(idx: Int, f: CaveRoom => CaveRoom): CaveScene =
    if (idx < 0 || idx >= rooms.size) this else copy(rooms = rooms.updated(idx, f(rooms(idx))))

  /** Живых мобов в пещере не осталось. */
  def cleared: Boolean = rooms.forall(_.monsters <= 0)
}

object CaveScene {
  implicit val encoder: Encoder[CaveScene] = (s: CaveScene) =>
    io.circe.Json.obj(
      "race"      -> s.race.asJson,
      "rooms"     -> s.rooms.asJson,
      "at"        -> s.at.asJson,
      "inside"    -> s.inside.asJson,
      "page"      -> s.page.asJson,
      "weakened"  -> s.weakened.asJson,
      "poisoned"  -> s.poisoned.asJson,
      "expEarned" -> s.expEarned.asJson,
      "restUsed"  -> s.restUsed.asJson,
      "restUntil" -> s.restUntil.asJson,
      "rewarded"  -> s.rewarded.asJson,
      "altarSpent" -> s.altarSpent.asJson,
      "pending"      -> s.pending.asJson,
      "pendingTrophy" -> s.pendingTrophy.asJson,
      "treasure"      -> s.treasure.asJson)

  implicit val decoder: Decoder[CaveScene] = (c: HCursor) =>
    for {
      race      <- c.get[String]("race")
      rooms     <- c.get[List[CaveRoom]]("rooms")
      at        <- c.getOrElse[Int]("at")(0)
      inside    <- c.getOrElse[Boolean]("inside")(false)
      page      <- c.getOrElse[Int]("page")(0)
      weakened  <- c.getOrElse[Boolean]("weakened")(false)
      poisoned  <- c.getOrElse[Boolean]("poisoned")(false)
      expEarned <- c.getOrElse[Long]("expEarned")(0L)
      restUsed  <- c.getOrElse[Boolean]("restUsed")(false)
      restUntil <- c.getOrElse[Long]("restUntil")(0L)
      rewarded  <- c.getOrElse[Boolean]("rewarded")(false)
      spent     <- c.getOrElse[Boolean]("altarSpent")(false)
      pending   <- c.getOrElse[Option[UndeadForm]]("pending")(None)
      trophy    <- c.getOrElse[Long]("pendingTrophy")(0L)
      treasure  <- c.getOrElse[Option[MapZone]]("treasure")(None)
    } yield CaveScene(race, rooms, at, inside, page, weakened, poisoned, expEarned, restUsed, restUntil,
                      rewarded, spent, pending, trophy, treasure)
}

/** Числа пещеры. Вынесены из компаньонов нарочно — их читают и генератор, и
  * состояние, и тесты. */
object CaveRates {

  /** Сколько комнат бывает в пещере. */
  val MinRooms: Int = 10
  val MaxRooms: Int = 20

  /** Сколько всего мобов в неё набивается. Игроку это число не называют. */
  val MinMonsters: Int = 15
  val MaxMonsters: Int = 30

  /** Размеры кучек, на которые мобы делятся по комнатам, — по кругу и поровну. */
  val GroupSizes: List[Int] = List(3, 4, 5)

  /** Редкости мобов пещеры: легендарных здесь не водится, зато мифических много.
    * Вес = число билетов в пуле, сумма = 100. */
  val RarityPool: List[Rarity] =
    List.fill(1)(Rarity.Common) ++ List.fill(30)(Rarity.Uncommon) ++
      List.fill(29)(Rarity.Rare) ++ List.fill(40)(Rarity.Mythical)

  /** Виды комнат помимо привала: пусто 45 · трава 20 · сундук 15 · схрон 20. */
  val KindPool: List[RoomKind] =
    List.fill(45)(RoomKind.Empty) ++ List.fill(20)(RoomKind.Herb) ++
      List.fill(15)(RoomKind.Chest) ++ List.fill(20)(RoomKind.Stash)

  /** Сонный дурман и дымная фляга: на столько процентов срезаны атака и энергия
    * всех мобов пещеры. */
  val DopeCutPct: Long = 20L

  /** Сундук: шанс (в %) на большую руну вместо обычной добычи. */
  val ChestRunePct: Int = 10

  /** Схрон в пещере — тот же, что прикопанный в лабиринте: 1–2 дублона. */
  val StashDoubloonMin: Int = 1
  val StashDoubloonMax: Int = 2

  /** И с каким шансом (в %) золото вообще окажется в схроне вместе с серебром:
    * в пещере дублоны попадаются на десятую часть реже, чем в прикопанном схроне. */
  val StashDoubloonChancePct: Int = 90

  /** Привал в пещере длится столько же, сколько у костра в лабиринте. */
  val RestMs: Long = 30_000L

  /** Шанс (в %), что в пещере окажется алтарь тёмных сил. Больше одного на
    * пещеру не бывает. */
  val AltarChancePct: Int = 50

  /** Сколько процентов от взятого с мобов пещеры опыта она докладывает за
    * зачистку. Было двести — стало сто шестьдесят: финальная награда срезана на пятую часть. */
  val ClearExpPct: Long = 160L
}

/** Чистая генерация пещеры: комнаты, ходы между ними, мобы по кучкам и находки.
  * Без ZIO и без скрытого Random — всё от [[Rng]], как и прочие генераторы. */
object CaveGenerator {

  /** Бросок в [0, bound). Берём СТАРШИЕ биты: у линейного генератора [[Rng]]
    * младшие ходят коротким циклом (у двух последних он равен четырём), и
    * блуждание по четырём направлениям на них топталось бы на месте. */
  private def roll(rng: Rng, bound: Int): (Int, Rng) = {
    val (l, next) = rng.nextLong
    (((l >>> 17) % bound.toLong).toInt, next)
  }

  /** Пещера расы `race`: связный клубок комнат, мобы кучками по 3–5 и ровно
    * один угол, где можно перевести дух. Первая комната — вход: в ней пусто. */
  def generate(race: String, rng: Rng): (CaveScene, Rng) = {
    val (extraRooms, r1) = roll(rng, CaveRates.MaxRooms - CaveRates.MinRooms + 1)
    val (cells, r2)      = dig(CaveRates.MinRooms + extraRooms, r1)
    val (total, r3)      = monsterCount(r2)
    val (groups, r4)     = spread(total, cells.size, r3)
    val (kinds, r5)      = kindsFor(cells.size, r4)
    val rooms = cells.zipWithIndex.map { case ((x, y), i) =>
      CaveRoom(x, y, groups.getOrElse(i, 0), kinds.getOrElse(i, RoomKind.Empty))
    }
    (CaveScene(race, rooms), r5)
  }

  private def monsterCount(rng: Rng): (Int, Rng) = {
    val (n, next) = roll(rng, CaveRates.MaxMonsters - CaveRates.MinMonsters + 1)
    (n + CaveRates.MinMonsters, next)
  }

  /** Прорыть `count` комнат случайным блужданием по сетке: каждая новая
    * пристраивается к уже прорытой, поэтому весь клубок связный, а соседние по
    * координатам комнаты и есть соединённые проходом. */
  private def dig(count: Int, rng: Rng): (List[(Int, Int)], Rng) = {
    @annotation.tailrec
    def loop(cells: Vector[(Int, Int)], r: Rng, guard: Int): (Vector[(Int, Int)], Rng) =
      if (cells.size >= count || guard <= 0) (cells, r)
      else {
        val (fromIdx, r1) = roll(r, cells.size)
        val (dirIdx, r2)  = roll(r1, CaveDir.values.size)
        val (x, y)        = cells(fromIdx)
        val dir           = CaveDir.values(dirIdx)
        val next          = (x + dir.dx, y + dir.dy)
        if (cells.contains(next)) loop(cells, r2, guard - 1)
        else loop(cells :+ next, r2, guard - 1)
      }
    // Потолок попыток: блуждание иногда утыкается в уже прорытое, но при
    // четырёх направлениях свободная клетка находится быстро.
    val (cells, next) = loop(Vector((0, 0)), rng, count * 50)
    (cells.toList, next)
  }

  /** Разложить `total` мобов кучками по 3, 4 и 5 (по кругу, чтобы кучек каждого
    * размера было поровну) по случайным комнатам, кроме входной. Хвост меньше
    * тройки расходится по уже набранным кучкам: одиночек по пещере не бродит,
    * и толще пятерых кучка не становится. */
  private def spread(total: Int, roomCount: Int, rng: Rng): (Map[Int, Int], Rng) = {
    val min = CaveRates.GroupSizes.min
    val max = CaveRates.GroupSizes.max
    // Хвост в одного-двух мобов не бродит по пещере сам по себе: он
    // расходится по уже набранным кучкам, и ни одна не толще пятерых.
    @annotation.tailrec
    def spill(left: Int, acc: List[Int]): List[Int] =
      if (left <= 0) acc
      else acc.indexWhere(_ < max) match {
        case -1  => acc
        case idx => spill(left - 1, acc.updated(idx, acc(idx) + 1))
      }
    @annotation.tailrec
    def cut(left: Int, i: Int, acc: List[Int]): List[Int] = {
      val size = CaveRates.GroupSizes(i % CaveRates.GroupSizes.size)
      if (left <= 0) acc
      else if (left >= size) cut(left - size, i + 1, acc :+ size)
      else if (left >= min) acc :+ left
      else spill(left, acc)
    }
    val groups = cut(total, 0, Nil)
    // Входная комната (индекс 0) остаётся пустой: на пороге не бьют.
    val (picked, next) = groups.foldLeft((Map.empty[Int, Int], rng)) { case ((acc, r), size) =>
      val free = (1 until roomCount).filterNot(acc.contains).toList
      if (free.isEmpty) (acc, r)
      else {
        val (i, r1) = roll(r, free.size)
        (acc + (free(i) -> size), r1)
      }
    }
    (picked, next)
  }

  /** Виды комнат: вход пустой, одна случайная — привал, ещё одна (в половине
    * пещер) — алтарь тёмных сил, остальным свой бросок. */
  private def kindsFor(roomCount: Int, rng: Rng): (Map[Int, RoomKind], Rng) = {
    val (shift, r1)     = roll(rng, (roomCount - 1).max(1))
    val restIdx         = shift + 1
    val (altarRoll, r2) = roll(r1, 100)
    // Алтарь катается среди мест БЕЗ привала: место привала просто
    // пропускается, а не отменяет алтарь. Иначе редкое совпадение съедало бы
    // несколько процентов и без того нечастого алтаря.
    val (altarShift, r3) = roll(r2, (roomCount - 2).max(1))
    val altarIdx        = if (altarShift + 1 >= restIdx) altarShift + 2 else altarShift + 1
    val withAltar       = altarRoll < CaveRates.AltarChancePct && altarIdx != restIdx
    val start: Map[Int, RoomKind] =
      Map(0 -> RoomKind.Empty, restIdx -> RoomKind.Rest) ++
        (if (withAltar) Map(altarIdx -> RoomKind.Altar) else Map.empty[Int, RoomKind])
    (1 until roomCount).foldLeft((start, r3)) {
      case ((acc, r), i) =>
        if (acc.contains(i)) (acc, r)
        else {
          val (k, next) = roll(r, CaveRates.KindPool.size)
          (acc + (i -> CaveRates.KindPool(k)), next)
        }
    }
  }

  /** Пристроить к пещере комнату с кладом: карта привела именно сюда, но где
    * копать — герой ищет сам. Комната встаёт на свободную клетку рядом с одной
    * из вырытых, поэтому ход в неё есть, а мобов в ней нет: клад стерегут те,
    * кто и так бродит по пещере.
    *
    * Клетки берутся в порядке обхода комнат, так что комната садится «за»
    * случайной из них, а не всегда у входа. */
  def addTreasureRoom(scene: CaveScene, rng: Rng): (CaveScene, Rng) = {
    val taken = scene.rooms.map(r => (r.x, r.y)).toSet
    val free  = scene.rooms.flatMap(r => CaveDir.values.toList.map(d => (r.x + d.dx, r.y + d.dy)))
                  .distinct.filterNot(taken.contains)
    if (free.isEmpty) (scene, rng)
    else {
      val (i, next) = roll(rng, free.size)
      val (x, y)    = free(i)
      (scene.copy(rooms = scene.rooms :+ CaveRoom(x, y, 0, RoomKind.Treasure)), next)
    }
  }

  /** Редкость очередного моба пещеры: пятой здесь не бывает (см. [[CaveRates.RarityPool]]). */
  def rollRarity(rng: Rng): (Rarity, Rng) = {
    val (i, next) = roll(rng, CaveRates.RarityPool.size)
    (CaveRates.RarityPool(i), next)
  }
}
