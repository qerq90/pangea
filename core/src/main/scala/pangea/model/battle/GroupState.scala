package pangea.model.battle

import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, Encoder, HCursor}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.stats.FightStats

/** Моб группы, стоящий НЕ в паре с героем: всё, что описывает его и его текущее
  * состояние, включая эффекты на нём (яд, кровь, огонь, порошок, дебафы). Когда
  * такой моб встаёт в пару, слот разворачивается в поля [[SoloPveBattle]], а
  * прежний активный сворачивается в слот — см. `SoloPveBattle.swapWith`. */
final case class MonsterSlot(
  lvl:           Long,
  race:          String,
  rarity:        String,
  stats:         FightStats,
  currentHp:     Long,
  currentArmor:  Long,
  marked:        Boolean,
  currentEnergy: Long,
  effects:       BattleEffects
) {
  def toMonster: Monster =
    Monster(0L, lvl, Race.withName(race), Rarity.withName(rarity), stats, marked)

  def name: String = toMonster.name

  /** Имя для кнопки — см. `Monster.shortName`. */
  def shortName: String = toMonster.shortName

  def alive: Boolean = currentHp > 0L

  /** Слот как запись о павшем — для добычи после боя. */
  def slain: SlainMonster = SlainMonster(lvl, race, rarity, marked, name)

  /** Проценты для строки группового экрана. */
  def hpPct: Long    = if (stats.hp <= 0L) 0L else currentHp * 100L / stats.hp
  def armorPct: Long = if (stats.armor <= 0L) 0L else currentArmor * 100L / stats.armor
}

object MonsterSlot {
  implicit val encoder: Encoder[MonsterSlot] = deriveEncoder

  implicit val decoder: Decoder[MonsterSlot] = (c: HCursor) =>
    for {
      lvl           <- c.get[Long]("lvl")
      race          <- c.get[String]("race")
      rarity        <- c.get[String]("rarity")
      stats         <- c.get[FightStats]("stats")
      currentHp     <- c.get[Long]("currentHp")
      currentArmor  <- c.get[Long]("currentArmor")
      marked        <- c.getOrElse[Boolean]("marked")(false)
      currentEnergy <- c.getOrElse[Long]("currentEnergy")(0L)
      effects       <- c.getOrElse[BattleEffects]("effects")(BattleEffects.empty)
    } yield MonsterSlot(lvl, race, rarity, stats, currentHp, currentArmor, marked, currentEnergy, effects)
}

/** Убитый моб — ровно то, что нужно, чтобы после боя накатать за него добычу. */
final case class SlainMonster(lvl: Long, race: String, rarity: String, marked: Boolean, name: String)

object SlainMonster {
  implicit val encoder: Encoder[SlainMonster] = deriveEncoder
  implicit val decoder: Decoder[SlainMonster] = (c: HCursor) =>
    for {
      lvl    <- c.get[Long]("lvl")
      race   <- c.get[String]("race")
      rarity <- c.get[String]("rarity")
      marked <- c.getOrElse[Boolean]("marked")(false)
      name   <- c.getOrElse[String]("name")("")
    } yield SlainMonster(lvl, race, rarity, marked, name)
}

/** Групповая часть боя. Мобы стоят в строю по местам 1, 2, …; герой стоит на
  * месте `heroPos` — напротив него активный моб, живущий в полях
  * [[SoloPveBattle]]. Здесь — всё остальное:
  *
  *  - `others`  — мобы на прочих местах, в порядке мест (без активного);
  *  - `heroPos` — место героя (и активного моба), с единицы;
  *  - `slain`   — павшие, в порядке гибели, для выдачи добычи после победы;
  *  - `round`   — сколько раундов прошло (каждый четвёртый — перемешивание);
  *  - `pendingMove` — Таран: место, на которое герой шагнёт в конце раунда;
  *  - `originRace` — раса первого моба: подкрепление приходит той же расы.
  *
  * Обычный бой 1 на 1 — это группа из одного: `others` пуст. */
final case class GroupState(
  others:      List[MonsterSlot]  = Nil,
  slain:       List[SlainMonster] = Nil,
  round:       Int                = 0,
  pendingMove: Option[Int]        = None,
  originRace:  Option[String]     = None,
  heroPos:     Int                = 1
) {
  def isGroup: Boolean = others.nonEmpty

  /** Сколько мобов ещё на ногах, включая активного. */
  def aliveCount: Int = 1 + others.count(_.alive)

  /** Сколько мест занято в строю, с активным. */
  def size: Int = others.size + 1

  /** Место моба `others(idx)`: до героя места идут подряд, после — с пропуском его места. */
  def posOf(idx: Int): Int = if (idx < heroPos - 1) idx + 1 else idx + 2

  /** Индекс в `others` для места `pos` (не места героя). */
  def idxOf(pos: Int): Int = if (pos < heroPos) pos - 1 else pos - 2

  /** Место есть в строю. */
  def hasPos(pos: Int): Boolean = pos >= 1 && pos <= size

  /** Достаёт ли герой до места `pos` (в радиусе, но не своё). */
  def inReach(pos: Int): Boolean = hasPos(pos) && pos != heroPos && math.abs(pos - heroPos) <= GroupState.Reach

  /** Места по соседству с героем, слева направо. */
  def neighbourPositions: List[Int] = List(heroPos - 1, heroPos + 1).filter(hasPos)

  /** Моб `others(idx)` пал: из строя — в павшие. Строй смыкается: места правее
    * сдвигаются на одно, вместе с местом героя, если павший стоял левее, и с
    * отложенным Тараном (в павшего — пропадает). Чужой индекс — ничего. */
  def withoutSlot(idx: Int): GroupState =
    others.lift(idx) match {
      case None       => this
      case Some(slot) =>
        val pos  = posOf(idx)
        val move = pendingMove.flatMap {
          case p if p == pos => None
          case p if p > pos  => Some(p - 1)
          case p             => Some(p)
        }
        copy(others = others.patch(idx, Nil, 1), slain = slain :+ slot.slain, pendingMove = move,
             heroPos = if (pos < heroPos) heroPos - 1 else heroPos)
    }
}

object GroupState {
  val empty: GroupState = GroupState()

  /** Сколько мест в строю: столько мобов может стоять против героя разом. */
  val MaxMonsters: Int = 10

  /** Каждый такой раунд пары рвутся и собираются заново. */
  val ShufflePeriod: Int = 4

  /** Радиус: с места n достают до мест n−1, n, n+1. Герой бьёт соседей своего
    * места, и только они бьют его сбоку. */
  val Reach: Int = 1

  /** Шанс (в %), что в начале раунда к мобу прибежит сородич. */
  val ReinforcementChancePct: Long = 2L

  /** Каждый свободный моб добавляет столько процентов к шансу, что он не даст сбежать. */
  val SurroundPctPerFreeMob: Long = 5L

  implicit val encoder: Encoder[GroupState] = deriveEncoder
  implicit val decoder: Decoder[GroupState] = (c: HCursor) =>
    for {
      others      <- c.getOrElse[List[MonsterSlot]]("others")(Nil)
      slain       <- c.getOrElse[List[SlainMonster]]("slain")(Nil)
      round       <- c.getOrElse[Int]("round")(0)
      pendingMove <- c.getOrElse[Option[Int]]("pendingMove")(None)
      originRace  <- c.getOrElse[Option[String]]("originRace")(None)
      heroPos     <- c.getOrElse[Int]("heroPos")(1)
    } yield GroupState(others, slain, round, pendingMove, originRace, heroPos)
}
