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

/** Групповая часть боя. Герой всегда стоит под номером 1; моб под номером 1 —
  * это активная пара, живущая в полях [[SoloPveBattle]]. Здесь — всё остальное:
  *
  *  - `others`  — мобы под номерами 2, 3, … в порядке номеров;
  *  - `slain`   — павшие, в порядке гибели, для выдачи добычи после победы;
  *  - `round`   — сколько раундов прошло (каждый четвёртый — перемешивание);
  *  - `pendingSwap` — Таран: индекс в `others`, кого в начале следующего раунда
  *    поставить в пару вместо нынешнего;
  *  - `originRace` — раса первого моба: подкрепление приходит той же расы.
  *
  * Обычный бой 1 на 1 — это группа из одного: `others` пуст. */
final case class GroupState(
  others:      List[MonsterSlot]  = Nil,
  slain:       List[SlainMonster] = Nil,
  round:       Int                = 0,
  pendingSwap: Option[Int]        = None,
  originRace:  Option[String]     = None
) {
  def isGroup: Boolean = others.nonEmpty

  /** Сколько мобов ещё на ногах, включая активного. */
  def aliveCount: Int = 1 + others.count(_.alive)

  /** Моб `others(idx)` пал: из строя — в павшие. Строй смыкается, поэтому
    * отложенный Таран, если целил в него, пропадает, а если целил дальше по
    * строю — сдвигается на одного. Чужой индекс — ничего не меняется. */
  def withoutSlot(idx: Int): GroupState =
    others.lift(idx) match {
      case None       => this
      case Some(slot) =>
        val swap = pendingSwap.flatMap {
          case i if i == idx => None
          case i if i > idx  => Some(i - 1)
          case i             => Some(i)
        }
        copy(others = others.patch(idx, Nil, 1), slain = slain :+ slot.slain, pendingSwap = swap)
    }
}

object GroupState {
  val empty: GroupState = GroupState()

  /** Сколько мобов может стоять против героя одновременно, включая активного. */
  val MaxMonsters: Int = 5

  /** Каждый такой раунд пары рвутся и собираются заново. */
  val ShufflePeriod: Int = 4

  /** Радиус: моб под номером n достаёт бойцов под номерами n−1, n, n+1. Герой
    * стоит под номером 1, поэтому сбоку его бьёт только моб под номером 2. */
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
      pendingSwap <- c.getOrElse[Option[Int]]("pendingSwap")(None)
      originRace  <- c.getOrElse[Option[String]]("originRace")(None)
    } yield GroupState(others, slain, round, pendingSwap, originRace)
}
