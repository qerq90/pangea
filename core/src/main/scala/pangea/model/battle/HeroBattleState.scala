package pangea.model.battle

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import pangea.model.stats.FightStats

case class HeroBattleState(buffs: List[Buff]) {

  // Adds atk/defence bonuses from all active buffs to the given FightStats.
  def applyTo(stats: FightStats): FightStats =
    if (buffs.isEmpty) stats
    else stats.copy(
      atk     = (stats.atk     + buffs.map(_.atk).sum).max(1L),
      defence = (stats.defence + buffs.map(_.defence).sum).max(0L)
    )

  // Flat armor bonus that reduces raw damage before physical armor absorbs (doesn't deplete).
  def armorBonus: Long = buffs.map(_.armor).sum

  // Decrement turnsLeft for each buff, remove expired ones.
  def tick: HeroBattleState = HeroBattleState(
    buffs.flatMap {
      case b if b.turnsLeft.contains(0) => None
      case b                            => Some(b.copy(turnsLeft = b.turnsLeft.map(_ - 1)))
    }
  )

  def add(buff: Buff): HeroBattleState = copy(buffs = buffs :+ buff)

  def isEmpty: Boolean = buffs.isEmpty
}

object HeroBattleState {
  val empty: HeroBattleState = HeroBattleState(Nil)

  implicit val encoder: Encoder[HeroBattleState] = deriveEncoder
  implicit val decoder: Decoder[HeroBattleState] = deriveDecoder
}
