package pangea.model.battle

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

case class Buff(
  atk:        Long,
  armor:      Long,         // flat damage reduction per incoming hit (doesn't deplete)
  defence:    Long,
  dodgePct:   Long,         // прибавка в п.п. к итоговому шансу уклонения игрока
  skillHitPct:Long,         // прибавка в п.п. к итоговому шансу применить активный навык
  turnsLeft:  Option[Int]   // None = lasts until battle ends, Some(n) = expires after n hero turns
)

object Buff {
  implicit val encoder: Encoder[Buff] = deriveEncoder
  implicit val decoder: Decoder[Buff] = deriveDecoder
}
