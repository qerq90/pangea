package pangea.service.state.states.gustavo

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Durable-данные Густаво (`heroes.gustavo_data`), общие для его стейтов:
 *  - `healCooldownUntil` — до какого момента недоступно зелье лечения травм (кулдаун 30 мин);
 *  - `freeBoostsUsed`    — ключи характеристик, для которых уже израсходовано бесплатное зелье бафа.
 *  Сами активные бафы живут не здесь, а в общем `heroes.stat_boosts` ([[pangea.model.stats.StatBoosts]]). */
final case class GustavoData(healCooldownUntil: Option[Long], freeBoostsUsed: List[String])

object GustavoData {
  val empty: GustavoData = GustavoData(None, Nil)

  val HealCooldownMs:  Long = 30L * 60L * 1000L // лечение: раз в 30 минут
  val BoostDurationMs: Long = 60L * 60L * 1000L // баф характеристики: час
  val CostPerLevel:    Long = 100L              // цена зелья = CostPerLevel × уровень
  val SupplyCostPerLevel: Long = 25L            // пополнение припасов = SupplyCostPerLevel × уровень

  implicit val encoder: Encoder[GustavoData] = deriveEncoder
  implicit val decoder: Decoder[GustavoData] = deriveDecoder
}
