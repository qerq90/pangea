package pangea.service.state.states.events.treasure

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Прогресс цепочки боёв события «мобы, выкопавшие сокровище». Живёт в `eventData`
 *  добычи и в `scene_data` событийных состояний между боями.
 *
 *  @param race        раса мобов цепочки (entryName) — все бои одной расы
 *  @param remaining   сколько боёв ещё осталось (включая ближайший спавн)
 *  @param doubloonMin минимум дублонов в финальном схроне
 *  @param doubloonMax максимум дублонов в финальном схроне
 */
final case class TreasureMobsChain(race: String, remaining: Int, doubloonMin: Int, doubloonMax: Int)

object TreasureMobsChain {
  implicit val encoder: Encoder[TreasureMobsChain] = deriveEncoder[TreasureMobsChain]
  implicit val decoder: Decoder[TreasureMobsChain] = deriveDecoder[TreasureMobsChain]
}

/** Прогресс раскопок прикопанного схрона (событие по таймеру). */
final case class TreasureDigProgress(startedAt: Long)

object TreasureDigProgress {
  implicit val encoder: Encoder[TreasureDigProgress] = deriveEncoder[TreasureDigProgress]
  implicit val decoder: Decoder[TreasureDigProgress] = deriveDecoder[TreasureDigProgress]
}

/** Прогресс похода за сокровищем по карте клада (таймер ~10 минут).
 *  @param startedAt момент отправки (epoch ms)
 *  @param mapLevel  уровень израсходованной карты — по нему масштабируется добыча
 */
final case class TreasureHuntProgress(startedAt: Long, mapLevel: Long)

object TreasureHuntProgress {
  implicit val encoder: Encoder[TreasureHuntProgress] = deriveEncoder[TreasureHuntProgress]
  implicit val decoder: Decoder[TreasureHuntProgress] = deriveDecoder[TreasureHuntProgress]
}
