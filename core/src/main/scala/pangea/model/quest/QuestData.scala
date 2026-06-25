package pangea.model.quest

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/**
 * Состояние доски заданий игрока (durable, `heroes.quest_data`).
 *
 * @param remaining сколько ещё заданий можно взять до обновления (старт = 3, 0 = доска пуста)
 * @param current   раса (entryName) текущего показанного задания; `None`, когда заданий не осталось
 * @param refreshAt момент (epoch ms), когда пул заданий обновится (раз в 20 часов)
 * @param active    раса (entryName) взятого активного задания; одновременно активно ≤ 1
 */
final case class QuestData(
  remaining: Int,
  current:   Option[String],
  refreshAt: Long,
  active:    Option[String]
)

object QuestData {
  implicit val encoder: Encoder[QuestData] = deriveEncoder
  implicit val decoder: Decoder[QuestData] = deriveDecoder
}
