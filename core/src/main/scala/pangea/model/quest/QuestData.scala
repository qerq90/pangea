package pangea.model.quest

import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, Encoder, HCursor}

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

  /** Декодер рукописный, каждое поле — с запасным значением: производный требует
    * все поля разом, и добавление нового обнулило бы доску заданий у всех, кто
    * уже взял задание. Пустая доска (`remaining = 0`) — безопасный дефолт: она
    * восстановится сама при ближайшем обновлении пула. */
  implicit val decoder: Decoder[QuestData] = (c: HCursor) =>
    for {
      remaining <- c.getOrElse[Int]("remaining")(0)
      current   <- c.getOrElse[Option[String]]("current")(None)
      refreshAt <- c.getOrElse[Long]("refreshAt")(0L)
      active    <- c.getOrElse[Option[String]]("active")(None)
    } yield QuestData(remaining, current, refreshAt, active)
}
