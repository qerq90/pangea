package pangea.service.activity

import io.circe.{ACursor, Json}

/** Что происходит в группе ВК и что из этого важно конкурсу активности: вышел
  * пост, игрок отметил пост «Нравится», игрок оставил под постом комментарий.
  * Разбор — здесь, в ядре: события приходят сырым JSON через Callback API. */
sealed trait GroupEvent

object GroupEvent {

  /** Пост группы вышел на стену. `postedAt` — в миллисекундах. */
  final case class PostPublished(postId: Long, postedAt: Long) extends GroupEvent

  /** Отметка «Нравится» на посте (не на комментарии и не на фото). */
  final case class Liked(vkId: Long, postId: Long) extends GroupEvent

  /** Комментарий под постом — в том числе ответ в ветке. */
  final case class Commented(vkId: Long, postId: Long) extends GroupEvent

  /** События, которые слушает конкурс: только они проходят проверку секрета. */
  val Types: Set[String] = Set("wall_post_new", "like_add", "wall_reply_new")

  /** Секретный ключ из события: ВК кладёт его в каждое событие, если он задан в
    * настройках Callback API группы. */
  def secretOf(json: Json): Option[String] = json.hcursor.get[String]("secret").toOption

  def typeOf(json: Json): Option[String] = json.hcursor.get[String]("type").toOption

  /** Разобрать событие. `None` — не наше: другой тип, отметка не на посте,
    * отложенная или предложенная запись, комментарий от имени группы. */
  def parse(json: Json): Option[GroupEvent] = {
    val obj = json.hcursor.downField("object")
    typeOf(json).flatMap {
      case "wall_post_new" =>
        // Отложенная («postpone») и предложенная («suggest») записи ещё не
        // вышли: когда выйдут, придёт новое событие с «post».
        Option.when(obj.get[String]("post_type").getOrElse("post") == "post")(())
          .flatMap(_ => (long(obj, "id"), long(obj, "date")) match {
            case (Some(id), Some(date)) => Some(PostPublished(id, date * 1000L))
            case _                      => None
          })
      case "like_add" =>
        Option.when(obj.get[String]("object_type").toOption.contains("post"))(())
          .flatMap(_ => (long(obj, "liker_id"), long(obj, "object_id")) match {
            case (Some(vk), Some(post)) if vk > 0L => Some(Liked(vk, post))
            case _                                 => None
          })
      case "wall_reply_new" =>
        // Отрицательный `from_id` — это группа: её ответы баллов не дают.
        (long(obj, "from_id"), long(obj, "post_id")) match {
          case (Some(vk), Some(post)) if vk > 0L => Some(Commented(vk, post))
          case _                                 => None
        }
      case _ => None
    }
  }

  private def long(c: ACursor, field: String): Option[Long] = c.get[Long](field).toOption
}
