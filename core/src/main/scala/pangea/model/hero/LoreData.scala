package pangea.model.hero

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Что герой уже знает о мире. Durable, лежит в `heroes.lore_data`.
 *
 *  `metElemental` — встречал ли элементаля хоть раз: по первой встрече
 *  показывается особая реплика, и после неё у трактирщика появляется кнопка
 *  «Что ты знаешь о элементалях?».
 *  `elementalLore` — куплена ли у трактирщика легенда об элементалях (тогда
 *  кнопка больше не предлагает платить).
 *  `metJoe`/`joeLore` — то же самое про Гнилого Джо. */
final case class LoreData(
  metElemental:  Boolean = false,
  elementalLore: Boolean = false,
  metJoe:        Boolean = false,
  joeLore:       Boolean = false
)

object LoreData {
  val empty: LoreData = LoreData()

  implicit val encoder: Encoder[LoreData] = deriveEncoder
  implicit val decoder: Decoder[LoreData] = deriveDecoder
}
