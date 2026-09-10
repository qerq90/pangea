package pangea.model.hero

import io.circe.generic.semiauto.deriveEncoder
import io.circe.{Decoder, Encoder, HCursor}

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

  /** Декодер рукописный, и каждое поле читается с запасным значением. Иначе
    * добавление нового знания (так было с Гнилым Джо) роняет разбор старых
    * записей целиком: герой, уже купивший легенду об элементалях, снова видел
    * бы её в продаже, а трактирщик — брал бы за неё серебро повторно. */
  implicit val decoder: Decoder[LoreData] = (c: HCursor) =>
    for {
      metElemental  <- c.getOrElse[Boolean]("metElemental")(false)
      elementalLore <- c.getOrElse[Boolean]("elementalLore")(false)
      metJoe        <- c.getOrElse[Boolean]("metJoe")(false)
      joeLore       <- c.getOrElse[Boolean]("joeLore")(false)
    } yield LoreData(metElemental, elementalLore, metJoe, joeLore)
}
