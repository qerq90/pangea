package pangea.model.item

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

/** Вид материала-ингредиента (для куба Азата и будущих систем крафта). Не
 *  надевается, лежит в инвентаре как предмет ([[ItemType.Material]] +
 *  [[ItemDetails.Material]]). */
sealed abstract class MaterialKind(val displayName: String) extends EnumEntry

object MaterialKind extends Enum[MaterialKind] {
  val values: IndexedSeq[MaterialKind] = findValues

  /** Мифрил — редкий металл для пересборки легендарных предметов в кубе. */
  case object Mithril extends MaterialKind("Мифрил")

  /** Результат рецепта «9 голов существ». */
  case object LevitatingMonsterHead extends MaterialKind("Левитирующая голова монстра")

  implicit val encoder: Encoder[MaterialKind] = (k: MaterialKind) => k.entryName.asJson
  implicit val decoder: Decoder[MaterialKind] = (c: HCursor) => c.as[String].map(MaterialKind.withName)
}
