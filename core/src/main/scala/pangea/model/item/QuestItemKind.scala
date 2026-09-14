package pangea.model.item

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

/** Сюжетные предметы. Лежат в инвентаре как [[ItemType.QuestItem]], места не
  * занимают и никуда, кроме сюжета, не деваются. */
sealed abstract class QuestItemKind(val displayName: String, val description: String) extends EnumEntry

object QuestItemKind extends Enum[QuestItemKind] {
  val values: IndexedSeq[QuestItemKind] = findValues

  /** Письмо с пятидесятого убитого моба — начало «Письма Марисе». */
  case object MarisaLetter extends QuestItemKind(
    "Старое письмо Марисе",
    "Старое заляпанное кровью письмо с восковой печатью с символом монеты, адресованное Марисе Кальдере, проживающей в городе Кинэт."
  )

  /** Карта с обратной стороны письма: тайник Кельвина под городом. */
  case object KelvinMap extends QuestItemKind(
    "Карта Кельвина",
    "Карта с обратной стороны письма Кельвина: путь к тайнику недалеко от Кинэта. Отправиться по ней можно только из города."
  )

  /** Предмет-шаблон (id = -1, присвоит персист). */
  def item(kind: QuestItemKind): Item =
    Item(
      id       = -1L,
      name     = kind.displayName,
      lvl      = 0L,
      rarity   = Rarity.Orange,
      itemType = ItemType.QuestItem,
      attack   = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details  = ItemDetails.Quest(kind)
    )

  implicit val encoder: Encoder[QuestItemKind] = (k: QuestItemKind) => k.entryName.asJson
  implicit val decoder: Decoder[QuestItemKind] = (c: HCursor) => c.as[String].map(QuestItemKind.withName)
}
