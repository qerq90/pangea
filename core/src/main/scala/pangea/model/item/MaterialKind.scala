package pangea.model.item

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

/** Вид материала-ингредиента (для куба Азата и будущих систем крафта). Не
 *  надевается, лежит в инвентаре как предмет ([[ItemType.Material]] +
 *  [[ItemDetails.Material]]). */
sealed abstract class MaterialKind(val displayName: String) extends EnumEntry {
  /** Описание для инвентаря; по умолчанию его нет. */
  val description: String = ""

  /** Сколько дублонов даёт за материал Ришелье. 0 — обычная продажа за серебро. */
  val doubloonPrice: Long = 0L
}

object MaterialKind extends Enum[MaterialKind] {
  val values: IndexedSeq[MaterialKind] = findValues

  /** Мифрил — редкий металл для пересборки легендарных предметов в кубе. */
  case object Mithril extends MaterialKind("Мифрил")

  /** Результат рецепта «9 голов существ». */
  case object LevitatingMonsterHead extends MaterialKind("Левитирующая голова монстра")

  /** Падает с огненного элементаля; в кубе переводит вещь в набор «Дикое пламя». */
  case object EverburningIron extends MaterialKind("Вечно огненное железо") {
    override val description: String =
      "Железо, которое отказалось остывать. Говорят, что внутри каждого такого куска " +
      "заключён крошечный осколок ярости огненного элементаля."

    /** Ришелье такому куску рад и платит золотом, а не серебром. */
    override val doubloonPrice: Long = 5L
  }

  /** Падает с каменного элементаля; в кубе переводит вещь в «Каменного стража». */
  case object MagicStone extends MaterialKind("Магический камень") {
    override val description: String =
      "Этот камень собирает вокруг себя другие камни даже лёжа в сумке. Это доставляет " +
      "хлопоты при его очистке, а также заинтересовывает алхимиков. Говорят, что внутри " +
      "каждого такого куска заключён крошечный осколок души каменного элементаля."

    /** Ришелье платит за него золотом, как и за железо огненного. */
    override val doubloonPrice: Long = 5L
  }

  implicit val encoder: Encoder[MaterialKind] = (k: MaterialKind) => k.entryName.asJson
  implicit val decoder: Decoder[MaterialKind] = (c: HCursor) => c.as[String].map(MaterialKind.withName)
}
