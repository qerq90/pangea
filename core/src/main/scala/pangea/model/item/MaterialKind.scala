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

  /** Из какого камня получена эта пыль. Задан только у пылей: по нему собирают
    * камень обратно (три пыли — надколотый) и берут грань, которой пыль
    * покрывает оружие. У прочих материалов — None. */
  val gem: Option[GemKind] = None
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

  /** Падает с Гнилого Джо; в кубе переводит вещь в набор «Упырь». */
  case object GhoulSkin extends MaterialKind("Кожа упыря") {
    override val description: String =
      "Представляет собой куски бледной, эластичной и противоестественно живучей " +
      "плоти. Ткани сохраняют остаточную регенеративную силу монстра даже после его " +
      "гибели."
  }


  // ── Пыль камней ───────────────────────────────────────────────────────────
  // Остаётся, когда игрок ломает камень: надколотый даёт единицу, каждый
  // следующий грейд — на одну больше. Три пыли собираются в кубе обратно в
  // надколотый камень, а ещё пылью можно посыпать оружие на один бой.
  case object DiamondDust extends MaterialKind("Бриллиантовая пыль") {
    override val description: String = "Искрящаяся крошка бриллианта. Ветер поднимает её с ладони раньше, чем успеешь сжать пальцы."
    override val gem: Option[GemKind] = Some(GemKind.Diamond)
  }

  case object RubyDust extends MaterialKind("Рубиновая пыль") {
    override val description: String = "Багровый порошок, тёплый на ощупь. В темноте видно, как в нём тлеют крошечные угли."
    override val gem: Option[GemKind] = Some(GemKind.Ruby)
  }

  case object TopazDust extends MaterialKind("Топазная пыль") {
    override val description: String = "Золотистая пыльца, от которой волосы на руке встают дыбом. Щиплет пальцы, если растереть."
    override val gem: Option[GemKind] = Some(GemKind.Topaz)
  }

  case object EmeraldDust extends MaterialKind("Изумрудная пыль") {
    override val description: String = "Зелёная труха с горьким запахом. Даже мухи облетают ладонь, на которой она лежит."
    override val gem: Option[GemKind] = Some(GemKind.Emerald)
  }

  case object SapphireDust extends MaterialKind("Сапфировая пыль") {
    override val description: String = "Синяя крошка, которая не тает. Ладонь под ней немеет от холода за пару вдохов."
    override val gem: Option[GemKind] = Some(GemKind.Sapphire)
  }

  case object AmethystDust extends MaterialKind("Аметистовая пыль") {
    override val description: String = "Лиловая пудра. Если долго смотреть сквозь неё на свет, начинаешь замечать то, что обычно ускользает."
    override val gem: Option[GemKind] = Some(GemKind.Amethyst)
  }

  case object BlackPowder extends MaterialKind("Чёрный порошок") {
    override val description: String = "Всё, что осталось от чьего-то черепа. Чёрный, жирный на ощупь и неприятно тёплый."
    override val gem: Option[GemKind] = Some(GemKind.Skull)
  }

  /** Пыль, которая остаётся от камня этого вида. */
  def dustOf(kind: GemKind): MaterialKind =
    values.find(_.gem.contains(kind)).getOrElse(Mithril)

  /** Все виды пыли — то, что можно сыпать на оружие и собирать в камни. */
  val dusts: IndexedSeq[MaterialKind] = values.filter(_.gem.isDefined)

  implicit val encoder: Encoder[MaterialKind] = (k: MaterialKind) => k.entryName.asJson
  implicit val decoder: Decoder[MaterialKind] = (c: HCursor) => c.as[String].map(MaterialKind.withName)
}
