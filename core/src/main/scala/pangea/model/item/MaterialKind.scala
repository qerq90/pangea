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

  /** Ранг травы (1 — простая, 2 — редкая); 0 — не трава. «Странный цветок» —
    * трава без ранга: герой не понял, что сорвал. */
  val herbRank: Int = 0

  /** Множитель цены травы у Густаво. */
  val herbModifier: Int = 0

  /** Трава: собирается на поляне, сдаётся Густаво, Ришелье её не берёт. */
  def isHerb: Boolean = this == MaterialKind.StrangeFlower || herbRank > 0
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

  /** Падает с Белого волка — один раз за всю жизнь героя; в кубе переводит вещь
   *  в набор «Охотник». */
  case object WhiteWolfHide extends MaterialKind("Шкура Белого волка") {
    override val description: String =
      "Величественная Шкура Белого волка, некогда принадлежавшая зверю. Особая трудность " +
      "состоит что бы убить зверя, не разорвав его шкуру на лоскуты. Несмотря на яркий " +
      "белый окрас, шкура теряется из виду уже через несколько метров."
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

  // ── Травы с поляны цветов ────────────────────────────────────────────────

  /** Сорвано без понимания — таким трава становится, пока герой её не знает. */
  case object StrangeFlower extends MaterialKind("Странный цветок") {
    override val description: String = "Этот цветок отличался от других, что там росли… но я правда не знаю, ценный ли он…"
  }

  sealed abstract class Herb(name: String, rank: Int, modifier: Int, text: String) extends MaterialKind(name) {
    override val description: String = text
    override val herbRank: Int       = rank
    override val herbModifier: Int   = modifier
  }

  case object Belladonna extends Herb("Красавка", 1, 3,
    "Красивое, но смертоносное растение с блестящими чёрными ягодами. В малых дозах её сок способен усыпить или одурманить разум, а в умелых руках становится основой как для тёмных ядов, так и для крепкого шнапса, дурманящего напитка.")
  case object Nettle extends Herb("Крапива", 1, 3,
    "Жгучий сорняк, растущий у каждой канавы. Несмотря на скверный нрав, крапива — верный друг лекаря. Она очищает кровь, затягивает раны и служит основой для самых простых, но действенных отваров.")
  case object Calendula extends Herb("Календула", 1, 3,
    "Яркие оранжевые цветы, цветущие в садах. Обладают удивительным свойством заживлять плоть и успокаивать нутро после тяжёлой попойки. Незаменимы для любого воина, часто бывающего в сече.")
  case object Valerian extends Herb("Валериана", 1, 3,
    "Растение с резким запахом, который так любят коты и каджиты и ненавидят бессонница с тревогой. Её корень успокаивает дрожь в руках, обостряет внимание и помогает ворам оставаться незамеченными в ночи.")
  case object Eyebright extends Herb("Очанка", 1, 3,
    "Маленький цветок, напоминающий ясный глаз. Старые травники утверждают, что отвары из неё возвращают зоркость слепцам, а стрелкам помогают без промаха разить цель на огромном расстоянии. Любимый цветок эльфийских мастеров.")
  case object Sage extends Herb("Шалфей", 1, 3,
    "Душистая трава, растущая на солнечных полянах. Известна своими очищающими свойствами. Помогает унять лихорадку, восстановить силы и прогнать заразу из больного тела.")
  case object Wormwood extends Herb("Полынь", 1, 3,
    "Горькая, как сама жизнь, трава. В больших количествах способна свести человека с ума, вызывая жуткие видения. Однако в малых дозах она укрепляет дух и незаменима при создании дурманящих боевых эликсиров.")
  case object Chamomile extends Herb("Ромашка", 1, 3,
    "Скромный белый цветок, дарующий покой. Её мягкий отвар незаменим для заживления глубоких ран и снятия жара, когда другие средства оказываются слишком грубыми для воспалённого тела.")

  case object BubbleLily extends Herb("Пузырьковая лилия", 2, 10,
    "Большой белый цветок, заключённый в нежный пузырь. Нектар пузырьковой лилии сладкий и известен своими целебными и омолаживающими свойствами. Однако если пузырь лопнет, цветок увядает и умирает, портя нектар.")
  case object MirageFlower extends Herb("Цветок-мираж", 2, 10,
    "Способен проецировать десятки своих призрачных копий, чтобы обмануть существ, желающих навредить растению. Полезный ингредиент во многих зельях, но его уникальная способность делает его сложным для сбора.")
  case object GlaiveMushroom extends Herb("Глефовый гриб", 2, 10,
    "Гриб, который взрывается, когда на него наступаешь. Используется в зельях яда, но можно и просто бросать его во врага.")
  case object SpiderBloom extends Herb("Паучий цвет", 2, 10,
    "Почти исключительно опыляется паукообразными: пауков всех типов и размеров привлекает к растению аромат цветов, слегка опьяняющий их. Красивые фиолетово-синие цветы часто скрывают, что паучьи гнёзда строятся в растении или рядом с ним, — что делает их довольно опасными. Эти цветы могут вырастать до огромных размеров, создавая лабиринты для своих паучьих «опекунов».")
  case object WolfHops extends Herb("Волчий хмель", 2, 10,
    "Известно, что из него делают особое крепкое пиво… Но поговаривают, что его жжёный запах способен приманить волков, а иногда и оборотней.")
  case object DoomFlower extends Herb("Роковой цветок", 2, 10,
    "«Чёрный лотос растёт в самых нежелательных местах, портя пейзажи, рассказывая секреты, предвещая обречённость. Лучше не становись садовником этих цветов, дитя, иначе твой разум станет почвой, на которой они расцветут», — так писал Ашалдарон в своих заметках об этих цветах.")

  /** Травы данного ранга — из них выбирается находка на поляне. */
  def herbsOfRank(rank: Int): IndexedSeq[MaterialKind] = values.filter(_.herbRank == rank)

  /** Пыль, которая остаётся от камня этого вида. */
  def dustOf(kind: GemKind): MaterialKind =
    values.find(_.gem.contains(kind)).getOrElse(Mithril)

  /** Все виды пыли — то, что можно сыпать на оружие и собирать в камни. */
  val dusts: IndexedSeq[MaterialKind] = values.filter(_.gem.isDefined)

  implicit val encoder: Encoder[MaterialKind] = (k: MaterialKind) => k.entryName.asJson
  implicit val decoder: Decoder[MaterialKind] = (c: HCursor) => c.as[String].map(MaterialKind.withName)
}
