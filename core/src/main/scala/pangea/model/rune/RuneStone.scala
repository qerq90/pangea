package pangea.model.rune

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import pangea.model.item.{Item, ItemDetails, ItemType, PassiveKind, Rarity}
import pangea.model.skill.Skill

/** Размер рунного камня: сколько понимания он даёт Казимиру и сколько за него
 *  даёт Ришелье. Уровня у камня нет — ни у большого, ни у малого. */
sealed abstract class RuneStoneSize(val prefix: String, val points: Long, val price: Long) extends EnumEntry

object RuneStoneSize extends Enum[RuneStoneSize] {
  case object Big extends RuneStoneSize("Большая руна", 5L, 400L)

  val values: IndexedSeq[RuneStoneSize] = findValues

  implicit val encoder: Encoder[RuneStoneSize] = (s: RuneStoneSize) => s.entryName.asJson
  implicit val decoder: Decoder[RuneStoneSize] = (c: HCursor) => c.as[String].map(RuneStoneSize.withName)
}

/** Рунный камень — плита с узором руны, выпадающая из мобов и схронов. Вещи он
 *  не заменяет: Казимир читает его, как прочёл бы руну с вещи, — углубляет
 *  понимание ([[RuneStoneSize.points]]) или выжигает клеймо за обычную цену
 *  (см. `Rune.of`/`Rune.pointsOf`). У Ришелье уходит в хлам по [[RuneStoneSize.price]],
 *  и только если в настройках автопродажи включены руны. */
object RuneStone {

  /** Родительный падеж имени руны (для заголовка «Большая руна …») и то, как
    * выглядит сам узор на плите. */
  final case class Look(genitive: String, engraving: String)

  /** Строка, которая стоит под узором на любом рунном камне. */
  val Insight: String = "Всматриваясь в неё, я как будто лучше начинаю её понимать…"

  /** Все руны, на которые бывают камни: боевые и пассивные. */
  def all: List[Rune] =
    Skill.values.toList.map(Rune.Active(_)) ++ PassiveKind.values.toList.map(Rune.Passive(_))

  private val looks: Map[String, Look] = Map(
    // ── Боевые руны: оружие ───────────────────────────────────────────────────
    Rune.Active(Skill.SweepingStrike).key -> Look("Размашистого удара",
      "Борозды расходятся от середины широкой дугой — будто плиту одним движением рассекли наискось."),
    Rune.Active(Skill.QuickStrike).key -> Look("Быстрого удара",
      "Узор короткий и злой: две черты, поставленные так резко, что на выходе камень скололся."),
    Rune.Active(Skill.CunningStrike).key -> Look("Хитрого удара",
      "Линии петляют, обрываются и продолжаются там, где их не ждёшь."),
    Rune.Active(Skill.BloodHarvest).key -> Look("Кровавой жатвы",
      "Все борозды сходятся к ямке в середине, и в ней запеклось что-то бурое."),
    Rune.Active(Skill.Bleeding).key -> Look("Кровотечения",
      "Тонкая нить узора с зазубринами по краю: такой линией не рубят — такой вскрывают."),
    Rune.Active(Skill.WeakSpotStrike).key -> Look("Удара в слабое место",
      "Плита почти пустая: весь узор — одна точка и стрелка, упёртая в неё."),
    Rune.Active(Skill.BladeWhirl).key -> Look("Вихря клинка",
      "Спираль, закрученная до головокружения; глазами вести по ней долго не выходит."),
    Rune.Active(Skill.FanCut).key -> Look("Веерного пореза",
      "Пять расходящихся борозд — след когтей, оставленный одним махом."),
    // ── Боевые руны: нагрудник ────────────────────────────────────────────────
    Rune.Active(Skill.MinorHeal).key -> Look("Малого исцеления",
      "Капля, а от неё круги — будто по воде, которая всё никак не успокоится."),
    Rune.Active(Skill.Reinforcement).key -> Look("Укрепления",
      "Борозды сложены встык, ряд к ряду, как кирпичи в кладке."),
    Rune.Active(Skill.Restoration).key -> Look("Восстановления",
      "Через всю плиту идёт трещина, и узор сшивает её поперёк ровными стежками."),
    Rune.Active(Skill.Bulwark).key -> Look("Заслона",
      "Щит, выбитый грубо и глубоко: в него будто упирались не раз и не два."),
    Rune.Active(Skill.Ram).key -> Look("Тарана",
      "Узор идёт из угла в угол напролом, ломая по дороге все прочие линии."),
    Rune.Active(Skill.BattleCry).key -> Look("Боевого клича",
      "От раскрытого рта расходятся волны; камень в этом месте выщерблен, будто и правда звенел."),
    Rune.Active(Skill.Shove).key -> Look("Отбрасывания",
      "Две ладони, упёртые в край плиты, а борозды перед ними смяты и разбегаются."),
    // ── Пассивные руны: шлем ──────────────────────────────────────────────────
    Rune.Passive(PassiveKind.Stealthy).key -> Look("Скрытности",
      "Узор едва намечен: отойдёшь на шаг — и уже не найдёшь, в каком месте плиты он был."),
    Rune.Passive(PassiveKind.Hunter).key -> Look("Охотника",
      "След, пересечённый другим следом: кто-то шёл, а кто-то шёл за ним."),
    // ── Пассивные руны: плечи, брасы, перчатки ───────────────────────────────
    Rune.Passive(PassiveKind.Taxidermist).key -> Look("Таксидермиста",
      "Аккуратная строчка по кругу — такой иглой зашивают шкуру, чтобы шва не было видно."),
    Rune.Passive(PassiveKind.Toughness).key -> Look("Крепкости",
      "Плита треснула поперёк, но узор держит края вместе и не даёт им разойтись."),
    Rune.Passive(PassiveKind.Spiky).key -> Look("Шипастого",
      "Из борозд наружу торчат острые засечки: голой рукой за такую плиту не возьмёшься."),
    Rune.Passive(PassiveKind.QuickHands).key -> Look("Быстрых рук",
      "Узор смазан, будто резчик спешил, — и всё равно успел довести его до конца."),
    Rune.Passive(PassiveKind.Blending).key -> Look("Сливающегося",
      "Линии повторяют прожилки камня, и где кончается порода, а где начинается узор, не разобрать."),
    Rune.Passive(PassiveKind.Terrifying).key -> Look("Ужасающего",
      "Ничего, кроме глаз. Смотреть в них дольше нескольких ударов сердца не хочется."),
    // ── Пассивные руны: штаны, поножи, сапоги, пояс ──────────────────────────
    Rune.Passive(PassiveKind.QuickFeet).key -> Look("Быстрых ног",
      "Цепочка следов, поставленных так часто, что издали сливается в одну черту."),
    Rune.Passive(PassiveKind.Impenetrable).key -> Look("Непробиваемого",
      "По плите били, и не раз: вмятины есть, а узор целёхонек."),
    Rune.Passive(PassiveKind.Stash).key -> Look("Тайника",
      "У узора двойное дно: под верхними бороздами угадываются ещё одни, поглубже."),
    Rune.Passive(PassiveKind.Reinforced).key -> Look("Укреплённого",
      "Пластины внахлёст, одна поверх другой, и каждая врезана глубже предыдущей."),
    Rune.Passive(PassiveKind.Glittering).key -> Look("Сверкающего",
      "В бороздах блестят крупинки слюды — на свету плита слепит глаза."),
    // ── Пассивные руны: кольца ────────────────────────────────────────────────
    Rune.Passive(PassiveKind.Jeweler).key -> Look("Ювелира",
      "Узор мелкий, как на перстне: резали его иглой, а не зубилом."),
    Rune.Passive(PassiveKind.Marauder).key -> Look("Мародёра",
      "Холмик со воткнутой лопатой, и борозды вокруг взрыхлены, будто копали второпях."),
    Rune.Passive(PassiveKind.Robber).key -> Look("Разбойника",
      "Нож, перечёркивающий кошель. Ни одной лишней линии."),
    Rune.Passive(PassiveKind.Healer).key -> Look("Целителя",
      "Ладони, сложенные лодочкой, и тепло, разбегающееся от них кольцами."),
    // ── Пассивные руны: амулеты ───────────────────────────────────────────────
    Rune.Passive(PassiveKind.Focused).key -> Look("Сосредоточенности",
      "Все борозды сходятся в одну точку и там обрываются разом."),
    Rune.Passive(PassiveKind.Precise).key -> Look("Точности",
      "Прямая от края до края: на камне такую ровно не проведёшь — а тут провели."),
    Rune.Passive(PassiveKind.Healing).key -> Look("Целебного",
      "Росток, пробившийся сквозь камень: узор растёт вместе с трещиной."),
    Rune.Passive(PassiveKind.SelfRepairing).key -> Look("Самовосстанавливающегося",
      "Скол на плите зарос собственной крошкой, и узор по нему сомкнулся обратно.")
  )

  /** Как выглядит узор этой руны; у руны без своей записи — общий вид. */
  def look(rune: Rune): Look =
    looks.getOrElse(rune.key, Look(rune.label, "Узор врезан глубоко, но разобрать его с ходу не выходит."))

  /** «Большая руна Размашистого удара». */
  def name(rune: Rune, size: RuneStoneSize): String = s"${size.prefix} ${look(rune).genitive}"

  /** Строки карточки: узор, то, что чувствуешь, глядя на него, и что даст руна. */
  def describe(rune: Rune): List[String] =
    List(look(rune).engraving, Insight, s"${rune.label}: ${rune.description}")

  /** Готовый предмет. Уровня у камня нет, редкость служебная — в заголовке её не
    * видно (см. `Item.displayTitle`), а цена у Ришелье своя, по размеру. */
  def item(rune: Rune, size: RuneStoneSize): Item =
    Item(
      id = -1L,
      name = name(rune, size),
      lvl = 1L,
      rarity = Rarity.Gray,
      itemType = ItemType.RuneStone,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.RuneStone(rune.key, size)
    )
}
