package pangea.model.item

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import pangea.model.battle.Element

/** Раскрывшаяся роза: цветок, который вышел из кубa Азата, когда Нераскрытую
 *  розу присыпали пылью самоцвета со спящей стихией. Носится в доп. слоте и
 *  бьёт по всему полю, как божественное оружие, — только силу берёт от уровня
 *  хозяина, а не от своего, и держит ровно [[RoseRates.Charges]] раскрытий.
 *
 *  Аметистовая пыль и Чёрный порошок розу не берут: в аметисте сидит точность,
 *  в черепе — вампиризм, стихии нет ни там, ни там. */
sealed abstract class RoseKind(
  val dust:        MaterialKind,
  val itemName:    String,
  val description: String,
  /** Стихия удара; у зелёной её нет — она травит (см. [[poisons]]). */
  val element:     Option[Element],
  /** Оставляет ли на врагах яд, как Клинок Бога Змеи. Значения по умолчанию
    * здесь быть не может: дефолт конструктора компилируется в метод
    * companion-объекта, и вариант при создании ждал бы `RoseKind$`, который в
    * этот момент сам инициализирует варианты, — дедлок на ровном месте. */
  val poisons:     Boolean
) extends EnumEntry

/** Числа роз — отдельно от компаньона: варианты читают их в конструкторе
 *  (см. FlaskRates и заметку про дедлок инициализации). */
object RoseRates {
  /** Сколько раскрытий держит цветок. */
  val Charges: Int = 3
}

object RoseKind extends Enum[RoseKind] {

  case object White extends RoseKind(MaterialKind.DiamondDust, "Белая роза",
    "Раскрылась и больше не закроется. От сердцевины во все стороны бьют струи воздуха — ровные, без порывов, " +
    "будто цветок дышит сразу всеми лепестками. Пыль вокруг неё не оседает, а стоит облаком на высоте ладони. " +
    "В сумке держать неудобно: всё лёгкое из неё выдувает наружу.",
    Some(Element.Air), poisons = false)

  case object Yellow extends RoseKind(MaterialKind.TopazDust, "Жёлтая роза",
    "Лепестки жёсткие и золотистые, и между ними без остановки трещат мелкие разряды. В темноте видно, " +
    "как искры перебегают от края к краю, будто цветок снова и снова пересчитывает свои лепестки. " +
    "Взять голой рукой можно. Потом полдня немеют пальцы.",
    Some(Element.Lightning), poisons = false)

  case object Green extends RoseKind(MaterialKind.EmeraldDust, "Зелёная роза",
    "Тяжёлый зелёный газ сползает с лепестков и стелется по земле, не поднимаясь выше колена. Пахнет сладко — " +
    "и это худшее, что о ней можно сказать: на сладкий запах приходят, а обратно уже не уходят. " +
    "Трава под ней чернеет за одну ночь.",
    None, poisons = true)

  case object Red extends RoseKind(MaterialKind.RubyDust, "Красная роза",
    "Горит и не сгорает. Пламя облегает лепестки, как вода облегает камень, и держится того же цвета, " +
    "что и сам цветок, — где кончается лепесток и начинается огонь, разглядишь не сразу. Рядом с ней тепло, " +
    "как у печи, а к утру всё, что лежало поблизости, оказывается сухим до ломкости.",
    Some(Element.Fire), poisons = false)

  case object Blue extends RoseKind(MaterialKind.SapphireDust, "Синяя роза",
    "Иней нарастает на ней быстрее, чем тает: соскоблишь — через десяток вдохов он снова на месте. " +
    "Лепестки под коркой синие до черноты и звенят, если задеть ногтем. Воду во фляге рядом с ней " +
    "прихватывает коркой даже в жару.",
    Some(Element.Cold), poisons = false)

  val values: IndexedSeq[RoseKind] = findValues

  /** Какая роза раскроется от этой пыли. Пыль аметиста и черепа — не отсюда. */
  def fromDust(dust: MaterialKind): Option[RoseKind] = values.find(_.dust == dust)

  /** Готовый цветок: уровня у розы нет, силу она берёт от хозяина. */
  def item(kind: RoseKind): Item =
    Item(
      id = -1L,
      name = kind.itemName,
      lvl = 0L,
      rarity = Rarity.Orange,
      itemType = ItemType.AdditionalWeapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Rose(kind, RoseRates.Charges, RoseRates.Charges)
    )

  implicit val encoder: Encoder[RoseKind] = (k: RoseKind) => k.entryName.asJson
  implicit val decoder: Decoder[RoseKind] = (c: HCursor) => c.as[String].map(RoseKind.withName)
}
