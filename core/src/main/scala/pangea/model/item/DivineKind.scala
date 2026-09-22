package pangea.model.item

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import pangea.model.battle.Element

/** Божественное оружие — клинок, перекованный в кубе Азата из чужого оружия и
 *  восьми надколотых камней (см. `CubeCraft`). Надевается в доп. слот, статов не
 *  даёт вовсе: вся его сила в ударе по всему полю боя, который он держит
 *  считанное число раз, — а потом рассыпается в руках.
 *
 *  Вид определяется камнем, которого при ковке было больше (шансы — по числу
 *  камней), стихия — стихией этого камня; у черепа и аметиста стихии нет, у них
 *  своя грань. */
sealed abstract class DivineKind(
  val gem:         GemKind,
  val itemName:    String,
  val description: String
) extends EnumEntry {

  /** Стихия удара: та же, что у камня (см. [[Element.of]]). */
  def element: Option[Element] = Element.of(gem)
}

/** Числа божественного оружия — отдельно от компаньона: варианты читают их в конструкторе
 *  (см. FlaskRates и заметку про дедлок инициализации). */
object DivineRates {
  /** Урон удара по каждому врагу: ставка × уровень божественного оружия. */
  val DamagePerLvl: Long = 55L

  /** Сколько ударов держит божественное оружие: база + грейд редкости (⚫ 1 … 🟠 7). */
  val BaseCharges: Int = 5

  /** Сколько раундов держатся эффекты аметиста и бриллианта. */
  val BuffRounds: Int = 5

  /** Шанс героя попасть, пока действует грань аметиста (в %). */
  val TrueStrikePct: Long = 95L

  /** Сколько уклонения и точности даёт бриллиант за каждого задетого врага (в %). */
  val HastePctPerFoe: Long = 5L

  /** Доля нанесённого урона, которую череп возвращает герою в HP (в %). */
  val DrainPct: Long = 50L
}

object DivineKind extends Enum[DivineKind] {

  case object DarkLordSword extends DivineKind(GemKind.Skull, "Меч Тёмного Владыки",
    "Клинок, выкованный на чужих костях: бьёт по всему полю разом и возвращает вам половину отнятой жизни. " +
    "Держится в руках через силу — рано или поздно развалится прямо в ладонях.")

  case object FireLordAxe extends DivineKind(GemKind.Ruby, "Топор Владыки Огня",
    "Лезвие светится изнутри, как жерло: один взмах — и пламя охватывает всех, кто стоит напротив. " +
    "Древко трещит от жара; такой топор долго не живёт.")

  case object AmethystGoddess extends DivineKind(GemKind.Amethyst, "Частица Аметистовой Богини",
    "Осколок чужого божества: после удара вы видите врагов насквозь и почти не мажете. " +
    "Камень идёт трещинами с каждым разом — однажды он просто осыплется.")

  case object WhiteDeathMace extends DivineKind(GemKind.Sapphire, "Булава Белой Смерти",
    "Навершие покрыто инеем, который не тает: удар выстуживает всё поле боя. " +
    "Рукоять обжигает холодом — она не выдержит долго.")

  case object SnakeGodBlade extends DivineKind(GemKind.Emerald, "Клинок Бога Змеи",
    "Зелёная сталь сочится ядом: один взмах травит всех врагов разом. " +
    "Яд ест и сам клинок — он расползётся в руках.")

  case object WindLordStaff extends DivineKind(GemKind.Diamond, "Посох Владыки Ветров",
    "Внутри посоха заперта буря: она сносит врагов и подхватывает вас, делая быстрее с каждым задетым. " +
    "Древко ходит ходуном — однажды буря вырвется и разнесёт его.")

  case object ThunderGodTrident extends DivineKind(GemKind.Topaz, "Трезубец бога молний",
    "Зубцы гудят от накопленного разряда и бьют молнией по всему строю врага. " +
    "Разряд копится и в самом трезубце — он не вечен.")

  val values: IndexedSeq[DivineKind] = findValues

  /** Какое божественное оружие куётся на этом камне. */
  def of(gem: GemKind): DivineKind = values.find(_.gem == gem).getOrElse(DarkLordSword)

  /** Сколько ударов держит божественное оружие этой редкости: база и по одному за грейд. */
  def chargesFor(rarity: Rarity): Int = DivineRates.BaseCharges + Rarity.values.indexOf(rarity) + 1

  /** Готовый предмет: уровень и редкость — от оружия, ушедшего в ковку. */
  def item(kind: DivineKind, lvl: Long, rarity: Rarity): Item = {
    val charges = chargesFor(rarity)
    Item(
      id = -1L,
      name = s"${rarity.emoji} ${kind.itemName}",
      lvl = lvl,
      rarity = rarity,
      itemType = ItemType.AdditionalWeapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Divine(kind, charges, charges)
    )
  }

  implicit val encoder: Encoder[DivineKind] = (k: DivineKind) => k.entryName.asJson
  implicit val decoder: Decoder[DivineKind] = (c: HCursor) => c.as[String].map(DivineKind.withName)
}
