package pangea.model.item

import enumeratum._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import pangea.model.battle.Element

/** Реликвия — оружие, перекованное в кубе Азата из чужого клинка и восьми
 *  надколотых камней (см. `CubeCraft`). Надевается в доп. слот, статов не даёт
 *  вовсе: вся её сила в ударе по всему полю боя, который она держит считанное
 *  число раз, — а потом рассыпается в руках.
 *
 *  Вид определяется камнем, которого при ковке было больше (шансы — по числу
 *  камней), стихия — стихией этого камня; у черепа и аметиста стихии нет, у них
 *  своя грань. */
sealed abstract class RelicKind(
  val gem:         GemKind,
  val itemName:    String,
  val description: String
) extends EnumEntry {

  /** Стихия удара: та же, что у камня (см. [[Element.of]]). */
  def element: Option[Element] = Element.of(gem)
}

/** Числа реликвий — отдельно от компаньона: варианты читают их в конструкторе
 *  (см. FlaskRates и заметку про дедлок инициализации). */
object RelicRates {
  /** Урон удара по каждому врагу: ставка × уровень реликвии. */
  val DamagePerLvl: Long = 55L

  /** Сколько ударов держит реликвия: база + грейд редкости (⚫ 1 … 🟠 7). */
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

object RelicKind extends Enum[RelicKind] {

  case object DarkLordSword extends RelicKind(GemKind.Skull, "Меч Тёмного Владыки",
    "Клинок, выкованный на чужих костях: бьёт по всему полю разом и возвращает вам половину отнятой жизни. " +
    "Держится в руках через силу — рано или поздно развалится прямо в ладонях.")

  case object FireLordAxe extends RelicKind(GemKind.Ruby, "Топор Владыки Огня",
    "Лезвие светится изнутри, как жерло: один взмах — и пламя охватывает всех, кто стоит напротив. " +
    "Древко трещит от жара; такой топор долго не живёт.")

  case object AmethystGoddess extends RelicKind(GemKind.Amethyst, "Частица Аметистовой Богини",
    "Осколок чужого божества: после удара вы видите врагов насквозь и почти не мажете. " +
    "Камень идёт трещинами с каждым разом — однажды он просто осыплется.")

  case object WhiteDeathMace extends RelicKind(GemKind.Sapphire, "Булава Белой Смерти",
    "Навершие покрыто инеем, который не тает: удар выстуживает всё поле боя. " +
    "Рукоять обжигает холодом — она не выдержит долго.")

  case object SnakeGodBlade extends RelicKind(GemKind.Emerald, "Клинок Бога Змеи",
    "Зелёная сталь сочится ядом: один взмах травит всех врагов разом. " +
    "Яд ест и сам клинок — он расползётся в руках.")

  case object WindLordStaff extends RelicKind(GemKind.Diamond, "Посох Владыки Ветров",
    "Внутри посоха заперта буря: она сносит врагов и подхватывает вас, делая быстрее с каждым задетым. " +
    "Древко ходит ходуном — однажды буря вырвется и разнесёт его.")

  case object ThunderGodTrident extends RelicKind(GemKind.Topaz, "Трезубец бога молний",
    "Зубцы гудят от накопленного разряда и бьют молнией по всему строю врага. " +
    "Разряд копится и в самом трезубце — он не вечен.")

  val values: IndexedSeq[RelicKind] = findValues

  /** Реликвия камня, если из него куют. */
  def of(gem: GemKind): RelicKind = values.find(_.gem == gem).getOrElse(DarkLordSword)

  /** Сколько ударов держит реликвия этой редкости: база и по одному за грейд. */
  def chargesFor(rarity: Rarity): Int = RelicRates.BaseCharges + Rarity.values.indexOf(rarity) + 1

  /** Готовый предмет: уровень и редкость — от оружия, ушедшего в ковку. */
  def item(kind: RelicKind, lvl: Long, rarity: Rarity): Item = {
    val charges = chargesFor(rarity)
    Item(
      id = -1L,
      name = s"${rarity.emoji} ${kind.itemName}",
      lvl = lvl,
      rarity = rarity,
      itemType = ItemType.AdditionalWeapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      details = ItemDetails.Relic(kind, charges, charges)
    )
  }

  implicit val encoder: Encoder[RelicKind] = (k: RelicKind) => k.entryName.asJson
  implicit val decoder: Decoder[RelicKind] = (c: HCursor) => c.as[String].map(RelicKind.withName)
}
