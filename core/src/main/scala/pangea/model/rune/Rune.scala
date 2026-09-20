package pangea.model.rune

import pangea.model.item.{Item, ItemDetails, PassiveKind, Rarity}
import pangea.model.skill.Skill

/** Руна — узор на зачарованной вещи: боевая (активный навык оружия или
  * нагрудника) либо пассивная (пассивка остального снаряжения). Казимир умеет
  * снять узор с вещи и выжечь на теле героя (см. [[RuneData]]), а понимание
  * руны — общее для всех её носителей: усиливает и клеймо, и надетую вещь. */
sealed trait Rune {
  /** Ключ в хранилище: боевые и пассивные руны не пересекаются по имени, но
    * префикс защищает от совпадения на будущее. */
  def key: String
  def label: String
  def description: String
  def isActive: Boolean
}

object Rune {

  final case class Active(skill: Skill) extends Rune {
    def key: String         = ActivePrefix + skill.entryName
    def label: String       = skill.label
    def description: String = skill.description.replace("{}", "…")
    def isActive: Boolean   = true
  }

  final case class Passive(kind: PassiveKind) extends Rune {
    def key: String         = PassivePrefix + kind.entryName
    def label: String       = kind.label
    def description: String = kind.describe
    def isActive: Boolean   = false
  }

  val ActivePrefix: String  = "s:"
  val PassivePrefix: String = "p:"

  /** Мест на теле под клеймо: боевых и пассивных. */
  val ActiveSlots: Int  = 2
  val PassiveSlots: Int = 4

  /** Стоимость клейма: 500 · (1 + рун на теле), в репутации искателей. */
  val CostBase: Long = 500L

  /** Потолок понимания — 30 на уровень героя. */
  val CapPerLevel: Long = 30L

  /** Руна на вещи, если она есть. */
  def of(item: Item): Option[Rune] = item.details match {
    case ItemDetails.Weapon(s) => Some(Active(s))
    case ItemDetails.Armor(s)  => Some(Active(s))
    case _                     => item.passive.map(Passive(_))
  }

  def byKey(key: String): Option[Rune] =
    if (key.startsWith(ActivePrefix)) Skill.withNameOption(key.stripPrefix(ActivePrefix)).map(Active(_))
    else if (key.startsWith(PassivePrefix)) PassiveKind.withNameOption(key.stripPrefix(PassivePrefix)).map(Passive(_))
    else None

  /** Сколько понимания даёт сожжённая вещь: по редкости. Обе фиолетовые — одинаково. */
  def points(rarity: Rarity): Long = rarity match {
    case Rarity.Gray | Rarity.White | Rarity.Green => 1L
    case Rarity.Blue                               => 2L
    case Rarity.Purple | Rarity.Violet             => 3L
    case Rarity.Orange                             => 5L
  }

  /** Сила руны в процентах от понимания: квадратичная шкала — 1000 понимания
    * дают +100 %, 4000 — +200 %. */
  def strengthPct(understanding: Long): Double =
    100.0 * math.sqrt(understanding.max(0L) / 1000.0)

  /** Множитель к числу руны: 1 + сила/100. */
  def mult(understanding: Long): Double = 1.0 + strengthPct(understanding) / 100.0

  /** Потолок понимания для героя этого уровня. */
  def cap(lvl: Long): Long = CapPerLevel * lvl.max(1L)

  /** «+17,3%» для экрана. */
  def strengthText(understanding: Long): String = f"+${strengthPct(understanding)}%.1f%%".replace('.', ',')

  /** Id боевого слота клейма в бою — отрицательный, чтобы не пересечься с id
    * предмета (кнопка `Skill_<id>` и слот `SkillSlotState.itemId`). */
  def bodySlotId(skill: Skill): Long = -(1L + Skill.values.indexOf(skill).toLong)

  def isBodySlot(itemId: Long): Boolean = itemId < 0L
}
