package pangea.model.item.stats

import enumeratum._

sealed trait Stat extends EnumEntry

object Stat extends Enum[Stat] {

  override def values: IndexedSeq[Stat] = findValues

  case object Attack        extends Stat
  case object Accuracy      extends Stat
  case object Concentration extends Stat
  case object Armor         extends Stat
  case object Defence       extends Stat
  case object Evasion       extends Stat
}
