package pangea.model.event

import enumeratum._

sealed trait DungeonEventStatus extends EnumEntry

object DungeonEventStatus
    extends Enum[DungeonEventStatus]
    with DoobieEnum[DungeonEventStatus] {

  override def values: IndexedSeq[DungeonEventStatus] = findValues

  case object InProgress extends DungeonEventStatus
  case object Ended      extends DungeonEventStatus
}
