package pangea.model.state

import enumeratum._

sealed trait StateType extends EnumEntry

object StateType extends Enum[StateType] with DoobieEnum[StateType] {

  val values = findValues

  // Battle 45% · FoundItem 22% · Spring 23% · GoldVein 10% (вес = число повторов в пуле).
  val events: List[StateType] =
    List.fill(45)(Battle) ++ List.fill(22)(FoundItem) ++ List.fill(23)(Spring) ++ List.fill(10)(GoldVein)

  case object GlobalMap     extends StateType
  case object HarborQuarter    extends StateType
  case object MarketSquare     extends StateType
  case object UnassumingBarrel extends StateType
  case object Registration extends StateType
  case object Dungeon      extends StateType
  case object HeroStats    extends StateType
  case object Battle       extends StateType
  case object Death        extends StateType

  case object FoundItem extends StateType
  case object Rest      extends StateType
  case object Spring    extends StateType
  case object Inventory  extends StateType
  case object Equipment  extends StateType
  case object Loot       extends StateType
  case object Merchant   extends StateType
  case object Tavern     extends StateType
  case object QuestBoard extends StateType
  case object Innkeeper  extends StateType
  case object GoldVein       extends StateType
  case object Construction   extends StateType
  case object Guild          extends StateType
  case object TrophyExchange extends StateType
  case object TrainingHall   extends StateType
  case object MasterHorn     extends StateType
  case object MentorKazimir  extends StateType
}
