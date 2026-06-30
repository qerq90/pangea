package pangea.model.state

import enumeratum._
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax.EncoderOps

sealed trait StateType extends EnumEntry

object StateType extends Enum[StateType] with DoobieEnum[StateType] {

  val values = findValues

  implicit val encoder: Encoder[StateType] = (s: StateType) => s.entryName.asJson
  implicit val decoder: Decoder[StateType] = (c: HCursor) => c.as[String].map(StateType.withName)

  // Battle 40% · FoundItem 20% · Spring 20% · GoldVein 10% · TreasureMobs 5% ·
  // TreasureDig 5% (вес = число повторов в пуле, сумма = 100).
  val events: List[StateType] =
    List.fill(40)(Battle) ++ List.fill(20)(FoundItem) ++ List.fill(20)(Spring) ++
      List.fill(10)(GoldVein) ++ List.fill(5)(TreasureMobs) ++ List.fill(5)(TreasureDig)

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

  // События-«сокровища» подземелья.
  case object TreasureMobs     extends StateType // встреча мобов, выкопавших сокровище (интро с выбором)
  case object TreasureMobsFight extends StateType // эффект-нода: спавн очередного боя цепочки
  case object TreasureSchron   extends StateType // эффект-нода: выдача схрона после цепочки боёв
  case object TreasureDig      extends StateType // прикопанный схрон (раскопки по таймеру)
}
