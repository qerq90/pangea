package pangea.model.state

import enumeratum._
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax.EncoderOps

sealed trait StateType extends EnumEntry

object StateType extends Enum[StateType] with DoobieEnum[StateType] {

  val values = findValues

  implicit val encoder: Encoder[StateType] = (s: StateType) => s.entryName.asJson
  implicit val decoder: Decoder[StateType] = (c: HCursor) => c.as[String].map(StateType.withName)

  // Battle 38% · FlowerMeadow 2% · FoundItem 19% · Spring 18% · Girl 2% · SilverVein 10% ·
  // TreasureMobs 5% · TreasureDig 5% · ElementalLair 1% (вес = число повторов в пуле,
  // сумма = 100). Процент под логово элементаля забран у боя, под девушку — у ручья,
  // под поляну цветов — по одному у боя и у находки. Поляна стоит сразу за боем,
  // чтобы билеты событий дальше по списку не сдвигались.
  val BattleTickets: Int = 38

  val events: List[StateType] =
    List.fill(BattleTickets)(Battle) ++ List.fill(2)(FlowerMeadow) ++ List.fill(19)(FoundItem) ++
      List.fill(18)(Spring) ++ List.fill(2)(Girl) ++
      List.fill(10)(SilverVein) ++ List.fill(5)(TreasureMobs) ++ List.fill(5)(TreasureDig) ++
      List.fill(1)(ElementalLair)

  /** Пул событий с изменённым весом боя. `battleFactor` множит число «билетов»
   *  Battle в пуле: пассивки «Охотник» (×1.2) повышают долю боёв, «Скрытность»
   *  (×0.8) — понижает (см. [[pangea.model.hero.HeroPassives.battleEncounterFactor]]).
   *  Прочие события не трогаются — их абсолютный вес прежний, а относительная доля
   *  сдвигается за счёт изменения общего числа билетов. */
  def eventsWithBattleFactor(battleFactor: Double): List[StateType] = {
    val battleTickets = (BattleTickets * battleFactor).round.toInt.max(0)
    List.fill(battleTickets)(Battle) ++ events.filter(_ != Battle)
  }

  case object GlobalMap     extends StateType
  case object HarborQuarter    extends StateType
  case object MarketSquare     extends StateType
  case object CityCenter       extends StateType // Центр города (Храм Азата)
  case object TempleAzat       extends StateType // Храм Азата (Жрец / Зал / Уйти)
  case object HallAzat         extends StateType // Зал Азата (кубы, заряды)
  case object Cube             extends StateType // крафт в кубе Азата
  case object UnassumingBarrel extends StateType
  case object TradeHouse       extends StateType // Торговый дом возле Храма (банкир Рахадим)
  case object BankVault        extends StateType // хранилище героя в Торговом доме
  case object Auction          extends StateType // аукцион Торгового дома
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
  case object Skills     extends StateType // список активных умений/пассивок героя
  case object Socketing  extends StateType // вставка камня-усилителя в гнездо снаряжения
  case object Loot       extends StateType
  case object RottenJoe  extends StateType // встреча с Гнилым Джо на раскопках
  case object ElementalLair extends StateType // логово элементаля: подход к минибоссу
  case object ElementalSearch extends StateType // осмотр логова после победы (афк-поиск камней)
  case object Merchant   extends StateType
  case object Gustavo         extends StateType
  case object GustavoHeal     extends StateType
  case object GustavoBoost    extends StateType
  case object GustavoSupplies extends StateType
  case object GustavoFlask    extends StateType
  case object GustavoBelt     extends StateType
  case object Tavern     extends StateType
  case object QuestBoard extends StateType
  case object Innkeeper  extends StateType
  case object CardSeller extends StateType // «Подозрительный человек» — продавец карт
  case object SilverVein     extends StateType
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
  case object Girl             extends StateType // девушка с криком «Помогите!» и трое вооружённых
  case object FlowerMeadow     extends StateType // поляна цветов: сбор трав по таймеру
  case object Knowledge        extends StateType // знания героя (меню персонажа)
  case object GustavoHerbs     extends StateType // Густаво: рассказ о травах и трактаты
  case object Squad            extends StateType // отряд героя (меню персонажа)
  case object Mercenaries      extends StateType // наёмники в таверне

  // Поход за сокровищем по карте клада.
  case object Outskirts    extends StateType // «За городом»: выбор карты и отправка в поход
  case object TreasureHunt extends StateType // сам поход по таймеру (~10 минут) → добыча

  // «Письмо Марисе».
  case object MarisaSearch extends StateType // поиски Долорес и Марисы в Портовом квартале
  case object MarisaHunt   extends StateType // поход к тайнику Кельвина по таймеру и встреча с коллектором

  // «Деревня Мурлоков».
  case object MurlocElder   extends StateType // старейшина выходит к герою после сотого убитого
  case object MurlocVillage extends StateType // по карте: налёт на деревню или помощь снаряжением

  /** Городские сцены — где герой «в городе» (сюжетную карту можно открыть только
    * отсюда). Смотрится по `returnState` меню персонажа. */
  val cityStates: Set[StateType] = Set(
    GlobalMap, HarborQuarter, MarketSquare, CityCenter, TempleAzat, HallAzat, Cube, UnassumingBarrel,
    TradeHouse, BankVault, Auction,
    Merchant, Gustavo, GustavoHeal, GustavoBoost, GustavoSupplies, GustavoFlask, GustavoBelt,
    Tavern, QuestBoard, Innkeeper, CardSeller, Construction, Guild, TrophyExchange, TrainingHall,
    MasterHorn, MentorKazimir)
}
