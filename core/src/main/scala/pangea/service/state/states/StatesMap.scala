package pangea.service.state.states

import pangea.dao.hero.HeroDao
import pangea.engine.{GraphValidator, Journal, Players, SceneContent}
import pangea.model.state.StateType
import pangea.model.state.StateType.{Battle, Death, Dungeon, Equipment, FoundItem, GlobalMap, GoldVein, HeroStats, Innkeeper, Inventory, Loot, Merchant, QuestBoard, Registration, Rest, Tavern}
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.service.schedule.Scheduler
import pangea.service.state.State
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.dungeon.DungeonState
import pangea.service.state.states.events.GoldVeinState
import pangea.service.state.states.events.item.FoundItemState
import pangea.service.state.states.merchant.MerchantState
import pangea.service.state.states.registration.RegistrationState
import pangea.service.state.states.hero.HeroStatsState
import pangea.service.state.states.tavern.{InnkeeperState, QuestBoardState, TavernState}
import zio.{ZIO, ZLayer}

case class StatesMap(states: Map[StateType, State])

object StatesMap {
  val live: ZLayer[
    Players with HeroDao with InventoryRepository with ItemRepository with Journal with SceneContent with Scheduler,
    Throwable,
    StatesMap
  ] =
    ZLayer.fromZIO(
      for {
        players       <- ZIO.service[Players]
        heroDao       <- ZIO.service[HeroDao]
        inventoryRepo <- ZIO.service[InventoryRepository]
        itemRepo      <- ZIO.service[ItemRepository]
        journal       <- ZIO.service[Journal]
        content       <- ZIO.service[SceneContent]
        scheduler     <- ZIO.service[Scheduler]
        states = Map[StateType, State](
          GlobalMap    -> GlobalMapState(heroDao, content),
          Registration -> RegistrationState(players, heroDao, inventoryRepo, itemRepo, journal, content),
          Dungeon      -> DungeonState(heroDao, inventoryRepo, content),
          HeroStats    -> HeroStatsState(heroDao, content),
          FoundItem    -> FoundItemState(heroDao, inventoryRepo, itemRepo, journal, content),
          Battle       -> BattleState(heroDao, content),
          Death        -> DeathState(heroDao, inventoryRepo, content),
          Rest         -> RestState(heroDao, scheduler, content),
          Inventory    -> InventoryState(heroDao, inventoryRepo, content),
          Equipment    -> EquipmentState(heroDao, inventoryRepo, content),
          Loot         -> LootState(heroDao, inventoryRepo, itemRepo, journal, content),
          Merchant     -> MerchantState(heroDao, inventoryRepo, itemRepo, content),
          Tavern       -> TavernState(heroDao, scheduler, content),
          QuestBoard   -> QuestBoardState(heroDao, content),
          Innkeeper    -> InnkeeperState(heroDao, inventoryRepo, content),
          GoldVein     -> GoldVeinState(heroDao, scheduler, content)
        )
        _ <- GraphValidator.validate(states)
      } yield new StatesMap(states)
    )
}
