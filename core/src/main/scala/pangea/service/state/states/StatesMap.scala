package pangea.service.state.states

import pangea.dao.hero.HeroDao
import pangea.engine.{GraphValidator, Journal, Players, SceneContent}
import pangea.model.state.StateType
import pangea.model.state.StateType.{
  Battle,
  CardSeller,
  CityCenter,
  Construction,
  Cube,
  Death,
  Dungeon,
  ElementalLair,
  ElementalSearch,
  Equipment,
  FoundItem,
  GlobalMap,
  SilverVein,
  Guild,
  Gustavo,
  GustavoBoost,
  GustavoBelt,
  GustavoFlask,
  GustavoHeal,
  GustavoSupplies,
  HallAzat,
  HarborQuarter,
  HeroStats,
  Innkeeper,
  Inventory,
  Loot,
  MarketSquare,
  MasterHorn,
  MentorKazimir,
  Merchant,
  Outskirts,
  QuestBoard,
  Registration,
  Rest,
  RottenJoe,
  Skills,
  Socketing,
  Tavern,
  TempleAzat,
  TrainingHall,
  TreasureDig,
  Girl,
  FlowerMeadow,
  Knowledge,
  Squad,
  Mercenaries,
  GustavoHerbs,
  MarisaSearch,
  MarisaHunt,
  MurlocElder,
  MurlocVillage,
  TreasureHunt,
  TreasureMobs,
  TreasureMobsFight,
  TreasureSchron,
  TrophyExchange,
  UnassumingBarrel,
  TradeHouse,
  BankVault,
  Auction,
  FetShop,
  Backpack,
  Casket,
  LivingBag,
  Wardrobe,
  Transfer,
  Mail
}
import pangea.repository.artifact.ArtifactRepository
import pangea.repository.auction.AuctionRepository
import pangea.repository.bank.BankRepository
import pangea.repository.barrel.BarrelRepository
import pangea.repository.inventory.InventoryRepository
import pangea.repository.item.ItemRepository
import pangea.repository.user.UserRepository
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.payout.Payouts
import pangea.service.schedule.Scheduler
import pangea.service.state.State
import pangea.model.artifact.ArtifactKind
import pangea.service.state.states.artifact.{ArtifactState, BackpackState, FetShopState}
import pangea.service.state.states.bank.{AuctionState, BankVaultState, TradeHouseState}
import pangea.service.state.states.parcel.{MailState, TransferState}
import pangea.service.state.states.battle.BattleState
import pangea.service.state.states.dungeon.DungeonState
import pangea.service.state.states.events.{ElementalLairState, ElementalSearchState, FlowerMeadowState, GirlState, RottenJoeState, SilverVeinState}
import pangea.service.state.states.temple.{CubeState, HallAzatState, TempleAzatState}
import pangea.service.state.states.events.treasure.{
  TreasureDigState,
  TreasureHuntState,
  TreasureMobsFightState,
  TreasureMobsState,
  TreasureSchronState
}
import pangea.service.state.states.guild.{
  GuildState,
  MasterHornState,
  MentorKazimirState,
  TrainingHallState,
  TrophyExchangeState
}
import pangea.service.state.states.events.item.FoundItemState
import pangea.service.state.states.marisa.{MarisaHuntState, MarisaSearchState}
import pangea.service.state.states.murloc.{MurlocElderState, MurlocVillageState}
import pangea.service.state.states.merchant.MerchantState
import pangea.service.state.states.gustavo.{
  GustavoBoostState,
  GustavoBeltState,
  GustavoFlaskState,
  GustavoHealState,
  GustavoHerbsState,
  GustavoState,
  GustavoSuppliesState
}
import pangea.service.state.states.registration.RegistrationState
import pangea.service.state.states.hero.{HeroStatsState, KnowledgeState, SkillsState, SquadState}
import pangea.service.state.states.tavern.{
  CardSellerState,
  InnkeeperState,
  MercenariesState,
  QuestBoardState,
  TavernState
}
import zio.{ZIO, ZLayer}

case class StatesMap(states: Map[StateType, State])

object StatesMap {
  val live: ZLayer[
    Players
      with HeroDao
      with InventoryRepository
      with BarrelRepository
      with BankRepository
      with AuctionRepository
      with ArtifactRepository
      with Payouts
      with Parcels
      with Transfers
      with ItemRepository
      with UserRepository
      with Journal
      with SceneContent
      with Scheduler,
    Throwable,
    StatesMap
  ] =
    ZLayer.fromZIO(
      for {
        players       <- ZIO.service[Players]
        heroDao       <- ZIO.service[HeroDao]
        inventoryRepo <- ZIO.service[InventoryRepository]
        barrelRepo    <- ZIO.service[BarrelRepository]
        bankRepo      <- ZIO.service[BankRepository]
        auctionRepo   <- ZIO.service[AuctionRepository]
        artifactRepo  <- ZIO.service[ArtifactRepository]
        payouts       <- ZIO.service[Payouts]
        parcels       <- ZIO.service[Parcels]
        transfers     <- ZIO.service[Transfers]
        itemRepo      <- ZIO.service[ItemRepository]
        userRepo      <- ZIO.service[UserRepository]
        journal       <- ZIO.service[Journal]
        content       <- ZIO.service[SceneContent]
        scheduler     <- ZIO.service[Scheduler]
        // Кошель героя: своё серебро, а следом — ячейка в Торговом доме.
        bank           = Some(bankRepo)
        // Ларец Азата и Живая сумка ловят добычу до того, как она попадёт в сумку.
        artifacts      = Some(artifactRepo)
        states = Map[StateType, State](
          GlobalMap     -> GlobalMapState(heroDao, content),
          HarborQuarter -> HarborQuarterState(content),
          MarketSquare  -> MarketSquareState(content),
          UnassumingBarrel -> UnassumingBarrelState(
            heroDao,
            inventoryRepo,
            barrelRepo,
            content
          ),
          Registration -> RegistrationState(
            players,
            heroDao,
            inventoryRepo,
            itemRepo,
            journal,
            content
          ),
          Dungeon   -> DungeonState(heroDao, inventoryRepo, scheduler, content),
          HeroStats -> HeroStatsState(heroDao, content),
          FoundItem -> FoundItemState(
            heroDao,
            inventoryRepo,
            itemRepo,
            journal,
            content,
            artifacts
          ),
          Battle -> BattleState(heroDao, inventoryRepo, itemRepo, content, scheduler),
          Death  -> DeathState(heroDao, inventoryRepo, content, artifacts),
          Rest   -> RestState(heroDao, scheduler, content),
          Inventory -> InventoryState(
            heroDao,
            inventoryRepo,
            itemRepo,
            content
          ),
          Equipment -> EquipmentState(heroDao, inventoryRepo, content),
          Skills    -> SkillsState(heroDao, content),
          Knowledge -> KnowledgeState(heroDao, content),
          Squad -> SquadState(heroDao, content),
          ElementalLair -> ElementalLairState(heroDao, content),
          RottenJoe     -> RottenJoeState(heroDao, content),
          ElementalSearch -> ElementalSearchState(heroDao, inventoryRepo, itemRepo, scheduler, content),
          Socketing -> SocketingState(heroDao, inventoryRepo, content),
          CityCenter -> CityCenterState(content),
          TradeHouse -> TradeHouseState(heroDao, bankRepo, parcels, content),
          Transfer   -> TransferState(heroDao, inventoryRepo, transfers, content),
          Mail       -> MailState(heroDao, inventoryRepo, parcels, content),
          BankVault  -> BankVaultState(heroDao, inventoryRepo, bankRepo, content),
          Auction    -> AuctionState(heroDao, inventoryRepo, itemRepo, auctionRepo, userRepo, bankRepo, payouts, players, content),
          FetShop    -> FetShopState(heroDao, artifactRepo, content),
          Backpack   -> BackpackState(heroDao, inventoryRepo, artifactRepo, content),
          Casket     -> ArtifactState(ArtifactKind.Casket, heroDao, inventoryRepo, itemRepo, artifactRepo, content),
          LivingBag  -> ArtifactState(ArtifactKind.LivingBag, heroDao, inventoryRepo, itemRepo, artifactRepo, content),
          Wardrobe   -> ArtifactState(ArtifactKind.Wardrobe, heroDao, inventoryRepo, itemRepo, artifactRepo, content),
          TempleAzat -> TempleAzatState(heroDao, inventoryRepo, itemRepo, content),
          HallAzat   -> HallAzatState(heroDao, content, bank, artifacts),
          Cube       -> CubeState(heroDao, inventoryRepo, itemRepo, content),
          Loot -> LootState(heroDao, inventoryRepo, itemRepo, journal, content, artifacts),
          Merchant -> MerchantState(heroDao, inventoryRepo, itemRepo, content, bank),
          Gustavo  -> GustavoState(heroDao, inventoryRepo, content),
          GustavoHerbs    -> GustavoHerbsState(heroDao, inventoryRepo, itemRepo, content, bank),
          GustavoHeal     -> GustavoHealState(heroDao, content, bank),
          GustavoBoost    -> GustavoBoostState(heroDao, content, bank),
          GustavoSupplies -> GustavoSuppliesState(heroDao, content),
          GustavoFlask    -> GustavoFlaskState(heroDao, content, bank),
          GustavoBelt     -> GustavoBeltState(heroDao, content, bank),
          Tavern          -> TavernState(heroDao, scheduler, content, bank),
          CardSeller -> CardSellerState(
            heroDao,
            inventoryRepo,
            itemRepo,
            content
          ),
          QuestBoard        -> QuestBoardState(heroDao, content),
          Innkeeper         -> InnkeeperState(heroDao, inventoryRepo, content, bank),
          Mercenaries       -> MercenariesState(heroDao, inventoryRepo, content, bank),
          SilverVein        -> SilverVeinState(heroDao, scheduler, content),
          TreasureMobs      -> TreasureMobsState(heroDao, content),
          TreasureMobsFight -> TreasureMobsFightState(heroDao, content),
          TreasureSchron    -> TreasureSchronState(heroDao, content),
          TreasureDig       -> TreasureDigState(heroDao, scheduler, content),
          Girl              -> GirlState(heroDao, inventoryRepo, itemRepo, barrelRepo, scheduler, content, bank),
          FlowerMeadow      -> FlowerMeadowState(heroDao, inventoryRepo, itemRepo, scheduler, content, artifacts),
          MarisaSearch      -> MarisaSearchState(heroDao, inventoryRepo, itemRepo, content),
          MarisaHunt        -> MarisaHuntState(heroDao, inventoryRepo, scheduler, content, bank),
          MurlocElder       -> MurlocElderState(heroDao, inventoryRepo, itemRepo, content),
          MurlocVillage     -> MurlocVillageState(heroDao, inventoryRepo, itemRepo, content),
          Outskirts -> OutskirtsState(
            heroDao,
            inventoryRepo,
            scheduler,
            content
          ),
          TreasureHunt -> TreasureHuntState(heroDao, scheduler, content),
          Construction -> ConstructionState(heroDao, scheduler, content),
          Guild        -> GuildState(heroDao, content),
          TrophyExchange -> TrophyExchangeState(
            heroDao,
            inventoryRepo,
            content
          ),
          TrainingHall  -> TrainingHallState(content),
          MasterHorn    -> MasterHornState(heroDao, inventoryRepo, content, bank),
          MentorKazimir -> MentorKazimirState(heroDao, inventoryRepo, content)
        )
        _ <- GraphValidator.validate(states)
      } yield new StatesMap(states)
    )
}
