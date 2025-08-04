package pangea.service.state.states

import pangea.dao.hero.HeroDao
import pangea.model.state.StateType
import pangea.model.state.StateType.{
  Dungeon,
  FoundItem,
  GlobalMap,
  Registration
}
import pangea.repository.event.EventRepository
import pangea.service.sender.Api
import pangea.service.state.State
import pangea.service.state.states.dungeon.DungeonState
import pangea.service.state.states.events.item.FoundItemState
import pangea.service.state.states.registration.RegistrationState
import zio.{ZIO, ZLayer}

case class StatesMap(states: Map[StateType, State])

object StatesMap {
  val live: ZLayer[Api with HeroDao with EventRepository, Nothing, StatesMap] =
    ZLayer.fromZIO(
      for {
        api       <- ZIO.service[Api]
        heroDao   <- ZIO.service[HeroDao]
        eventRepo <- ZIO.service[EventRepository]
        states = Map[StateType, State](
          GlobalMap    -> GlobalMapState(),
          Registration -> RegistrationState(api, heroDao),
          Dungeon      -> DungeonState(api),
          FoundItem    -> FoundItemState(api, eventRepo, heroDao)
        )
      } yield new StatesMap(states)
    )
}
