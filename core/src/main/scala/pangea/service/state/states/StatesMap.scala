package pangea.service.state.states

import pangea.dao.hero.HeroDao
import pangea.model.state.StateType
import pangea.model.state.StateType.{GlobalMap, Registration}
import pangea.service.sender.Api
import pangea.service.state.State
import pangea.service.state.states.Registration.RegistrationState
import zio.{ZIO, ZLayer}

case class StatesMap(states: Map[StateType, State])

object StatesMap {
  val live: ZLayer[Api with HeroDao, Nothing, StatesMap] = ZLayer.fromZIO(
    for {
      sender  <- ZIO.service[Api]
      heroDao <- ZIO.service[HeroDao]
      states = Map[StateType, State](
        GlobalMap    -> GlobalMapState(),
        Registration -> RegistrationState(sender, heroDao)
      )
    } yield new StatesMap(states)
  )
}
