package pangea.service.state.states

import pangea.dao.hero.HeroDao
import pangea.model.state.StateType
import pangea.model.state.StateType.{GlobalMap, Registration}
import pangea.service.sender.Sender
import pangea.service.state.State
import pangea.service.state.states.Registration.RegistrationState
import zio.{ZIO, ZLayer}

case class StatesMap(states: Map[StateType, State])

object StatesMap {
  val live: ZLayer[Sender with HeroDao, Nothing, StatesMap] = ZLayer.fromZIO(
    for {
      sender  <- ZIO.service[Sender]
      heroDao <- ZIO.service[HeroDao]
      states = Map[StateType, State](
        GlobalMap    -> GlobalMapState(),
        Registration -> RegistrationState(sender, heroDao)
      )
    } yield new StatesMap(states)
  )
}
