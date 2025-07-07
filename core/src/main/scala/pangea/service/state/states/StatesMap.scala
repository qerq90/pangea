package pangea.service.state.states

import pangea.model.state.StateType
import pangea.model.state.StateType.GlobalMap
import pangea.service.state.State
import zio.{ULayer, ZLayer}

case class StatesMap(states: Map[StateType, State])

object StatesMap {
  val live: ULayer[StatesMap] = ZLayer.succeed(
    StatesMap(Map[StateType, State](GlobalMap -> GlobalMapState()))
  )
}
