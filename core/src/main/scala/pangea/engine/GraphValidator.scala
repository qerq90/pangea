package pangea.engine

import pangea.model.state.StateType
import pangea.service.state.State
import zio.{IO, ZIO}

object GraphValidator {

  def validate(states: Map[StateType, State]): IO[Throwable, Unit] = {
    val broken = for {
      (from, state) <- states.toList
      to            <- state.targetStates
      if !states.contains(to)
    } yield s"  $from → $to (state '$to' not registered in StatesMap)"

    ZIO.fail(new Throwable(s"Graph validation failed:\n${broken.mkString("\n")}"))
      .unless(broken.isEmpty)
      .unit
  }
}
