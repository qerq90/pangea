package pangea.engine

import pangea.model.GameEvent
import zio.Task

trait Journal {
  def append(event: GameEvent): Task[Unit]
}
