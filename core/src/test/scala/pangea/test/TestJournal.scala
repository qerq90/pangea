package pangea.test

import pangea.engine.Journal
import pangea.model.GameEvent
import zio.{Ref, Task}

class TestJournal(ref: Ref[List[GameEvent]]) extends Journal {
  override def append(event: GameEvent): Task[Unit] = ref.update(_ :+ event)
  def logged: Task[List[GameEvent]]                  = ref.get
}

object TestJournal {
  def make: Task[TestJournal] = Ref.make(List.empty[GameEvent]).map(new TestJournal(_))
}
