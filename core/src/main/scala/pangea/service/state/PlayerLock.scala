package pangea.service.state

import pangea.model.user.VkId
import zio.{Ref, Semaphore, Task, UIO}

class PlayerLock(ref: Ref[Map[VkId, Semaphore]]) {
  def withLock[A](id: VkId)(action: Task[A]): Task[A] =
    getSemaphore(id).flatMap(_.withPermit(action))

  private def getSemaphore(id: VkId): UIO[Semaphore] =
    Semaphore.make(1).flatMap { fresh =>
      ref.modify { map =>
        map.get(id) match {
          case Some(existing) => (existing, map)
          case None           => (fresh, map.updated(id, fresh))
        }
      }
    }
}

object PlayerLock {
  def make: UIO[PlayerLock] =
    Ref.make(Map.empty[VkId, Semaphore]).map(new PlayerLock(_))
}
