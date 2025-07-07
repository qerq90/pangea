package pangea.client

import org.http4s.client.{Client => HClient}
import org.http4s.ember.client.EmberClientBuilder
import zio.{Task, ZIO}
import zio.interop.catz._

object Client {
  def make: Task[HClient[Task]] =
    ZIO.scoped {
      EmberClientBuilder
        .default[Task]
        .build
        .toScopedZIO
    }
}
