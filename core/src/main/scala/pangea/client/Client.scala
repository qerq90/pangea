package pangea.client

import fs2.io.net.Network
import org.http4s.client.{Client => HClient}
import org.http4s.ember.client.EmberClientBuilder
import zio.{Task, ZIO}
import zio.interop.catz._

object Client {
  // Явный Network[Task] вместо устаревшего неявного implicitForAsync (http4s 3.7.0).
  private implicit val network: Network[Task] = Network.forAsync[Task]

  def make: Task[HClient[Task]] =
    ZIO.scoped {
      EmberClientBuilder
        .default[Task]
        .build
        .toScopedZIO
    }
}
