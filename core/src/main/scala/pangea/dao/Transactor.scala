package pangea.dao

import cats.implicits.catsSyntaxOptionId
import doobie.hikari.{Config, HikariTransactor}
import doobie.util.transactor.Transactor
import pangea.dao.config.PostgresConfig
import zio.{Task, ZIO, ZLayer}
import zio.interop.catz._

object Transactor {
  val live: ZLayer[PostgresConfig, Throwable, Transactor[Task]] =
    ZLayer.scoped {
      for {
        config <- ZIO.service[PostgresConfig]

        daoconfig = Config(
          jdbcUrl = config.jdbcUrl,
          username = config.username.some,
          password = config.password.some
        )

        xa <- HikariTransactor.fromConfig[Task](daoconfig).toScopedZIO
      } yield xa
    }
}
