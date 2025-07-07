package pangea.dao.config

import pureconfig.ConfigSource
import pureconfig.generic.auto._
import zio.{ZIO, ZLayer}

case class PostgresConfig(
  jdbcUrl: String,
  username: String,
  password: String
)

object PostgresConfig {
  private val loadConfig =
    ZIO.attempt(
      ConfigSource.default.at("postgres").loadOrThrow[PostgresConfig]
    )

  val live: ZLayer[Any, Throwable, PostgresConfig] =
    ZLayer.fromZIO(loadConfig)
}
