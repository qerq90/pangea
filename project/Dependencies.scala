import sbt._

object Dependencies {
  private val http4s =
    List(
      "http4s-ember-server",
      "http4s-dsl",
      "http4s-circe",
      "http4s-ember-client"
    )
      .map("org.http4s" %% _ % "0.23.16")

  private val ip4s = List("com.comcast" %% "ip4s-core" % "3.7.0")

  private val pureconfig =
    List(
      "pureconfig",
      "pureconfig-ip4s",
      "pureconfig-http4s"
    )
      .map("com.github.pureconfig" %% _ % "0.17.7")

  private val zio = List(
    "dev.zio" %% "zio"              % "2.0.22",
    "dev.zio" %% "zio-interop-cats" % "23.1.0.2"
  )

  private val doobie = List(
    "doobie-core",
    "doobie-hikari",
    "doobie-postgres",
    "doobie-postgres-circe"
  ).map("org.tpolecat" %% _ % "1.0.0-RC8")

  private val circe = List(
    "circe-core",
    "circe-generic",
    "circe-parser"
  ).map("io.circe" %% _ % "0.14.13")

  private val enumeratum = List(
    "com.beachape" %% "enumeratum"        % "1.7.6",
    "com.beachape" %% "enumeratum-doobie" % "1.7.8"
  )

  private val telegram = List(
    "com.bot4s"      %% "telegram-core" % "5.8.4",
    "org.augustjune" %% "canoe"         % "0.6.0"
  )

  private val sttp = List(
    "com.softwaremill.sttp.client3" %% "core" % "3.11.0",
    "com.softwaremill.sttp.client3" %% "zio"  % "3.11.0",
    "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % "3.11.0"
  )

  private val logging = List(
    "org.slf4j"      % "slf4j-api"         % "2.0.17",
    "ch.qos.logback" % "logback-classic"   % "1.5.18",
    "dev.zio"       %% "zio-logging"       % "2.5.0",
    "dev.zio"       %% "zio-logging-slf4j" % "2.5.0"
  )

  val apiDependencies: List[ModuleID] = pureconfig ++ ip4s ++ logging
  val coreDependencies: List[ModuleID] =
    zio ++ http4s ++ doobie ++ circe ++ pureconfig ++ enumeratum ++ telegram ++ sttp ++ logging
}
