import Dependencies.*

lazy val root = (project in file("."))
  .aggregate(api)

lazy val api = (project in file("app"))
  .settings(
    standartSettings,
    libraryDependencies ++= apiDependencies,
    Compile / mainClass := Some("app.Main"),
    name                := "app",
    version             := "0.1.0"
  )
  .dependsOn(core)

lazy val core = (project in file("core"))
  .settings(
    standartSettings,
    libraryDependencies ++= coreDependencies,
    name := "core"
  )

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Ywarn-unused"
)

lazy val standartSettings = Seq(
  scalaVersion := "2.13.16"
)

resolvers ++= Seq(
  Resolver.mavenLocal,
  "Maven Central" at "https://repo1.maven.org/maven2/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)
