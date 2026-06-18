import Dependencies.*

lazy val root = (project in file("."))
  .aggregate(app)

lazy val app = (project in file("app"))
  .settings(
    standartSettings,
    libraryDependencies ++= apiDependencies,
    Compile / mainClass         := Some("app.Main"),
    assembly / mainClass        := Some("app.Main"),
    assembly / assemblyJarName  := "app.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "services", xs @ _*) => MergeStrategy.concat
      case PathList("META-INF", xs @ _*)             => MergeStrategy.discard
      case "reference.conf"                          => MergeStrategy.concat
      case "module-info.class"                       => MergeStrategy.discard
      case _                                         => MergeStrategy.first
    },
    name    := "app",
    version := "0.1.0"
  )
  .dependsOn(core)

lazy val core = (project in file("core"))
  .settings(
    standartSettings,
    libraryDependencies ++= coreDependencies ++ testDependencies,
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
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
