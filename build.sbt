import sbtghactions.JavaSpec

ThisBuild / scalaVersion                        := "3.4.1"
ThisBuild / version                             := scala.sys.process.Process("git rev-parse HEAD").!!.trim.slice(0, 7)
ThisBuild / organization                        := "io.github.kirill5k"
ThisBuild / githubWorkflowPublishTargetBranches := Nil
ThisBuild / githubWorkflowJavaVersions          := Seq(JavaSpec.temurin("22"))
ThisBuild / scalacOptions ++= Seq("-Wunused:all")

val noPublish = Seq(
  publish         := {},
  publishLocal    := {},
  publishArtifact := false,
  publish / skip  := true
)

val core = project
  .in(file("modules/core"))
  .settings(
    name                 := "stocks-checker-core",
    moduleName           := "stocks-checker-core",
    libraryDependencies ++= Dependencies.core ++ Dependencies.test
  )

val root = project
  .in(file("."))
  .settings(noPublish)
  .settings(
    name := "stocks-checker"
  )
  .aggregate(core)