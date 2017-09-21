import sbt.Keys._
import sbt.Resolver

lazy val commonSettings = Seq(
  version := "1.3.1-SNAPSHOT",
  organization := "de.tu_dortmund.cs.ls14",

  scalaVersion := "2.11.11",

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.typesafeRepo("snapshots")
  ),

  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions"
  )
)

lazy val examples = (Project(id = "cls-scala-examples", base = file("examples")))
    .settings(commonSettings: _*)
    .enablePlugins(SbtTwirl)
    .enablePlugins(PlayScala)
    .disablePlugins(PlayLayoutPlugin)
    .settings(
      moduleName := "cls-scala-examples",

      PlayKeys.playMonitoredFiles ++= (sourceDirectories in (Compile, TwirlKeys.compileTemplates)).value
    ).dependsOn(core)

lazy val core = (Project(id = "cls-scala", base = file("core")))
    .settings(commonSettings: _*)
    .settings(
      moduleName := "cls-scala",

      crossScalaVersions := Seq("2.11.11", "2.12.2"),
      libraryDependencies ++= Seq(
        "de.tu_dortmund.cs.ls14" %% "shapeless-feat" % "0.2.1",
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "org.scalactic" %% "scalactic" % "3.0.1",
        "org.scalatest" %% "scalatest" % "3.0.1" % "test"
      )
    )

lazy val root = (Project(id = "cls-scala-root", base = file(".")))
  .settings(commonSettings: _*)
  .aggregate(core, examples)
  .settings(
    moduleName := "cls-scala-root"
  )
