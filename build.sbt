import sbt.Keys._
import sbt.Resolver

lazy val commonSettings = Seq(
  version := "1.0",
  organization := "de.tu_dortmund.cs.ls14",

  scalaVersion := "2.11.8",

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeRepo("releases")
  ),

  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions"
  )
)

lazy val examples = (Project(id = "cls-scala-examples", base = file("./examples")))
    .settings(commonSettings: _*)
    .enablePlugins(SbtTwirl)
    .settings(
      moduleName := "cls-scala-examples",

      sourceDirectories in (Compile, TwirlKeys.compileTemplates) := (unmanagedSourceDirectories in Compile).value
    ).dependsOn(core)

lazy val core = (Project(id = "cls-scala", base = file("core")))
    .settings(commonSettings: _*)
    .settings(
      moduleName := "cls-scala",

      libraryDependencies ++= Seq(
        "de.tu_dortmund.cs.ls14" %% "shapeless-feat" % "0.1.0",
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "org.scalactic" %% "scalactic" % "2.2.6",
        "org.scalatest" %% "scalatest" % "2.2.6" % "test"
      )
    )

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  aggregate(core, examples).
  settings(
    moduleName := "cls-scala-root"
  )
