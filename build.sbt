name := "twirltest"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += Resolver.sonatypeRepo("releases")

lazy val root = (project in file("."))
  .enablePlugins(SbtTwirl)
  .settings(
    libraryDependencies ++= Seq(
      "de.tu_dortmund.cs.ls14" %% "shapeless-feat" % "0.1.0",
      "org.scalactic" %% "scalactic" % "2.2.6",
      "org.scalatest" %% "scalatest" % "2.2.6" % "test"
    ),
    sourceDirectories in (Compile, TwirlKeys.compileTemplates) := (unmanagedSourceDirectories in Compile).value
  )