name := "cls-scala"

version := "1.0"
resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.typesafeRepo("releases")

lazy val exampleProject =
  (project in file("./example"))
    .dependsOn(root)
    .enablePlugins(SbtTwirl)
    .settings(
      scalaVersion := "2.11.8",
      sourceDirectories in (Compile, TwirlKeys.compileTemplates) := (unmanagedSourceDirectories in Compile).value
    )

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.11.8",
    libraryDependencies ++= Seq(
      "de.tu_dortmund.cs.ls14" %% "shapeless-feat" % "0.1.0",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scalactic" %% "scalactic" % "2.2.6",
      "org.scalatest" %% "scalatest" % "2.2.6" % "test"
    )
  )
