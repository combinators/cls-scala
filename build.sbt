import sbt.Keys._
import sbt.Resolver

lazy val commonSettings = Seq(
  organization := "org.combinators",

  scalaVersion := "2.13.6",
  crossScalaVersions := Seq("2.11.12", "2.12.15", scalaVersion.value),

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.typesafeRepo("snapshots")
  ),

  headerLicense := Some(HeaderLicense.ALv2("2018-2021", "Jan Bessai")),

  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions"
  ),
  ThisBuild / scapegoatVersion := "1.4.10"
) ++ publishSettings

lazy val examples = (Project(id = "examples", base = file("examples")))
    .settings(commonSettings: _*)
    .settings(noPublishSettings: _*)
    .settings(
      moduleName := "examples"
    ).dependsOn(root)

lazy val root = (Project(id = "cls-scala", base = file(".")))
    .settings(commonSettings: _*)
    .settings(
      moduleName := "cls-scala",
      libraryDependencies ++= Seq(
        "org.combinators" %% "shapeless-feat" % "0.2.5",
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "org.scalactic" %% "scalactic" % "3.2.10" % "test",
        "org.scalatest" %% "scalatest" % "3.2.10" % "test",
        "ch.qos.logback" % "logback-classic" % "1.2.6",
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4"
      ),
      libraryDependencies ++= {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, n)) if n >= 13 =>
            Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4")
          case _ => Seq()
        }
      },

      Compile/unmanagedSourceDirectories += {
        val sourceDir = (Compile/sourceDirectory).value
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, n)) if n >= 13 => sourceDir / "scala-2.13+"
          case _                       => sourceDir / "scala-2.12-"
        }
      },
      Test/unmanagedSourceDirectories += {
        val sourceDir = (Test/sourceDirectory).value
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, n)) if n >= 13 => sourceDir / "scala-2.13+"
          case _                       => sourceDir / "scala-2.12-"
        }
      }
    )

lazy val publishSettings = Seq(
  homepage := Some(url("https://combinators.org")),
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scmInfo := Some(ScmInfo(url("https://www.github.com/combinators/cls-scala"), "scm:git:git@github.com:combinators/cls-scala.git")),
  developers := List(
    Developer("JanBessai", "Jan Bessai", "jan.bessai@tu-dortmund.de", url("http://janbessai.github.io")),
    Developer("heineman", "George T. Heineman", "heineman@wpi.edu", url("http://www.cs.wpi.edu/~heineman")),
    Developer("BorisDuedder", "Boris Düdder", "boris.d@di.ku.dk", url("http://duedder.net"))
  ),
  releaseEarlyWith := SonatypePublisher
)

lazy val noPublishSettings = Seq(
  publish := Seq.empty,
  publishLocal := Seq.empty,
  publishArtifact := false
)
