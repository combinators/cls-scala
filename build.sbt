import sbt.Keys._
import sbt.Resolver

lazy val commonSettings = Seq(
  organization := "org.combinators",

  scalaVersion := "2.13.3",
  crossScalaVersions := Seq("2.11.12", "2.12.12", scalaVersion.value),

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.typesafeRepo("snapshots")
  ),

  headerLicense := Some(HeaderLicense.ALv2("2018-2020", "Jan Bessai")),

  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions"
  ),
  scapegoatVersion in ThisBuild := "1.4.6",
  concurrentRestrictions in Global ++= {
    if (sys.env.get("CI").isDefined) Seq(Tags.limitAll(1)) else Seq.empty
  }
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
        "org.scalactic" %% "scalactic" % "3.2.0" % "test",
        "org.scalatest" %% "scalatest" % "3.2.2" % "test",
        "ch.qos.logback" % "logback-classic" % "1.2.3",
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
      ),
      libraryDependencies ++= {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, n)) if n >= 13 =>
            Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.0")
          case _ => Seq()
        }
      },

      unmanagedSourceDirectories in Compile += {
        val sourceDir = (sourceDirectory in Compile).value
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, n)) if n >= 13 => sourceDir / "scala-2.13+"
          case _                       => sourceDir / "scala-2.12-"
        }
      },
      unmanagedSourceDirectories in Test += {
        val sourceDir = (sourceDirectory in Test).value
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
