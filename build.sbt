import sbt.Keys._
import sbt.Resolver

lazy val commonSettings = Seq(
  organization := "org.combinators",
  releaseVersionBump := sbtrelease.Version.Bump.Minor,
  releaseIgnoreUntrackedFiles := true,

  scalaVersion := "2.12.3",
  crossScalaVersions := Seq("2.11.11", scalaVersion.value),
  releaseCrossBuild := true,

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

      crossScalaVersions := Seq("2.11.11", scalaVersion.value),
      libraryDependencies ++= Seq(
        "de.tu_dortmund.cs.ls14" %% "shapeless-feat" % "0.2.1",
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "org.scalactic" %% "scalactic" % "3.0.4" % "test",
        "org.scalatest" %% "scalatest" % "3.0.4" % "test"
      )
    )


lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishTo := { version { (v: String) =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }.value },
  homepage := Some(url("https://combinators.org")),
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scmInfo := Some(ScmInfo(url("https://www.github.com/combinators/cls-scala"), "scm:git:git@github.com:combinators/cls-scala.git")),
  pomExtra := (
    <developers>
      <developer>
        <id>JanBessai</id>
        <name>Jan Bessai</name>
        <url>http://janbessai.github.io/</url>
      </developer>
      <developer>
        <id>BorisDuedder</id>
        <name>Boris Düdder</name>
        <url>http://duedder.net/</url>
      </developer>
      <developer>
        <id>heineman</id>
        <name>George T. Heineman</name>
        <url>http://www.cs.wpi.edu/~heineman</url>
      </developer>
    </developers>
    )
)

lazy val noPublishSettings = Seq(
  publish := Seq.empty,
  publishLocal := Seq.empty,
  publishArtifact := false
)

credentials in ThisBuild ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
