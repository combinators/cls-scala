import sbt.Keys._
import sbt.Resolver

lazy val commonSettings = Seq(
  organization := "de.tu_dortmund.cs.ls14",
  releaseVersionBump := sbtrelease.Version.Bump.Minor,
  releaseIgnoreUntrackedFiles := true,

  scalaVersion := "2.12.3",
  crossScalaVersions := Seq("2.11.11", "2.12.3"),
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

lazy val examples = (Project(id = "cls-scala-examples", base = file("examples")))
    .settings(commonSettings: _*)
    .settings(noPublishSettings: _*)
    .enablePlugins(SbtTwirl)
    .enablePlugins(PlayScala)
    .disablePlugins(PlayLayoutPlugin)
    .settings(
      moduleName := "cls-scala-examples",
      libraryDependencies += guice,
      PlayKeys.playMonitoredFiles ++= (sourceDirectories in (Compile, TwirlKeys.compileTemplates)).value
    ).dependsOn(core)

lazy val core = (Project(id = "cls-scala", base = file("core")))
    .settings(commonSettings: _*)
    .settings(
      moduleName := "cls-scala",

      crossScalaVersions := Seq("2.11.11", scalaVersion.value),
      libraryDependencies ++= Seq(
        "de.tu_dortmund.cs.ls14" %% "shapeless-feat" % "0.2.1",
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "org.scalactic" %% "scalactic" % "3.0.1",
        "org.scalatest" %% "scalatest" % "3.0.1" % "test"
      )
    )

lazy val presentationPlayGit = (Project(id = "cls-scala-presentation-play-git", base = file(".") / "presentation" / "play-git"))
  .settings(commonSettings: _*)
  .enablePlugins(SbtTwirl)
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin)
  .dependsOn(core)
  .settings(
    moduleName := "cls-scala-presentation-play-git",

    libraryDependencies ++= Seq(
      "org.eclipse.jgit" % "org.eclipse.jgit" % "4.8.0.201706111038-r",
      "org.webjars" %% "webjars-play" % "2.6.1",
      "org.webjars" % "bootstrap" % "3.3.7-1"
    ),
    sourceDirectories in (Compile, TwirlKeys.compileTemplates) := Seq(sourceDirectory.value / "main" / "html-templates")
  )

lazy val templatingJava = (Project(id = "cls-scala-templating-java", base = file(".") / "templating" / "java"))
  .settings(commonSettings: _*)
  .dependsOn(core)
  .enablePlugins(SbtTwirl)
  .settings(
    moduleName := "cls-scala-templating-java",

    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-text" % "1.1",
      "com.github.javaparser" % "javaparser-core" % "3.5.3"
    )
  )

lazy val templatingPython = (Project(id = "cls-scala-templating-python", base = file(".") / "templating" / "python"))
  .settings(commonSettings: _*)
  .dependsOn(core)
  .enablePlugins(SbtTwirl)
  .settings(
    moduleName := "cls-scala-templating-python",

    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-text" % "1.1"
    )
  )

lazy val root = (Project(id = "cls-scala-root", base = file(".")))
  .settings(commonSettings: _*)
  .settings(noPublishSettings: _*)
  .aggregate(core, presentationPlayGit, templatingJava, templatingPython, examples)
  .settings(
    moduleName := "cls-scala-root"
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
  homepage := Some(url("https://www.github.com/JanBessai/cls-scala")),
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scmInfo := Some(ScmInfo(url("https://github.com/JanBessai/cls-scala"), "scm:git:git@github.com:JanBessai/cls-scala.git")),
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
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

credentials in ThisBuild ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq