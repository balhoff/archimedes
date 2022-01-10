import scala.scalanative.build._

lazy val commonSettings = Seq(
  organization := "org.geneontology",
  name := "archimedes",
  version := "0.1",
  homepage := Some(url("https://github.com/balhoff/archimedes")),
  licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause")),
  scalaVersion := "2.13.7",
  scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-Ypatmat-exhaust-depth", "off"),
  publishArtifact in Test := false,
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  libraryDependencies ++= {
    Seq(
      "com.lihaoyi" %%% "fastparse" % "2.3.1",
      "com.lihaoyi" %%% "utest" % "0.7.7" % Test
    )
  },
  testFrameworks += new TestFramework("utest.runner.Framework")
)

lazy val archimedes = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("."))
  .settings(commonSettings)
  .jsSettings()
  .jvmSettings()
  .nativeSettings()
