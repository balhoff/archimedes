import scala.scalanative.build._

lazy val commonSettings = Seq(
  organization := "org.geneontology",
  name := "archimedes",
  version := "0.1.1",
  homepage := Some(url("https://github.com/balhoff/archimedes")),
  licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause")),
  scalaVersion := "2.13.14",
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
  pomExtra := <scm>
    <url>git@github.com:balhoff/archimedes.git</url>
    <connection>scm:git:git@github.com:balhoff/archimedes.git</connection>
  </scm>
    <developers>
      <developer>
        <id>balhoff</id>
        <name>Jim Balhoff</name>
        <email>jim@balhoff.org</email>
      </developer>
    </developers>,
  libraryDependencies ++= {
    Seq(
      "com.lihaoyi" %%% "fastparse" % "2.3.3",
      "com.lihaoyi" %%% "utest" % "0.7.11" % Test
    )
  },
  testFrameworks += new TestFramework("utest.runner.Framework")
)

lazy val archimedes = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("."))
  .settings(commonSettings)
  .jsSettings()
  .jvmSettings()
  .nativeSettings()
