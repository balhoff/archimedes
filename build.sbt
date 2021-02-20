
//name := "archimedes"

//version := "0.1-SNAPSHOT"

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

homepage := Some(url("https://github.com/balhoff/archimedes"))

lazy val commonSettings = Seq(
  scalaVersion := "2.13.4",
  scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-Ypatmat-exhaust-depth", "off"),
  scalacOptions in Test ++= Seq("-Yrangepos"),
  publishArtifact in Test := false
)

lazy val root = project.in(file("."))
  .settings(commonSettings)
  .enablePlugins(ScalaJSPlugin)
  .aggregate(archimedesJS, archimedesJVM)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val archimedes = crossProject(JSPlatform, JVMPlatform).in(file("."))
  .settings(commonSettings)
  .settings(
    organization := "org.geneontology",
    name := "archimedes",
    version := "0.1-SNAPSHOT",
    libraryDependencies ++= {
      Seq(
        "com.lihaoyi" %%% "fastparse" % "2.3.1",
        "com.lihaoyi" %%% "utest"     % "0.7.7" % Test
      )
    }
  )
  .jvmSettings(
    // Add JVM-specific settings here
  )
  .jsSettings(
    // Add JS-specific settings here
  )

lazy val archimedesJVM = archimedes.jvm
lazy val archimedesJS = archimedes.js

testFrameworks += new TestFramework("utest.runner.Framework")

//fork in Test := true

pomExtra := <scm>
  <url>git@github.com:balhoff/archimedes.git</url>
  <connection>scm:git:git@github.com:balhoff/archimedes.git</connection>
</scm>
  <developers>
    <developer>
      <id>balhoff</id>
      <name>Jim Balhoff</name>
      <email>balhoff@renci.org</email>
    </developer>
  </developers>
