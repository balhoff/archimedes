enablePlugins(JavaAppPackaging)

organization  := "org.geneontology"

name          := "archimedes"

version       := "0.1-SNAPSHOT"

publishMavenStyle := true

publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
    else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

homepage := Some(url("https://github.com/balhoff/archimedes"))

scalaVersion  := "2.12.7"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

testFrameworks += new TestFramework("utest.runner.Framework")

fork in Test := true

libraryDependencies ++= {
  Seq(
    "com.lihaoyi"                 %% "utest"                  % "0.6.3"     % Test
  )
}

pomExtra := (
    <scm>
        <url>git@github.com:balhoff/archimedes.git</url>
        <connection>scm:git:git@github.com:balhoff/archimedes.git</connection>
    </scm>
    <developers>
        <developer>
            <id>balhoff</id>
            <name>Jim Balhoff</name>
            <email>jim@balhoff.org</email>
        </developer>
    </developers>
)
