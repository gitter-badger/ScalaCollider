name := "ScalaCollider"

version := "1.8.0-SNAPSHOT"

organization := "de.sciss"

scalaVersion := "2.10.0"  // _not 2.10.1 or 2.10.2_ because of SI-7268 / SI-7436 ; "2.10.3-RC1"

description := "A sound synthesis library for the SuperCollider server"

homepage <<= name { n => Some(url("https://github.com/Sciss/" + n)) }

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

libraryDependencies <<= version { v =>
  //  val i  = v.lastIndexOf('.') + 1
  //  val uv = v.substring(0, i) + "+"
  val uv = "1.6.+"
  Seq(
    "de.sciss" %% "scalaosc"                % "1.1.+",
    "de.sciss" %% "scalaaudiofile"          % "1.4.+",
    "de.sciss" %% "processor"               % "0.2.+",
    "de.sciss" %% "scalacolliderugens-core" % uv,
    "org.scalatest" %% "scalatest" % "1.9.1" % "test"
  )
}

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

scalacOptions ++= Seq("-Xelide-below", "INFO")     // elide debug logging!

// ---- console ----

initialCommands in console :=
"""import de.sciss.osc
  |import de.sciss.synth._
  |import ugen._
  |import Predef.{any2stringadd => _}
  |import Ops._
  |def s = Server.default
  |def boot() { Server.run(_ => ()) }
""".stripMargin

// ---- build info ----

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
  BuildInfoKey.map(homepage ) { case (k, opt) => k -> opt.get },
  BuildInfoKey.map(licenses ) { case (_, Seq( (lic, _) )) => "license" -> lic }
)

buildInfoPackage := "de.sciss.synth"

// ---- publishing ----

publishMavenStyle := true

publishTo <<= version { (v: String) =>
   Some(if (v.endsWith("-SNAPSHOT"))
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
   else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
   )
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra <<= name { n =>
<scm>
  <url>git@github.com:Sciss/{n}.git</url>
  <connection>scm:git:git@github.com:Sciss/{n}.git</connection>
</scm>
<developers>
   <developer>
      <id>sciss</id>
      <name>Hanns Holger Rutz</name>
      <url>http://www.sciss.de</url>
   </developer>
</developers>
}

// ---- disable scaladoc generation during development phase ----

// publishArtifact in (Compile, packageDoc) := false

// ---- ls.implicit.ly ----

seq(lsSettings :_*)

(LsKeys.tags in LsKeys.lsync) := Seq("sound-synthesis", "sound", "music", "supercollider")

(LsKeys.ghUser in LsKeys.lsync) := Some("Sciss")

(LsKeys.ghRepo in LsKeys.lsync) <<= name(Some(_))
