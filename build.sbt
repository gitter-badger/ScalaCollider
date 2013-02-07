name := "ScalaCollider"

version := "1.3.1"

organization := "de.sciss"

scalaVersion := "2.10.0"

description := "A sound synthesis library for the SuperCollider server"

homepage := Some( url( "https://github.com/Sciss/ScalaCollider" ))

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

resolvers += "Sonatype OSS Releases" at "https://oss.sonatype.org/content/groups/public"

// libraryDependencies ++= Seq(
//    "de.sciss" %% "scalaosc" % "1.1.+",
//    "de.sciss" %% "scalaaudiofile" % "1.2.+",
//    ("org.scalatest" % "scalatest" % "1.8" cross CrossVersion.full) % "test"
// )
//
// libraryDependencies <++= scalaVersion { sv =>
//    sv match {
//       case "2.9.2" => Seq.empty
//       case _ => Seq( "org.scala-lang" % "scala-actors" % sv )
//    }
// }

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked" )

// ---- build info ----

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq( name, organization, version, scalaVersion, description,
   BuildInfoKey.map( homepage ) { case (k, opt) => k -> opt.get },
   BuildInfoKey.map( licenses ) { case (_, Seq( (lic, _) )) => "license" -> lic }
)

buildInfoPackage := "de.sciss.synth"

// ---- publishing ----

publishMavenStyle := true

publishTo <<= version { (v: String) =>
   Some( if( v.endsWith( "-SNAPSHOT" ))
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
   else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
   )
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra :=
<scm>
  <url>git@github.com:Sciss/ScalaCollider.git</url>
  <connection>scm:git:git@github.com:Sciss/ScalaCollider.git</connection>
</scm>
<developers>
   <developer>
      <id>sciss</id>
      <name>Hanns Holger Rutz</name>
      <url>http://www.sciss.de</url>
   </developer>
</developers>

// ---- disable scaladoc generation during development phase ----

// publishArtifact in (Compile, packageDoc) := false

// ---- ls.implicit.ly ----

seq( lsSettings :_* )

(LsKeys.tags in LsKeys.lsync) := Seq( "sound-synthesis", "sound", "music", "supercollider" )

(LsKeys.ghUser in LsKeys.lsync) := Some( "Sciss" )

(LsKeys.ghRepo in LsKeys.lsync) := Some( "ScalaCollider" )

// bug in ls -- doesn't find the licenses from global scope
(licenses in LsKeys.lsync) := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))
