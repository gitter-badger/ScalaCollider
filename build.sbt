name := "scalacollider"

version := "0.33"

organization := "de.sciss"

scalaVersion := "2.10.0-M2"

description := "A sound synthesis library for the SuperCollider server"

homepage := Some( url( "https://github.com/Sciss/ScalaCollider" ))

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

libraryDependencies ++= Seq(
   "de.sciss" %% "scalaosc" % "0.33",
   "de.sciss" %% "scalaaudiofile" % "0.20"
)

retrieveManaged := true

scalacOptions ++= Seq( /* "-Yvirtpatmat", */ "-deprecation", "-unchecked" )

// ---- console ----

initialCommands in console := """import de.sciss.osc; import de.sciss.synth.{ osc => sosc, _ }; import ugen._; var s: Server = null; def boot = Server.run( s = _ )"""

