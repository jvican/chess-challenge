name := "Chess"

organization := "jvican"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.scalaz" %% "scalaz-concurrent" % "7.1.5"
)

mainClass in (Compile, run) := Some("chess.async.ChessApp")

javaOptions += "-Xmx2G"
