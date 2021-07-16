scalaVersion := "2.11.2"

name := "program-analysis"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
)

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"
