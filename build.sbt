scalaVersion := "2.12.14"

name := "program-analysis"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
