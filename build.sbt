scalaVersion := "2.10.4"

name := "program-analysis"

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "provided")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"
