name := "apdl"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"
libraryDependencies += "net.liftweb" % "lift-json_2.10" % "2.6.3"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

coverageEnabled := true

scalacOptions ++= Seq(
  "-language:postfixOps"
)