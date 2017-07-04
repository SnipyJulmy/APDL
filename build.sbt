name := "apdl"

version := "1.0"

scalaVersion := "2.12.1"

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_2.12" % "1.0.6"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0"
libraryDependencies += "io.spray" %% "spray-json" % "1.3.3"
libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"

// libraryDependencies += "com.github.SnipyJulmy" %% "scalacolor" % "1.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

coverageEnabled in(Test, compile) := true
coverageEnabled in(Compile, compile) := false

scalacOptions ++= Seq(
  "-language:postfixOps",
  "-language:implicitConversions",
  "-deprecation"
)

test in assembly := {}