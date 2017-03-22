name := "apdl"

version := "0.1"

scalaVersion := "2.11.2"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.scala-lang.lms" %% "lms-core" % "1.1.0-SNAPSHOT"
libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.11.2"
libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % "2.11.2"
libraryDependencies += "org.scala-lang.virtualized" % "scala-reflect" % "2.11.2"

scalaOrganization := "org.scala-lang.virtualized"

scalacOptions += "-Yvirtualize"

scalacOptions += "-deprecation"