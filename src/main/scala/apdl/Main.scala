package apdl

import apdl.ApdlUtils._
import apdl.generation.{ApdlProjectManager, ProjectGenerator}

import scala.io.Source

object Main extends App {

  implicit val config = (new ApdlArgsParser).parse(args)

  val input = config.mainFile
  val source = Source.fromFile(input).mkString
  val manager = new ApdlProjectManager(source)
  val generator = new ProjectGenerator(manager.project)
  generator.mkProject()
  debug("End")
}
