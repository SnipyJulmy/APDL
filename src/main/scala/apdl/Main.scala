package apdl

import java.io.File

import scala.io.Source

case class ApdlConfig(
                       mainFile: File = new File("."),
                       outputDirectory: File = new File("./default-apdl-output"),
                       overrideExistingProject: Boolean = true,
                       debug: Boolean = true
                     )

object Main extends App {

  implicit val config = ApdlConfig()
  implicit val debug : Boolean = config.debug

  val input = "example.apdl"
  val source = Source.fromFile(input).mkString
  val manager = new ApdlProjectManager(source)
  val generator = new ProjectGenerator(manager.project)
  generator.mkProject()

  /*

  def parse(args: Array[String]): ApdlConfig = {

    val argsParser = new scopt.OptionParser[ApdlConfig]("apdl") {
      // Metadata
      head("apdl", "1.0")

      // Argument
      arg[File]("<file>")
        .action((f, c) => c.copy(mainFile = f))

      opt[File]('d', "dir")
        .action((o, c) => c.copy(outputDirectory = o))
        .text("Output directory")
    }

    argsParser.parse(args, ApdlConfig()) match {
      case Some(value) => value
      case None =>
        println("Unable to parse args".red)
        System.exit(0)
        throw new Exception("Unreachable code")
    }
  }
  */
}
