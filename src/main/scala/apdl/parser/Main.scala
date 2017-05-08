package apdl.parser

import java.io.File

import apdl.parser.ApdlGenerationType.ApdlGenerationType
import com.github.SnipyJulmy.scalacolor.ScalaColor._

import scala.language.postfixOps

case class ApdlConfig(mainFile: File = new File("."),
                      outputDirectory: File = new File("./default-apdl-output"),
                      generationType: ApdlGenerationType = ApdlGenerationType.default)

object Main extends App {

  val apdlParser = new MainParser(parse(args))
  val project = apdlParser.parseFile()

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
}

