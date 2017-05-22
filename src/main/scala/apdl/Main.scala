package apdl

import java.io.File

import apdl.parser.MainParsers

import scala.io.Source
import scala.language.postfixOps
import scala.util.parsing.input.CharSequenceReader

case class ApdlConfig(
                       mainFile: File = new File("."),
                       outputDirectory: File = new File("./default-apdl-output")
                     )

object Main extends App {

  val input = "example.apdl"
  val source = Source.fromFile(input).mkString
  val manager = new ApdlProjectManager(source)

  val project = manager.project

  println(project)

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

object Try extends App {
  val code =
    """|@device arduino1 {
       |    id = uno
       |    framework = arduino
       |    @input rawTemp analogInput 1
       |    @input lum analogInput 0
       |    @input temp tf rawTemp
       |    @input lumTemp simpleOperator + lum temp
       |    @serial lum each 1 s
       |    @serial temp each 1 s
       |}""".stripMargin

  val parsers = new MainParsers

  import parsers._

  parsers.parse(parsers.apdlDevice, new PackratReader[Char](new CharSequenceReader(code))) match {
    case Success(result, next) => println(result)
    case n: NoSuccess => println(s"$n")
  }
}
