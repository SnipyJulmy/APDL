package apdl.parser

import java.io.File

import apdl.parser.ApdlTarget.ApdlTarget
import com.github.SnipyJulmy.scalacolor.ScalaColor._

import scala.io.Source
import scala.language.postfixOps
import scala.util.Try
import scala.util.parsing.input.CharSequenceReader

case class ApdlConfig(file: File = new File("."),
                      target: ApdlTarget = ApdlTarget.default,
                      outputDirectory: File = new File("./default-apdl-output"))

object Main extends App {

  val config = parse(args)
  val parser = new ApdlParser

  import parser._

  // TODO optimize result, verification, semantic analysis

  val codeStr: String = {
    Source.fromFile(config.file).mkString
  }
  val code = parser.parse(parser.program, new PackratReader[Char](new CharSequenceReader(codeStr)))
  code match {
    case Success(result, _) =>
      // TODO specify generator
      val backend = new ArduinoGenerator
      Try {
        backend.generate(config.outputDirectory, result)
      } match {
        case util.Failure(exception) => exception match {
          case e: ApdlBackendException =>
            println(s"Generation failed : ${e.getMessage}".red)
            System.exit(1)
          case _ =>
            println(exception)
        }
        case util.Success(_) =>
          println("Compilation end successfully".blue)
      }
    case error: NoSuccess =>
      println(s"Parse error : $error".red)
  }

  def parse(args: Array[String]): ApdlConfig = {
    implicit val apdlTargetRead: scopt.Read[ApdlTarget.Value] =
      scopt.Read.reads(ApdlTarget withName)

    val argsParser = new scopt.OptionParser[ApdlConfig]("apdl") {
      // Metadata
      head("apdl", "1.0")

      // Argument
      arg[File]("<file>")
        .action((f, c) => c.copy(file = f))

      // Optional args
      opt[ApdlTarget]('t', "target")
        .action((t, c) => c.copy(target = t))
        .text("targeting framework")

      opt[File]('d', "dir")
        .action((o, c) => c.copy(outputDirectory = o))
        .text("Output directory")
    }

    argsParser.parse(args, ApdlConfig()) match {
      case Some(value) => value
      case None =>
        System.exit(0)
        throw new Exception("Unreachable code")
    }
  }
}

