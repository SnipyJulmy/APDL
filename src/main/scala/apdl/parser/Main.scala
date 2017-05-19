package apdl.parser

import java.io.File

import com.github.SnipyJulmy.scalacolor.ScalaColor._

import scala.io.Source
import scala.language.postfixOps
import scala.util.parsing.input.CharSequenceReader

case class ApdlConfig(
                       mainFile: File = new File("."),
                       outputDirectory: File = new File("./default-apdl-output")
                     )

object Main extends App {

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

object DefineTry extends App {
  val file = "./src/main/resources/apdl_component.apdl"
  val componentFile = new File(file)
  val source = Source.fromFile(componentFile).mkString
  val parser = new DefineParsers

  val code = " def fn (ks64i:float[]) -> short {(wxb <= true)sgsnt2xiUXu(true + -3.112279961714391E306)}"

  import parser._

  parser.parse(parser.tfFunctionDeclaration, new PackratReader[Char](new CharSequenceReader(code))) match {
    case Success(result, next) =>
      println(next.atEnd)
      println(result)
    case n: NoSuccess => println(s"error : $n".red)
  }
}