package apdl.parser

import java.io.File

import scala.language.postfixOps
import scala.util.parsing.input.CharSequenceReader

// TODO Args parsing + main app
object Main extends App {
  println(ApdlConfig(args))
  val src = scala.io.Source.fromFile(new File(s"src/main/resources/example.apdl")) mkString
  val parser = new ApdlParser

  import parser._

  val code = parser.parse(parser.program, new PackratReader[Char](new CharSequenceReader(src)))
  code match {
    case Success(result, _) =>
      // TODO optimize result, verification, semantic analysis
      println((new ArduinoGenerator).generate(result))
    case error: NoSuccess => println(s"error... : $error")
  }

}

case class ApdlConfig(filepath: String, target: ApdlTarget)

object ApdlConfig {

  def specifyOption(optionName: String, nbArg: Int, args: Array[String], errorMsg: String): List[String] = {
    if (!args.contains(s"-$optionName")) throw new ApdlArgsException(errorMsg)
    val index = args.indexOf(s"-$optionName")
    for (i <- 1 to nbArg) {
      if (args(index + i).beginWith("-"))
        throw new ApdlArgsException(s"command arg #$i specified after -$optionName is incorrect")
    }
    args.slice(index + 1, index + nbArg + 1).toList
  }

  def apply(args: Array[String]): ApdlConfig = {
    if (args.length == 0) throw new ApdlArgsException("Empty arg list")
    // filepath : option -f
    val filepath = specifyOption("f", 1, args, "No input file specified, use -f [filepath]").head
    val target = ApdlTarget.arg2target(specifyOption("t", 1, args, "no target specified, use -t [target]").head)
    ApdlConfig(filepath, target)
  }
  implicit class ApdlArgs(str: String) {
    def beginWith(prefix: String): Boolean = str.reverse.endsWith(prefix.reverse)
  }
}