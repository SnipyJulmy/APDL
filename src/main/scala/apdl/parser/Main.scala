package apdl.parser

import java.io.{File, PrintWriter}

import scala.language.postfixOps
import scala.util.parsing.input.CharSequenceReader

// TODO Args parsing + main app
object Main extends App {

  val config = ApdlConfig(args)

  val src = scala.io.Source.fromFile(new File(config.filepath)) mkString
  val parser = new ApdlParser

  import parser._

  // TODO optimize result, verification, semantic analysis

  val code = parser.parse(parser.program, new PackratReader[Char](new CharSequenceReader(src)))
  code match {
    case Success(result, _) =>
      // TODO specify generator
      val generatedCode = (new ArduinoGenerator).generate(result)
      config.outputFile match {
        case Some(value) =>
          val outputFile = new File(value)
          val pw = new PrintWriter(outputFile)
          pw.write(generatedCode)
          pw.flush()
          pw.close()
        case None =>
          println(generatedCode)
      }
    case error: NoSuccess => println(s"error... : $error")
  }

}

case class ApdlConfig(filepath: String, target: ApdlTarget, outputFile: Option[String])

object ApdlConfig {

  def specifyOption(optionName: String, nbArg: Int, args: Array[String], errorMissingMsg: String): List[String] = {
    if (!args.contains(s"-$optionName")) throw new ApdlArgsException(errorMissingMsg)
    val index = args.indexOf(s"-$optionName")
    for (i <- 1 to nbArg) {
      if (args(index + i).beginWith("-"))
        throw new ApdlArgsException(s"command arg #$i specified after -$optionName is incorrect")
    }
    args.slice(index + 1, index + nbArg + 1).toList
  }

  def specifyOptionalOption(optionName: String, nbArg: Int, args: Array[String]): Option[List[String]] = {
    if (!args.contains(s"-$optionName")) None
    else {
      val index = args.indexOf(s"-$optionName")
      for (i <- 1 to nbArg) {
        if (args(index + i).beginWith("-"))
          throw new ApdlArgsException(s"command arg #$i specified after -$optionName is incorrect")
      }
      Some(args.slice(index + 1, index + nbArg + 1).toList)
    }
  }

  def apply(args: Array[String]): ApdlConfig = {
    if (args.length == 0) throw new ApdlArgsException("Empty arg list")
    // filepath : option -f
    val filepath = specifyOption("f", 1, args, "No input file specified, use -f [filepath]").head
    val target = ApdlTarget.arg2target(specifyOption("t", 1, args, "no target specified, use -t [target]").head)
    val outputFile = specifyOptionalOption("o", 1, args) match {
      case Some(value) => Some(value.head)
      case None => None
    }
    ApdlConfig(filepath, target, outputFile)
  }
  implicit class ApdlArgs(str: String) {
    def beginWith(prefix: String): Boolean = str.reverse.endsWith(prefix.reverse)
  }
}