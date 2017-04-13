package apdl.parser

import java.io.File

import scala.language.postfixOps
import scala.util.parsing.input.CharSequenceReader

object Example extends App {
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
