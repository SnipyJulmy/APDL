package apdl.parser

import java.io.File

object Test extends App {
  val src = scala.io.Source.fromFile(new File(s"src/main/resources/example.apdl")) mkString
  val parser = new ApdlParser

  import parser._

  val code = parser.parse(parser.program, src)
  code match {
    case Success(result, _) =>
      println((new ArduinoGenerator).generate(result))
    case error: NoSuccess => println(s"error... : $error")
  }
}
