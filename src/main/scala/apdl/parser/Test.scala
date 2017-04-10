package apdl.parser

import java.io.File

import scala.io.Source

object Test extends App {
  val src = Source.fromFile(new File(s"src/main/resources/example.apdl")) mkString
  val parser = new ApdlParser

  import parser._

  val code = parser.parse(parser.program, src)
  code match {
    case Success(result, _) =>
      println(result)
      println((new ArduinoGenerator).generate(result))
    case error: NoSuccess => println(s"error... : $error")
  }
}
