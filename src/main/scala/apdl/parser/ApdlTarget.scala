package apdl.parser

sealed trait ApdlTarget
object ApdlTarget {
  def arg2target(arg: String): ApdlTarget = arg match {
    case "arduino" => Arduino
    case _ => throw new ApdlArgsException(s"Unsupported target : $arg")
  }
}

object Arduino extends ApdlTarget
