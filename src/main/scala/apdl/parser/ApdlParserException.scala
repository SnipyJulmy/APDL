package apdl.parser

class ApdlParserException(s: String) extends Throwable {
  override def toString: String = {
    s"$s ${super.toString}"
  }
}
