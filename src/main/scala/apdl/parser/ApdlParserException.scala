package apdl.parser

class ApdlDslException(s: String) extends Throwable {
  override def toString: String = {
    s"$s ${super.toString}"
  }
}

class ApdlParserException(s: String) extends Throwable {
  override def toString: String = {
    s"$s ${super.toString}"
  }
}
