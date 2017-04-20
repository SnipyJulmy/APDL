package apdl.parser

class ApdlDslException(s: String) extends Throwable {
  def this() = this("")

  override def toString: String = {
    s"$s ${super.toString}"
  }
}

class ApdlParserException(s: String) extends Throwable {
  def this() = this("")

  override def toString: String = {
    s"$s ${super.toString}"
  }
}

class ApdlArgsException(s: String) extends Throwable {
  def this() = this("")

  override def toString: String = {
    s"$s ${super.toString}"
  }
}
