package apdl

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

class ApdlBackendException(s: String) extends Throwable {
  def this() = this("")

  override def toString: String = {
    s"$s ${super.toString}"
  }
}

class ApdlFormatException(s: String) extends ApdlParserException {
  def this() = this("")

  override def toString: String = {
    s"$s ${super.toString}"
  }
}