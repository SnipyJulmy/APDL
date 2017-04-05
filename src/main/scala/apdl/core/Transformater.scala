package apdl.core

trait Transformater extends ApdlTfParser {
  val source: String
  lazy val identifier: String = f.identifier.name

  lazy val f: Def = parse(_def, source) match {
    case Success(result, _) => result
    case n: NoSuccess => throw new Exception(s"parser error ! : $n")
  }
  lazy val compiledCode: String = {
    f.toString
  }
  def emitCSource(): Unit = {
    println(cSource)
  }
  def cSource: String = {
    val backend = new ArduinoBackend
    val code = backend.code(f)
    code
  }
}

object Transformater {
  def apply(code: String): Transformater = new Transformater {
    override val source: String = code
  }
}

