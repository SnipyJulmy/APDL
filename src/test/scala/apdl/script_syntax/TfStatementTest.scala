package apdl.script_syntax

import apdl.parser._
import org.scalatest.FlatSpec

import scala.util.parsing.input.CharSequenceReader

class TfStatementTest extends FlatSpec {

  val parser = new ApdlParser

  import parser._

  def assertAst(code: String, expected: Statement): Unit = {
    val result = parser.parse(tf_statement, new PackratReader(new CharSequenceReader(code))) match {
      case Success(r, _) => r
      case _: NoSuccess =>
        //noinspection NameBooleanParameters
        assert(false)
    }

    s"$code" should s"produce $expected" in {
      assert(result == expected)
    }
  }
}
