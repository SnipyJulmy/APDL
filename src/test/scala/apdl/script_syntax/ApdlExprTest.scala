package apdl.script_syntax

import apdl.parser._
import org.scalatest.FlatSpec

import scala.util.parsing.input.CharSequenceReader

class ApdlExprTest extends FlatSpec {
  val parser = new ApdlParser

  import parser._

  def assertAst(code: String, expected: Expr): Unit = {
    val result = parser.parse(tf_expr, new PackratReader(new CharSequenceReader(code))) match {
      case Success(r, _) => r
      case _: NoSuccess =>
        //noinspection NameBooleanParameters
        assert(false)
    }

    s"$code" should s"produce $expected" in {
      assert(result == expected)
    }
  }

  assertAst("x + 2", Add(Symbol("x"), Literal(Number("2"))))
  assertAst("y - 10", Sub(Symbol("y"), Literal(Number("10"))))
  assertAst("x * 3 + 4", Add(Mul(Symbol("x"), Literal(Number("3"))), Literal(Number("4"))))
  assertAst("x + 3 * 4", Add(Symbol("x"), Mul(Literal(Number("3")), Literal(Number("4")))))
  assertAst("x * 3 / 4", Div(Mul(Symbol("x"), Literal(Number("3"))), Literal(Number("4"))))
  assertAst("(x + 3) * 4", Mul(Add("x",3),4))
  assertAst("x / 4 / 5 / 6 / 7 / 8 / 9",Div(Div(Div(Div(Div(Div("x",4),5),6),7),8),9))
  assertAst("x * 4 * 5 * 6 * 7 * 8 * 9",Mul(Mul(Mul(Mul(Mul(Mul("x",4),5),6),7),8),9))
  assertAst("x / 4 * 5 / 6 * 7 / 8 * 9",Mul(Div(Mul(Div(Mul(Div("x",4),5),6),7),8),9))
  assertAst("log(x)",FunctionCall("log",List(Symbol("x"))))
  assertAst("sqrt(x)",FunctionCall("sqrt",List(Symbol("x"))))
  assertAst("pow(x,y)",FunctionCall("pow",List(Symbol("x"),Symbol("y"))))
}
