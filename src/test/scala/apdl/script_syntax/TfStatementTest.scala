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

  val i = Symbol("i")
  val j = Symbol("j")
  val n = Symbol("n")
  val a = Symbol("a")
  val b = Symbol("b")
  val x = Symbol("x")
  val zero = Literal("0")
  val one = Literal("1")
  val five = Literal("5")
  val ten = Literal("10")

  assertAst("while(i > 0) i = i - 1", While(Greater(i, zero), VarAssignement(i, Sub(i, 1))))
  assertAst("while(i > 0 && j < 10) n = n * 2 + 1", While(And(Greater(i, zero), Smaller(j, ten)), VarAssignement(n, Add(Mul(n, 2), 1))))
  assertAst("do i = i + 1 while(true)", DoWhile(True(), VarAssignement(i, Add(i, 1))))
  assertAst(
    "if(a == b) print(a) else print(b)",
    IfThenElse(
      Equals(a, b),
      ExpressionStatement(FunctionCall("print", List(a))),
      ExpressionStatement(FunctionCall("print", List(b)))
    )
  )
  assertAst("i = 5",VarAssignement(i,Literal("5")))
  assertAst("i = 5.12",VarAssignement(i,Literal("5.12")))
  assertAst("i = i + 1",VarAssignement(i,Add(i,1)))
  assertAst("i = j > 1 || b < 1",VarAssignement(i,Or(Greater(j,one),Smaller(b,one))))
  assertAst("if (a || b < 5) x = log(b)",IfThen(Or(a,Smaller(b,five)),VarAssignement(x,FunctionCall("log",List(b)))))
}
