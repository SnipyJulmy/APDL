package apdl.unit_test

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
  val two = Literal("2")
  val three = Literal("3")
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
  assertAst("var j : int = i + 1",NewVar(j,TfInt(),Some(Add(i,1))))
  assertAst("var array : int[] = {1,2,3}",NewArray(Symbol("array"),TfArray(TfInt()),ArrayInitValue(List(one,two,three))))
  assertAst("array[9] = 10",ArrayAssignement(Symbol("array"),Literal("9"),Literal("10")))
  assertAst("array[i] = i",ArrayAssignement(Symbol("array"),i,i))
  assertAst("var array : float[] = [100]",NewArray(Symbol("array"),TfArray(TfFloat()),ArrayInitCapacity(Literal("100"))))
  assertAst("var a : int", NewVar(a,TfInt(),None))
  assertAst("var a : short", NewVar(a,TfShort(),None))
  assertAst("var a : float", NewVar(a,TfFloat(),None))
  assertAst("var a : double", NewVar(a,TfDouble(),None))
  assertAst("var a : byte", NewVar(a,TfByte(),None))
  assertAst("var a : char", NewVar(a,TfChar(),None))

  assertThrows[ApdlParserException] {
    assertAst("var a : int[]",NewVar(a,TfArray(TfInt()),None))
  }

  assertThrows[ApdlParserException] {
    assertAst(
      """
        |var a : int[]
        |var b : float = 3.2
      """.stripMargin,NewVar(a,TfArray(TfInt()),None))
  }

}
