package apdl.script_syntax

import apdl.parser._
import org.scalatest.FlatSpec

import scala.util.parsing.input.CharSequenceReader

class TfExpressionTest extends FlatSpec {
  val parser = new ApdlParser

  import parser._

  def assertEquivExpr(a : String, b : String) : Unit = {
    val resultA = parser.parse(tf_expr, new PackratReader[Char](new CharSequenceReader(a))) match {
      case Success(result, _) => result
      case error: NoSuccess => throw new ApdlParserException(s"can't parse $a -> $error")
    }

    val resultB = parser.parse(tf_expr, new PackratReader[Char](new CharSequenceReader(b))) match {
      case Success(result, _) => result
      case error: NoSuccess => throw new ApdlParserException(s"can't parse $b -> $error")
    }

    s"$a and $b" should "produce the same AST" in {
      assert(resultA == resultB)
    }
  }

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

  def assertBooleanAst(code: String, expected: Expr): Unit = {
    val result = parser.parse(tf_expr, new PackratReader(new CharSequenceReader(code))) match {
      case Success(r, _) => r
      case n: NoSuccess =>
        println(n)
        //noinspection NameBooleanParameters
        assert(false)
    }

    s"$code" should s"produce $expected" in {
      assert(result == expected)
    }
  }

  def assertCompanionObject(a : Expr, b : Expr) : Unit = {
    s"$a and $b" should "produce the same AST" in {
      assert(a == b)
    }
  }

  /* Companion object test (apply method) */

  // (AnyVal,AnyVal)
  assertCompanionObject(Add(5,2),Add(Literal("5"),Literal("2")))
  assertCompanionObject(Sub(5,10230),Sub(Literal("5"),Literal("10230")))
  assertCompanionObject(Mul(5,2.123),Mul(Literal("5"),Literal("2.123")))
  assertCompanionObject(Div(5.2,2),Div(Literal("5.2"),Literal("2")))

  // (String,String)
  assertCompanionObject(Add("x","y"),Add(Symbol("x"),Symbol("y")))
  assertCompanionObject(Sub("z","y"),Sub(Symbol("z"),Symbol("y")))
  assertCompanionObject(Mul("xasdds","y"),Mul(Symbol("xasdds"),Symbol("y")))
  assertCompanionObject(Div("sad123","asd1"),Div(Symbol("sad123"),Symbol("asd1")))

  // (String,AnyVal)
  assertCompanionObject(Add("x",2),Add(Symbol("x"),Literal("2")))
  assertCompanionObject(Sub("x",2),Sub(Symbol("x"),Literal("2")))
  assertCompanionObject(Mul("x",2),Mul(Symbol("x"),Literal("2")))
  assertCompanionObject(Div("x",2),Div(Symbol("x"),Literal("2")))

  // (AnyVal,Sting)
  assertCompanionObject(Add(2,"x"),Add(Literal("2"),Symbol("x")))
  assertCompanionObject(Sub(2,"x"),Sub(Literal("2"),Symbol("x")))
  assertCompanionObject(Mul(2,"x"),Mul(Literal("2"),Symbol("x")))
  assertCompanionObject(Div(2,"x"),Div(Literal("2"),Symbol("x")))

  // (Expr,Anyval)
  assertCompanionObject(Add(Add(2,"a"),3),Add(Add(Literal("2"),Symbol("a")),Literal("3")))
  assertCompanionObject(Sub(Sub(2,"a"),3),Sub(Sub(Literal("2"),Symbol("a")),Literal("3")))
  assertCompanionObject(Mul(Mul(2,"a"),3),Mul(Mul(Literal("2"),Symbol("a")),Literal("3")))
  assertCompanionObject(Div(Div(2,"a"),3),Div(Div(Literal("2"),Symbol("a")),Literal("3")))

  // (AnyVal,Expr)
  assertCompanionObject(Add(3,Add(2,"a")),Add(Literal("3"),Add(Literal("2"),Symbol("a"))))
  assertCompanionObject(Sub(3,Sub(2,"a")),Sub(Literal("3"),Sub(Literal("2"),Symbol("a"))))
  assertCompanionObject(Mul(3,Mul(2,"a")),Mul(Literal("3"),Mul(Literal("2"),Symbol("a"))))
  assertCompanionObject(Div(3,Div(2,"a")),Div(Literal("3"),Div(Literal("2"),Symbol("a"))))

  // (Expr,String)
  assertCompanionObject(Add(Add(2,"a"),"x"),Add(Add(Literal("2"),Symbol("a")),Symbol("x")))
  assertCompanionObject(Sub(Sub(2,"z"),"x"),Sub(Sub(Literal("2"),Symbol("z")),Symbol("x")))
  assertCompanionObject(Mul(Mul(2,"g"),"x"),Mul(Mul(Literal("2"),Symbol("g")),Symbol("x")))
  assertCompanionObject(Div(Div(2,"a"),"y"),Div(Div(Literal("2"),Symbol("a")),Symbol("y")))

  // (String,Expr)
  assertCompanionObject(Add("y",Add(2,"a")),Add(Symbol("y"),Add(Literal("2"),Symbol("a"))))
  assertCompanionObject(Sub("y",Sub(2,"a")),Sub(Symbol("y"),Sub(Literal("2"),Symbol("a"))))
  assertCompanionObject(Mul("y",Mul(2,"a")),Mul(Symbol("y"),Mul(Literal("2"),Symbol("a"))))
  assertCompanionObject(Div("y",Div(2,"a")),Div(Symbol("y"),Div(Literal("2"),Symbol("a"))))

  /* Arithemetic expression ast test */
  assertAst("(2*2)+(4*4)",Add(Mul(2,2),Mul(4,4)))
  assertAst("(2*2)/(4*4)",Div(Mul(2,2),Mul(4,4)))
  assertAst("(2*2)*(4*4)",Mul(Mul(2,2),Mul(4,4)))
  assertAst("(2*2)-(4*4)",Sub(Mul(2,2),Mul(4,4)))
  assertAst("x + 2", Add(Symbol("x"), Literal("2")))
  assertAst("y - 10", Sub(Symbol("y"), Literal("10")))
  assertAst("5 * 6",Mul(5,6))
  assertAst("x * y",Mul("x","y"))
  assertAst("asd / asfa",Div("asd","asfa"))
  assertAst("9 / 5 / x * 4",Mul(Div(Div(Literal("9"),Literal("5")),Symbol("x")),Literal("4")))
  assertAst("log(x*232)",FunctionCall("log",List(Mul("x",232))))
  assertAst("x * 3 + 4", Add(Mul(Symbol("x"), Literal("3")), Literal("4")))
  assertAst("x + 3 * 4", Add(Symbol("x"), Mul(Literal("3"), Literal("4"))))
  assertAst("x * 3 / 4", Div(Mul(Symbol("x"), Literal("3")), Literal("4")))
  assertAst("(x + 3) * 4", Mul(Add("x", 3), 4))
  assertAst("x / 4 / 5 / 6 / 7 / 8 / 9", Div(Div(Div(Div(Div(Div("x", 4), 5), 6), 7), 8), 9))
  assertAst("x * 4 * 5 * 6 * 7 * 8 * 9", Mul(Mul(Mul(Mul(Mul(Mul("x", 4), 5), 6), 7), 8), 9))
  assertAst("x / 4 * 5 / 6 * 7 / 8 * 9", Mul(Div(Mul(Div(Mul(Div("x", 4), 5), 6), 7), 8), 9))
  assertAst("log(x)", FunctionCall("log", List(Symbol("x"))))
  assertAst("sqrt(x)", FunctionCall("sqrt", List(Symbol("x"))))
  assertAst("pow(x,y)", FunctionCall("pow", List(Symbol("x"), Symbol("y"))))
  assertAst("fct(1+2,2+3*3,fct(4,4,5))", FunctionCall("fct",
    List(Add(1, 2), Add(2, Mul(3, 3)), FunctionCall("fct", List(
      Literal("4"),
      Literal("4"),
      Literal("5")
    )))))
  assertAst("x", Symbol("x"))
  assertAst("1", Literal("1"))
  assertAst("1.29", Literal("1.29"))
  assertAst("-123", Literal("-123"))
  assertAst("+123", Literal("+123"))

  val a = Symbol("a")
  val b = Symbol("b")
  val c = Symbol("c")

  /* Boolean expression AST test */
  assertBooleanAst("true", True())
  assertBooleanAst("false", False())
  assertBooleanAst("a || true", Or(Symbol("a"),True()))
  assertBooleanAst("false && b", And(False(),Symbol("b")))
  assertBooleanAst("5 < 4",Smaller(Literal("5"),Literal("4")))
  assertBooleanAst("7 > 8",Greater(Literal("7"),Literal("8")))
  assertBooleanAst("x == b",Equals(Symbol("x"),Symbol("b")))
  assertBooleanAst("x != b",NotEquals(Symbol("x"),Symbol("b")))
  assertBooleanAst("x <= 10",SmallerEquals(Symbol("x"),Literal("10")))
  assertBooleanAst("z >= 2 + 3 * 4",GreaterEquals(Symbol("z"),Add(2,Mul(3,4))))
  assertBooleanAst("3 * x > log(10)",Greater(Mul(3,"x"),FunctionCall("log",List(Literal("10")))))
  assertBooleanAst("3 * x > 5 && x < 10",And(Greater(Mul(3,"x"),Literal("5")),Smaller(Symbol("x"),Literal("10"))))
  assertBooleanAst("a && b && c",And(And(a,b),c))
  assertBooleanAst("a || b && c",Or(a,And(b,c)))
  assertBooleanAst("a || b || c",Or(Or(a,b),c))
  assertBooleanAst("(a || b) && c",And(Or(a,b),c))

  /* Equiv expr */
  assertEquivExpr("x*2","(x*2)")
  assertEquivExpr("x*2+3","(x*2)+3")
  assertEquivExpr("3 + (4 * 5)","3 + 4 * 5")
}
