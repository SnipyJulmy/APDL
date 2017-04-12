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
    val result = parser.parse(tf_boolean_expr, new PackratReader(new CharSequenceReader(code))) match {
      case Success(r, _) => r
      case _: NoSuccess =>
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
  assertCompanionObject(Add(5,2),Add(Literal(Number("5")),Literal(Number("2"))))
  assertCompanionObject(Sub(5,10230),Sub(Literal(Number("5")),Literal(Number("10230"))))
  assertCompanionObject(Mul(5,2.123),Mul(Literal(Number("5")),Literal(Number("2.123"))))
  assertCompanionObject(Div(5.2,2),Div(Literal(Number("5.2")),Literal(Number("2"))))

  // (String,String)
  assertCompanionObject(Add("x","y"),Add(Symbol("x"),Symbol("y")))
  assertCompanionObject(Sub("z","y"),Sub(Symbol("z"),Symbol("y")))
  assertCompanionObject(Mul("xasdds","y"),Mul(Symbol("xasdds"),Symbol("y")))
  assertCompanionObject(Div("sad123","asd1"),Div(Symbol("sad123"),Symbol("asd1")))

  // (String,AnyVal)
  assertCompanionObject(Add("x",2),Add(Symbol("x"),Literal(Number("2"))))
  assertCompanionObject(Sub("x",2),Sub(Symbol("x"),Literal(Number("2"))))
  assertCompanionObject(Mul("x",2),Mul(Symbol("x"),Literal(Number("2"))))
  assertCompanionObject(Div("x",2),Div(Symbol("x"),Literal(Number("2"))))

  // (AnyVal,Sting)
  assertCompanionObject(Add(2,"x"),Add(Literal(Number("2")),Symbol("x")))
  assertCompanionObject(Sub(2,"x"),Sub(Literal(Number("2")),Symbol("x")))
  assertCompanionObject(Mul(2,"x"),Mul(Literal(Number("2")),Symbol("x")))
  assertCompanionObject(Div(2,"x"),Div(Literal(Number("2")),Symbol("x")))

  // (Expr,Anyval)
  assertCompanionObject(Add(Add(2,"a"),3),Add(Add(Literal(Number("2")),Symbol("a")),Literal(Number("3"))))
  assertCompanionObject(Sub(Sub(2,"a"),3),Sub(Sub(Literal(Number("2")),Symbol("a")),Literal(Number("3"))))
  assertCompanionObject(Mul(Mul(2,"a"),3),Mul(Mul(Literal(Number("2")),Symbol("a")),Literal(Number("3"))))
  assertCompanionObject(Div(Div(2,"a"),3),Div(Div(Literal(Number("2")),Symbol("a")),Literal(Number("3"))))

  // (AnyVal,Expr)
  assertCompanionObject(Add(3,Add(2,"a")),Add(Literal(Number("3")),Add(Literal(Number("2")),Symbol("a"))))
  assertCompanionObject(Sub(3,Sub(2,"a")),Sub(Literal(Number("3")),Sub(Literal(Number("2")),Symbol("a"))))
  assertCompanionObject(Mul(3,Mul(2,"a")),Mul(Literal(Number("3")),Mul(Literal(Number("2")),Symbol("a"))))
  assertCompanionObject(Div(3,Div(2,"a")),Div(Literal(Number("3")),Div(Literal(Number("2")),Symbol("a"))))

  // (Expr,String)
  assertCompanionObject(Add(Add(2,"a"),"x"),Add(Add(Literal(Number("2")),Symbol("a")),Symbol("x")))
  assertCompanionObject(Sub(Sub(2,"z"),"x"),Sub(Sub(Literal(Number("2")),Symbol("z")),Symbol("x")))
  assertCompanionObject(Mul(Mul(2,"g"),"x"),Mul(Mul(Literal(Number("2")),Symbol("g")),Symbol("x")))
  assertCompanionObject(Div(Div(2,"a"),"y"),Div(Div(Literal(Number("2")),Symbol("a")),Symbol("y")))

  // (String,Expr)
  assertCompanionObject(Add("y",Add(2,"a")),Add(Symbol("y"),Add(Literal(Number("2")),Symbol("a"))))
  assertCompanionObject(Sub("y",Sub(2,"a")),Sub(Symbol("y"),Sub(Literal(Number("2")),Symbol("a"))))
  assertCompanionObject(Mul("y",Mul(2,"a")),Mul(Symbol("y"),Mul(Literal(Number("2")),Symbol("a"))))
  assertCompanionObject(Div("y",Div(2,"a")),Div(Symbol("y"),Div(Literal(Number("2")),Symbol("a"))))

  /* Arithemetic expression ast test */
  assertAst("(2*2)+(4*4)",Add(Mul(2,2),Mul(4,4)))
  assertAst("(2*2)/(4*4)",Div(Mul(2,2),Mul(4,4)))
  assertAst("(2*2)*(4*4)",Mul(Mul(2,2),Mul(4,4)))
  assertAst("(2*2)-(4*4)",Sub(Mul(2,2),Mul(4,4)))
  assertAst("x + 2", Add(Symbol("x"), Literal(Number("2"))))
  assertAst("y - 10", Sub(Symbol("y"), Literal(Number("10"))))
  assertAst("5 * 6",Mul(5,6))
  assertAst("x * y",Mul("x","y"))
  assertAst("asd / asfa",Div("asd","asfa"))
  assertAst("9 / 5 / x * 4",Mul(Div(Div(Literal(Number("9")),Literal(Number("5"))),Symbol("x")),Literal(Number("4"))))
  assertAst("log(x*232)",FunctionCall("log",List(Mul("x",232))))
  assertAst("x * 3 + 4", Add(Mul(Symbol("x"), Literal(Number("3"))), Literal(Number("4"))))
  assertAst("x + 3 * 4", Add(Symbol("x"), Mul(Literal(Number("3")), Literal(Number("4")))))
  assertAst("x * 3 / 4", Div(Mul(Symbol("x"), Literal(Number("3"))), Literal(Number("4"))))
  assertAst("(x + 3) * 4", Mul(Add("x", 3), 4))
  assertAst("x / 4 / 5 / 6 / 7 / 8 / 9", Div(Div(Div(Div(Div(Div("x", 4), 5), 6), 7), 8), 9))
  assertAst("x * 4 * 5 * 6 * 7 * 8 * 9", Mul(Mul(Mul(Mul(Mul(Mul("x", 4), 5), 6), 7), 8), 9))
  assertAst("x / 4 * 5 / 6 * 7 / 8 * 9", Mul(Div(Mul(Div(Mul(Div("x", 4), 5), 6), 7), 8), 9))
  assertAst("log(x)", FunctionCall("log", List(Symbol("x"))))
  assertAst("sqrt(x)", FunctionCall("sqrt", List(Symbol("x"))))
  assertAst("pow(x,y)", FunctionCall("pow", List(Symbol("x"), Symbol("y"))))
  assertAst("fct(1+2,2+3*3,fct(4,4,5))", FunctionCall("fct",
    List(Add(1, 2), Add(2, Mul(3, 3)), FunctionCall("fct", List(
      Literal(Number("4")),
      Literal(Number("4")),
      Literal(Number("5"))
    )))))
  assertAst("x", Symbol("x"))
  assertAst("1", Literal(Number("1")))
  assertAst("1.29", Literal(Number("1.29")))
  assertAst("-123", Literal(Number("-123")))
  assertAst("+123", Literal(Number("+123")))

  val a = BooleanSymbol("a")
  val b = BooleanSymbol("b")
  val c = BooleanSymbol("c")

  /* Boolean expression AST test */
  assertBooleanAst("true", True())
  assertBooleanAst("false", False())
  assertBooleanAst("a || true", Or(BooleanSymbol("a"),True()))
  assertBooleanAst("false && b", And(False(),BooleanSymbol("b")))
  assertBooleanAst("!c", Not(BooleanSymbol("c")))
  assertBooleanAst("x", BooleanSymbol("x"))
  assertBooleanAst("5 < 4",Smaller(Literal(Number("5")),Literal(Number("4"))))
  assertBooleanAst("7 > 8",Greater(Literal(Number("7")),Literal(Number("8"))))
  assertBooleanAst("x == b",Equals(Symbol("x"),Symbol("b")))
  assertBooleanAst("x != b",NotEquals(Symbol("x"),Symbol("b")))
  assertBooleanAst("x <= 10",SmallerEquals(Symbol("x"),Literal(Number("10"))))
  assertBooleanAst("z >= 2 + 3 * 4",GreaterEquals(Symbol("z"),Add(2,Mul(3,4))))
  assertBooleanAst("3 * x > log(10)",Greater(Mul(3,"x"),FunctionCall("log",List(Literal(Number("10"))))))
  assertBooleanAst("3 * x > 5 && x < 10",And(Greater(Mul(3,"x"),Literal(Number("5"))),Smaller(Symbol("x"),Literal(Number("10")))))
  assertBooleanAst("a && b && c",And(And(a,b),c))

  /* Equiv expr */
  assertEquivExpr("x*2","(x*2)")
  assertEquivExpr("x*2+3","(x*2)+3")
  assertEquivExpr("3 + (4 * 5)","3 + 4 * 5")
}
