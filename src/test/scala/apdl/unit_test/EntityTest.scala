package apdl.unit_test

import apdl.parser._
import org.scalatest.FlatSpec

import scala.util.parsing.input.CharSequenceReader

class EntityTest extends FlatSpec {

  def parseTransform(code: String): Transformater = {
    val parser = new ApdlParser
    import parser._

    parser.parse(transform, new PackratReader[Char](new CharSequenceReader(code))) match {
      case Success(result, next) =>
        if (!next.atEnd) {
          throw new ApdlParserException(s"Unable to completely parse $code")
        }
        result
      case n: NoSuccess =>
        throw new ApdlParserException(s"Unable to parse $code : $n")
    }
  }

  /* Transformation Test */

  val t1: String =
    """|transform def tf (x:int) -> float {
       |    val B : int = 3975
       |    val resistance : float = (float)(1023 - x) * 1000 / x
       |    val temperature : float = 1 / (log(resistance/1000) / B + 1 / 298.15) - 273.15
       |    return temperature
       |}""".stripMargin

  t1 should "produce the correct ast" in {
    val expected: Transformater = Transformater(FunctionDecl(
      FunctionHeader(TfFloat(), "tf", List(TypedIdentifier("x", TfInt()))),
      FunctionBody(Block(List(
        NewVal(Symbol("B"), TfInt(), Literal("3975")),
        NewVal(Symbol("resistance"), TfFloat(), Div(Mul(Cast(TfFloat(), Sub(Literal("1023"), Symbol("x"))), Literal("1000")), Symbol("x"))),
        NewVal(Symbol("temperature"), TfFloat(),
          Sub(
            Div(Literal("1"),
              Add(
                Div(FunctionCall("log", List(Div(Symbol("resistance"), Literal("1000")))), Symbol("B")),
                Div(Literal("1"), Literal("298.15"))
              )
            ),
            Literal("273.15"))),
        Return(Symbol("temperature"))
      )))))
    assert(expected == parseTransform(t1))
  }

  val t2: String =
    """|transform def x() -> int {
       | return 2
       |}""".stripMargin

  t2 should "produce the correct ast" in {
    val expected = Transformater(FunctionDecl(
      FunctionHeader(TfInt(), "x", List()),
      FunctionBody(Block(List(Return(Literal("2")))))
    ))
    assert(expected == parseTransform(t2))
  }

  val t3: String =
    """|transform def max(a:float,b:float,c:float,d:float) -> float {
       |  return max(max(a,b),max(c,d))
       |}""".stripMargin

  t3 should "produce the correct ast" in {
    val expected: Transformater = Transformater(FunctionDecl(
      FunctionHeader(
        TfFloat(),
        "max",
        List(
          TypedIdentifier("a", TfFloat()),
          TypedIdentifier("b", TfFloat()),
          TypedIdentifier("c", TfFloat()),
          TypedIdentifier("d", TfFloat())
        )
      ),
      FunctionBody(Block(List(
        Return(FunctionCall("max", List(
          FunctionCall("max", List(Symbol("a"), Symbol("b"))),
          FunctionCall("max", List(Symbol("c"), Symbol("d")))
        )))
      )))
    ))

    assert(expected == parseTransform(t3))
  }

  val t4: String =
    """|transform def factorial (x:int) -> int {
       |  if (x < 2) return 1 else return x * factorial(x - 1)
       |}""".stripMargin

  t4 should "produce the correct ast" in {
    val expected: Transformater = Transformater(FunctionDecl(
      FunctionHeader(TfInt(), "factorial", List(TypedIdentifier("x", TfInt()))),
      FunctionBody(Block(List(IfThenElse(
        Smaller(Symbol("x"), Literal("2")),
        Return(Literal("1")),
        Return(Mul(Symbol("x"), FunctionCall("factorial", List(Sub(Symbol("x"), Literal("1"))))))
      ))))
    ))
    assert(expected == parseTransform(t4))
  }
  val t5: String =
    """|transform def sumArray (a:int[],size:int) -> int {
       |  var res : int = 0
       |  while(size > 0) {
       |    res = res + a[size]
       |    size = size - 1
       |  }
       |  return res
       |}""".stripMargin
  t5 should "produce the correct ast" in {
    val expected: Transformater = Transformater(FunctionDecl(
      FunctionHeader(TfInt(), "sumArray", List(
        TypedIdentifier("a", TfArray(TfInt())),
        TypedIdentifier("size", TfInt())
      )),
      FunctionBody(Block(List(
        NewVar(Symbol("res"), TfInt(), Some(Literal("0"))),
        While(
          Greater(Symbol("size"), Literal("0")),
          Block(List(
            VarAssignement(Symbol("res"), Add(Symbol("res"), ArrayAccess(Symbol("a"), Symbol("size")))),
            VarAssignement(Symbol("size"), Sub(Symbol("size"), Literal("1")))
          ))
        ),
        Return(Symbol("res"))
      )))
    ))
    assert(expected == parseTransform(t5))
  }

  val t6: String =
    """|transform def printTemp(temp : int) -> void {
       |  printf("%d\n",temp)
       |}""".stripMargin

  t6 should "throw a ApdlParserExeception" in {
    assertThrows[ApdlParserException](parseTransform(t6))
  }
}
