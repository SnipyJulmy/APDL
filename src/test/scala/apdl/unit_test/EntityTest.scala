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
      case _: NoSuccess =>
        throw new ApdlParserException("Unable to parse $code")
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
}
