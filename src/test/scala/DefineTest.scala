import apdl.ApdlParserException
import apdl.parser._
import org.scalacheck.Prop._
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

import scala.util.parsing.input.CharSequenceReader

class DefineTest extends FlatSpec with Checkers {

  val parser = new DefineParsers

  import parser._

  val apdlCodeGenerator = new DslApdlBackendGenerators {}

  private def parse[A](code: String, astParser: Parser[A]): A = {
    parser.parse(astParser, new PackratReader[Char](new CharSequenceReader(code))) match {
      case Success(result, next) =>
        if (!dropWs(next).atEnd) throw new ApdlParserException(s"Unable to parse completely $code: $next")
        else result
      case n: NoSuccess =>
        if (code != "") throw new ApdlParserException(s"Unable to parse $code: $n")
        else throw new ApdlParserException(s"Unable to parse '': $n")
    }
  }

  private def dropWs(input: parser.Input): parser.Input = {
    if (input.atEnd)
      input
    else {
      if (parser.ws.pattern.matcher(input.first.toString).matches())
        dropWs(input.rest)
      else
        input
    }
  }

  behavior of "DefineParser"

  it should "Parse some correct defined component" in {
    check {
      forAllNoShrink(StringGenerators.defineComponentGen) { c =>
        parse(c, define).isInstanceOf[DefineComponent]
      }
    }
  }

  it should "Parse some correct defined input" in {
    check {
      forAllNoShrink(StringGenerators.defineInputGen) { i =>
        parse(i, define).isInstanceOf[DefineInput]
      }
    }
  }

  it should "Parse some correct defined @gen" in {
    check {
      forAllNoShrink(StringGenerators.genGen) { g =>
        parse(g, gen)._2.isInstanceOf[Gen]
      }
    }
  }

  it should "Parse some correct defined @in" in {
    check {
      forAllNoShrink(StringGenerators.inGen) { i =>
        parse(i, inputs).isInstanceOf[List[Parameter]]
      }
    }
  }

  it should "Parse some correct defined @out" in {
    check {
      forAllNoShrink(StringGenerators.outGen) { o =>
        parse(o, output).isInstanceOf[ApdlType]
      }
    }
  }

  val t1: String =
    """
      |@define component simpleOperator op:str {
      |    @in x:num y:num
      |    @out num
      |    @gen mbed {
      |        global = ""
      |        setup = ""
      |        loop = ""
      |        expr = "@x @op @y"
      |    }
      |    @gen arduino {
      |        global = ""
      |        setup = ""
      |        loop = ""
      |        expr = "@x @op @y"
      |    }
      |}
    """.stripMargin

  //noinspection VariablePatternShadow
  it should s"Produce the correct AST for $t1" in {
    val ast = parse(t1, define)
    ast match {
      case DefineComponent(name, parameters, inputs, outputType, gens) =>
        assert(name == "simpleOperator")
        assert(parameters == List(Parameter("op", ApdlType.Str)))
        assert(inputs == List(Parameter("x", ApdlType.Num), Parameter("y", ApdlType.Num)))
        assert(outputType == ApdlType.Num)
        assert(gens == Map(
          "mbed" -> Gen("", "", "", "@x @op @y"),
          "arduino" -> Gen("", "", "", "@x @op @y")
        ))
      case _ => fail("Should not produce something else than a define component")
    }
  }

  it should "Parse some correct AST transpilled to code" in {
    val apdlTestGenerators = new ApdlTestGenerators(1,1)
    import apdlTestGenerators._

    check {
      forAll(typGen) { t =>
        val code = apdlCodeGenerator.toApdlCode(t)
        val ast = parse(code, apdlType)
        ast == t
      }
      forAll(parameterGen) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, parameter)
        ast == x
      }
      forAll(genGen) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, genBody)
        ast == x
      }
      forAll(genGens) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, gens)
        ast == x
      }
      forAll(inGen) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, inputs)
        ast == x
      }
      forAll(outGen) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, output)
        ast == x
      }
      forAll(defineComponentGen) { x =>
        val code = apdlCodeGenerator.toApdlCode(x.asInstanceOf[Define])
        val ast = parse(code, define)
        ast match {
          case component: DefineComponent => component == x
          case _ => false
        }
      }
      forAll(defineInputGen) { x =>
        val code = apdlCodeGenerator.toApdlCode(x.asInstanceOf[Define])
        val ast = parse(code, define)
        ast match {
          case input: DefineInput => input == x
          case _ => false
        }
      }
    }
    check {
      forAll(defineTransformGen(1)) { x =>
        val code = apdlCodeGenerator.toApdlCode(x.asInstanceOf[Define])
        val ast = parse(code, define)
        ast match {
          case tf: DefineTransform => tf == x
          case _ =>
            false
        }
      }
    }
  }
}
