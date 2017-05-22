import apdl.parser._
import org.scalacheck.Prop._

class DefineTest extends ApdlFlatSpec {

  val apdlCodeGenerator = new DslApdlBackendGenerators {}

  import parser._

  behavior of "DefineParser"

  it should "Parse some correct defined component" in {
    check {
      forAllNoShrink(StringGenerators.defineComponentGen) { c =>
        parse(c, apdlDefine).isInstanceOf[ApdlDefineComponent]
      }
    }
  }

  it should "Parse some correct defined input" in {
    check {
      forAllNoShrink(StringGenerators.defineInputGen) { i =>
        parse(i, apdlDefine).isInstanceOf[ApdlDefineInput]
      }
    }
  }

  it should "Parse some correct defined @gen" in {
    check {
      forAllNoShrink(StringGenerators.genGen) { g =>
        val (_,genData) = parse(g, gen)
        genData.isInstanceOf[Gen]
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
    val ast = parse(t1, apdlDefine)
    ast match {
      case ApdlDefineComponent(name, parameters, inputs, outputType, gens) =>
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

  it should "Parse some correct AST transcoded to code" in {
    val apdlDefineGenerators = new ApdlDefineGenerators(3,3)
    check {
      forAll(apdlDefineGenerators.typGen) { t =>
        val code = apdlCodeGenerator.toApdlCode(t)
        val ast = parse(code, apdlType)
        ast == t
      }
      forAll(apdlDefineGenerators.genParameter) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, parameter)
        ast == x
      }
      forAll(apdlDefineGenerators.genGen) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, genBody)
        ast == x
      }
      forAll(apdlDefineGenerators.genGens) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, gens)
        ast == x
      }
      forAll(apdlDefineGenerators.inGen) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, inputs)
        ast == x
      }
      forAll(apdlDefineGenerators.outGen) { x =>
        val code = apdlCodeGenerator.toApdlCode(x)
        val ast = parse(code, output)
        ast == x
      }
      forAll(apdlDefineGenerators.genDefineComponent) { x =>
        val code = apdlCodeGenerator.toApdlCode(x.asInstanceOf[ApdlDefine])
        val ast = parse(code, apdlDefine)
        ast match {
          case component: ApdlDefineComponent => component == x
          case _ => false
        }
      }
      forAll(apdlDefineGenerators.genDefineInput) { x =>
        val code = apdlCodeGenerator.toApdlCode(x.asInstanceOf[ApdlDefine])
        val ast = parse(code, apdlDefine)
        ast match {
          case input: ApdlDefineInput => input == x
          case _ => false
        }
      }
      forAll(apdlDefineGenerators.genDefineTransform) { x =>
        val code = apdlCodeGenerator.toApdlCode(x.asInstanceOf[ApdlDefine])
        val ast = parse(code, apdlDefine)
        ast == x
      }
    }
  }
}