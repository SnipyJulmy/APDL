import apdl.parser._
import org.scalacheck.Gen

object StringGenerators {
  def typGen: Gen[String] = Gen.oneOf(ApdlType.values).map(_.toString)

  def parameterGen: Gen[String] = for {
    id <- Gen.identifier
    typ <- typGen
  } yield s"$id : $typ"

  def idGen: Gen[String] = Gen.identifier

  def genGen: Gen[String] = for {
    id <- Gen.identifier
    g <- Gen.alphaNumStr
    s <- Gen.alphaNumStr
    l <- Gen.alphaNumStr
    e <- Gen.alphaNumStr
  } yield
    s"""
       |@gen $id {
       |  global = "$g"
       |  setup = "$s"
       |  loop = "$l"
       |  expr = "$e"
       |}
    """.stripMargin

  def inGen: Gen[String] = for {
    params <- Gen.listOf(parameterGen) suchThat (_.nonEmpty)
  } yield s"@in ${params.mkString(" ")}"

  def outGen: Gen[String] = typGen.map(t => s"@out $t")

  def defineComponentGen: Gen[String] = (for {
    id <- Gen.identifier
    params <- Gen.listOf(parameterGen)
    in <- inGen
    out <- outGen
    gens <- Gen.listOf(genGen)
  } yield
    s"""
       |@define component $id ${params mkString " "} {
       |  $in
       |  $out
       |  ${gens mkString "\n"}
       |}
    """.stripMargin).label("Define component generator")

  def defineInputGen: Gen[String] = (for {
    id <- Gen.identifier
    params <- Gen.listOf(parameterGen)
    gens <- Gen.listOf(genGen)
  } yield
    s"""
       |@define input $id ${params mkString ""} {
       | ${gens mkString "\n"}
       |}
     """.stripMargin).label("Define input generator")
}

object CaseClassGenerators {

  def idGen: Gen[String] = Gen.identifier

  def typGen: Gen[ApdlType] = Gen.oneOf(ApdlType.values)

  def parameterGen: Gen[Parameter] = for {
    id <- idGen
    typ <- typGen
  } yield Parameter(id, typ)

  def genGen: Gen[apdl.parser.Gen] = for {
    g <- Gen.alphaNumStr
    s <- Gen.alphaNumStr
    l <- Gen.alphaNumStr
    e <- Gen.alphaNumStr
  } yield apdl.parser.Gen(g, s, l, e)

  def genGens: Gen[Map[String, apdl.parser.Gen]] = for {
    id <- Gen.listOf(Gen.identifier)
    gen <- Gen.listOf(genGen)
  } yield (id zip gen).toMap

  def inGen: Gen[List[Parameter]] = Gen.listOf(parameterGen).suchThat(_.nonEmpty)

  def outGen: Gen[ApdlType] = typGen

  def defineComponentGen: Gen[DefineComponent] = for {
    id <- Gen.identifier
    params <- Gen.listOf(parameterGen)
    in <- inGen
    out <- outGen
    gens <- genGens
  } yield DefineComponent(id, params, in, out, gens)

  def defineInputGen: Gen[DefineInput] = for {
    id <- Gen.identifier
    params <- Gen.listOf(parameterGen)
    gens <- genGens suchThat(m => m.nonEmpty)
  } yield DefineInput(id, params, gens
}