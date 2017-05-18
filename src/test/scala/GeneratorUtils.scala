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
    gens <- genGens suchThat (m => m.nonEmpty)
  } yield DefineInput(id, params, gens)

  /* APDL Transform DSL case class Generator */

  def genExpr: Gen[Expr] = Gen.oneOf(
    genAdd, genMul, genSub, genDiv, genSymbol, genLiteral,
    genTrue, genFalse, genOr, genAdd, genNot,
    genSmaller, genSmallerEquals, genEquals, genNotEquals, genGreater, genGreaterEquals,
    genFunctionCall
  )

  def genAdd: Gen[Add] = for {
    e1 <- genExpr
    e2 <- genExpr
  } yield Add(e1, e2)

  def genMul: Gen[Mul] = for {
    e1 <- genExpr
    e2 <- genExpr
  } yield Mul(e1, e2)

  def genSub: Gen[Sub] = for {
    e1 <- genExpr
    e2 <- genExpr
  } yield Sub(e1, e2)

  def genDiv: Gen[Div] = for {
    e1 <- genExpr
    e2 <- genExpr
  } yield Div(e1, e2)

  def genCast: Gen[Cast] = for {
    typ <- genPrimitivesTyp
    expr <- genExpr
  } yield Cast(typ, expr)

  def genLiteral: Gen[Literal] = for {
    num <- Gen.numStr
  } yield Literal(num)

  def genSymbol: Gen[Symbol] = for {
    id <- Gen.identifier
  } yield Symbol(id)

  def genFunctionCall: Gen[FunctionCall] = for {
    id <- Gen.identifier
    args <- Gen.listOf(genExpr)
  } yield FunctionCall(id, args)

  def genArrayAccess: Gen[ArrayAccess] = for {
    array <- genTargetExpr
    field <- genExpr
  } yield ArrayAccess(array, field)

  def genTrue: Gen[True] = True()

  def genFalse: Gen[False] = False()

  def genOr: Gen[Or] = for {
    e1 <- genExpr
    e2 <- genExpr
  } yield Or(e1, e2)

  def genAnd: Gen[And] = for {
    e1 <- genExpr
    e2 <- genExpr
  } yield And(e1, e2)

  def genNot: Gen[Not] = for {
    e1 <- genExpr
  } yield Not(e1)

  def genGreater: Gen[Greater] = for {
    e1 <- genExpr
    e2 <- genExpr
  } yield Greater(e1, e2)

  def genSmaller: Gen[Smaller] = for {
    e1 <- genExpr
    e2 <- genExpr
  } yield Smaller(e1, e2)

  def genGreaterEquals: Gen[GreaterEquals] = for {
    e1 <- genExpr
    e2 <- genExpr
  } yield GreaterEquals(e1, e2)

  def genSmallerEquals: Gen[SmallerEquals] = for {
    e1 <- genExpr
    e2 <- genExpr
  } yield SmallerEquals(e1, e2)

  def genEquals: Gen[Equals] = for {
    e1 <- genExpr
    e2 <- genExpr
  } yield Equals(e1, e2)

  def genNotEquals: Gen[NotEquals] = for {
    e1 <- genExpr
    e2 <- genExpr
  } yield NotEquals(e1, e2)

  def genPrimitivesTyp: Gen[TfPrimitivesTyp] = Gen.oneOf(
    genTfBoolean, genTfInt, genTfLong, genTfByte,
    genTfShort, genTfChar, genTfDouble, genTfFloat
  )

  def genTyp: Gen[TfTyp] = Gen.oneOf(
    genTfBoolean, genTfInt, genTfLong, genTfByte,
    genTfShort, genTfChar, genTfDouble, genTfFloat, genTfArray
  )

  def genRetTyp: Gen[TfRetTyp] = Gen.oneOf(
    genTfBoolean, genTfInt, genTfLong, genTfByte, genTfVoid,
    genTfShort, genTfChar, genTfDouble, genTfFloat, genTfArray
  )

  def genTfBoolean: Gen[TfBoolean] = TfBoolean()
  def genTfInt: Gen[TfInt] = TfInt()
  def genTfLong: Gen[TfLong] = TfLong()
  def genTfByte: Gen[TfByte] = TfByte()
  def genTfShort: Gen[TfShort] = TfShort()
  def genTfChar: Gen[TfChar] = TfChar()
  def genTfDouble: Gen[TfDouble] = TfDouble()
  def genTfFloat: Gen[TfFloat] = TfFloat()
  def genTfVoid: Gen[TfVoid] = TfVoid()

  def genTfArray: Gen[TfArray] = for {
    typ <- genTyp
  } yield TfArray(typ)

  def genTypedIdentifier: Gen[TypedIdentifier] = for {
    id <- Gen.identifier
    typ <- genTyp
  } yield TypedIdentifier(id, typ)

  def genStatement: Gen[Statement] = Gen.oneOf(
    genWhile, genDoWhile
  )

  def genWhile: Gen[While] = for {
    cond <- genExpr
    statement <- genStatement
  } yield While(cond, statement)

  def genDoWhile: Gen[DoWhile] = for {
    cond <- genExpr
    statement <- genStatement
  } yield DoWhile(cond, statement)

  def genIfThenElse: Gen[IfThenElse] = for {
    cond <- genExpr
    trueStatement <- genStatement
    falseStatement <- genStatement
  } yield IfThenElse(cond, trueStatement, falseStatement)

  def genIfThen: Gen[IfThen] = for {
    cond <- genExpr
    statement <- genStatement
  } yield IfThen(cond, statement)

  def genReturn: Gen[Return] = for {
    expr <- genExpr
  } yield Return(expr)

  def genBreak: Gen[Break] = Break()
  def genContinue: Gen[Continue] = Continue()

  def genBlock: Gen[Block] = for {
    statements <- Gen.listOf(genStatement)
  } yield Block(statements)

  def genExpressionStatement: Gen[ExpressionStatement] = for {
    expr <- genExpr
  } yield ExpressionStatement(expr)

  def genTargetExpr: Gen[Expr] = Gen.oneOf(
    genSymbol, genArrayAccess
  )

  def genVarAssignement: Gen[VarAssignement] = for {
    target <- genTargetExpr
    value <- genExpr
  } yield VarAssignement(target, value)

  def genFunctionDecl: Gen[FunctionDecl] = for {
    header <- genFunctionHeader
    body <- genFunctionBody
  } yield FunctionDecl(header, body)

  def genFunctionHeader: Gen[FunctionHeader] = for {
    retTyp <- genRetTyp
    id <- Gen.identifier
    parameters <- Gen.listOf(genTypedIdentifier)
  } yield FunctionHeader(retTyp, id, parameters)

  def genFunctionBody: Gen[FunctionBody] = for {
    block <- genBlock
  } yield FunctionBody(block)

  def genNewVal: Gen[NewVal] = for {
    symbol <- genSymbol
    typ <- genTyp
    init <- genExpr
  } yield NewVal(symbol, typ, init)

  def genNewVar: Gen[NewVar] = Gen.oneOf(genNewVarNone, genNewVarSome)

  def genNewVarNone: Gen[NewVar] = for {
    symbol <- genSymbol
    typ <- genTyp
  } yield NewVar(symbol, typ, None)

  def genNewVarSome: Gen[NewVar] = for {
    symbol <- genSymbol
    typ <- genTyp
    init <- genExpr
  } yield NewVar(symbol, typ, Some(init))

  def genNewArray: Gen[NewArray] = for {
    symbol <- genSymbol
    typ <- genTfArray
    init <- genArrayInitValue
  } yield NewArray(symbol, typ, init)

  def genArrayInit: Gen[ArrayInit] = Gen.oneOf(genArrayInitValue, genArrayInitCapacity)

  def genArrayInitValue: Gen[ArrayInitValue] = for {
    values <- Gen.listOf(genExpr)
  } yield ArrayInitValue(values)

  def genArrayInitCapacity: Gen[ArrayInitCapacity] = for {
    cap <- genLiteral
  } yield ArrayInitCapacity(cap)
}

