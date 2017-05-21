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

class ApdlBaseGenerators(maxIdentifierSize: Int = 20) {

  def genPrimitivesTyp: Gen[TfPrimitivesTyp] = Gen.oneOf(
    genTfBoolean, genTfInt, genTfLong, genTfByte,
    genTfShort, genTfChar, genTfDouble, genTfFloat
  )

  def genLiteral: Gen[Literal] = for {
    num <- Gen.choose(0, Double.MaxValue)
  } yield Literal(num.toString)

  def genSymbol: Gen[Symbol] = for {
    id <- genIdentifier
  } yield Symbol(id)

  def genIdentifier: Gen[String] = for {
    l <- Gen.choose(1, maxIdentifierSize)
    c <- Gen.alphaLowerChar
    cs <- Gen.listOfN(l, Gen.alphaNumChar)
  } yield (c :: cs).mkString

  def typGen: Gen[ApdlType] = Gen.oneOf(ApdlType.values)

  def parameterGen: Gen[Parameter] = for {
    id <- genIdentifier
    typ <- typGen
  } yield Parameter(id, typ)

  def genTypedIdentifier: Gen[TypedIdentifier] = for {
    id <- Gen.identifier
    typ <- genTyp
  } yield TypedIdentifier(id, typ)

  def genTyp: Gen[TfTyp] = Gen.lzy(
    Gen.oneOf(
      genTfBoolean, genTfInt, genTfLong, genTfByte,
      genTfShort, genTfChar, genTfDouble, genTfFloat, genTfArray
    )
  )

  def genRetTyp: Gen[TfRetTyp] = Gen.lzy(
    Gen.oneOf(
      genTfBoolean, genTfInt, genTfLong, genTfByte, genTfVoid,
      genTfShort, genTfChar, genTfDouble, genTfFloat, genTfArray
    )
  )

  def genTfBoolean: Gen[TfBoolean.type] = TfBoolean
  def genTfInt: Gen[TfInt.type] = TfInt
  def genTfLong: Gen[TfLong.type] = TfLong
  def genTfByte: Gen[TfByte.type] = TfByte
  def genTfShort: Gen[TfShort.type] = TfShort
  def genTfChar: Gen[TfChar.type] = TfChar
  def genTfDouble: Gen[TfDouble.type] = TfDouble
  def genTfFloat: Gen[TfFloat.type] = TfFloat
  def genTfVoid: Gen[TfVoid.type] = TfVoid

  def genTfArray: Gen[TfArray] = for {
    typ <- genTyp
  } yield TfArray(typ)

  def isValid(l: List[Statement]): Boolean = {
    if (l.length < 2) true
    else {
      val l1 = l.reverse.tail.reverse // drop last
      val l2 = l.tail // drop first
      val crtWithNext = l1 zip l2
      crtWithNext.forall {
        case (Return(_), ExpressionStatement(_)) => false
        case (ExpressionStatement(_), ExpressionStatement(Cast(_, _))) => false
        case _ => true
      }
    }
  }
}

class ApdlExprGenerators(maxExprSize: Int = 4) extends ApdlBaseGenerators {

  def genExpr: Gen[Expr] = genExprInner(maxExprSize)

  private def genExprInner(depth: Int): Gen[Expr] = {
    if (depth == 0) genExprTerminal
    else {
      val nextDepth = depth - 1
      Gen.oneOf(
        genAdd(nextDepth),
        genMul(nextDepth),
        genDiv(nextDepth),
        genSub(nextDepth),
        genCast(nextDepth),
        genOr(nextDepth),
        genAdd(nextDepth),
        genNot(nextDepth),
        genSmaller(nextDepth),
        genSmallerEquals(nextDepth),
        genEquals(nextDepth),
        genNotEquals(nextDepth),
        genGreater(nextDepth),
        genGreaterEquals(nextDepth),
        genFunctionCall(nextDepth),
        genSymbol,
        genLiteral,
        genTrue,
        genFalse
      )
    }
  }

  private def genExprTerminal: Gen[Expr] = Gen.oneOf(
    genLiteral, genSymbol, genTrue, genFalse
  )

  def genAdd(depth: Int): Gen[Add] =
    for {
      e1 <- genExprInner(depth)
      e2 <- genExprInner(depth)
    } yield Add(e1, e2)

  def genMul(depth: Int): Gen[Mul] = Gen.lzy(for {
    e1 <- genExprInner(depth)
    e2 <- genExprInner(depth)
  } yield Mul(e1, e2))

  def genSub(depth: Int): Gen[Sub] = Gen.lzy(for {
    e1 <- genExprInner(depth)
    e2 <- genExprInner(depth)
  } yield Sub(e1, e2))

  def genDiv(depth: Int): Gen[Div] = Gen.lzy(for {
    e1 <- genExprInner(depth)
    e2 <- genExprInner(depth)
  } yield Div(e1, e2))

  def genCast(depth: Int): Gen[Cast] = for {
    typ <- genPrimitivesTyp
    expr <- genExprInner(depth)
  } yield Cast(typ, expr)

  def genFunctionCall(depth: Int): Gen[FunctionCall] = for {
    id <- genIdentifier
    args <- Gen.listOf(genExprInner(depth))
  } yield FunctionCall(id, args)

  def genArrayAccess(depth: Int): Gen[ArrayAccess] = for {
    array <- genExprInner(depth)
    field <- genExprInner(depth)
  } yield ArrayAccess(array, field)

  def genTrue: Gen[True] = True()

  def genFalse: Gen[False] = False()

  def genOr(depth: Int): Gen[Or] = for {
    e1 <- genExprInner(depth)
    e2 <- genExprInner(depth)
  } yield Or(e1, e2)

  def genAnd(depth: Int): Gen[And] = for {
    e1 <- genExprInner(depth)
    e2 <- genExprInner(depth)
  } yield And(e1, e2)

  def genNot(depth: Int): Gen[Not] = for {
    e1 <- genExprInner(depth)
  } yield Not(e1)

  def genGreater(depth: Int): Gen[Greater] = for {
    e1 <- genExprInner(depth)
    e2 <- genExprInner(depth)
  } yield Greater(e1, e2)

  def genSmaller(depth: Int): Gen[Smaller] = for {
    e1 <- genExprInner(depth)
    e2 <- genExprInner(depth)
  } yield Smaller(e1, e2)

  def genGreaterEquals(depth: Int): Gen[GreaterEquals] = for {
    e1 <- genExprInner(depth)
    e2 <- genExprInner(depth)
  } yield GreaterEquals(e1, e2)

  def genSmallerEquals(depth: Int): Gen[SmallerEquals] = for {
    e1 <- genExprInner(depth)
    e2 <- genExprInner(depth)
  } yield SmallerEquals(e1, e2)

  def genEquals(depth: Int): Gen[Equals] = for {
    e1 <- genExprInner(depth)
    e2 <- genExprInner(depth)
  } yield Equals(e1, e2)

  def genNotEquals(depth: Int): Gen[NotEquals] = for {
    e1 <- genExprInner(depth)
    e2 <- genExprInner(depth)
  } yield NotEquals(e1, e2)
}

class ApdlStatementGenerators(maxExprSize: Int = 4, maxBlockSize: Int = 10) extends ApdlExprGenerators(maxExprSize) {

  def genStatement: Gen[Statement] = Gen.oneOf(
    genWhile,
    genDoWhile,
    genIfThen,
    genExpressionStatement,
    genBreak,
    genContinue,
    genReturn
  )

  def genBlock: Gen[Block] = for {
    size <- Gen.choose(1, maxBlockSize)
    statements <- Gen.listOfN(size, genStatement) suchThat (l => isValid(l))
  } yield Block(statements)

  def genExpressionStatement: Gen[ExpressionStatement] = for {
    expr <- genExpr
  } yield ExpressionStatement(expr)

  def genWhile: Gen[While] = for {
    cond <- genExpr
    statement <- Gen.oneOf(genStatement, genBlock)
  } yield While(cond, statement)

  def genDoWhile: Gen[DoWhile] = for {
    cond <- genExpr
    statement <- Gen.oneOf(genStatement, genBlock)
  } yield DoWhile(cond, statement)

  def genIfThenElse: Gen[IfThenElse] = for {
    cond <- genExpr
    trueStatement <- Gen.oneOf(genStatement, genBlock)
    falseStatement <- Gen.oneOf(genStatement, genBlock)
  } yield IfThenElse(cond, trueStatement, falseStatement)

  def genIfThen: Gen[IfThen] = for {
    cond <- genExpr
    statement <- Gen.oneOf(genStatement, genBlock)
  } yield IfThen(cond, statement)

  def genReturn: Gen[Return] = for {
    expr <- genExpr
  } yield Return(expr)

  def genBreak: Gen[Break] = Break()
  def genContinue: Gen[Continue] = Continue()

  def genVarAssignement(depth: Int): Gen[VarAssignement] = for {
    target <- genExpr
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

  def genNewVal(depth: Int): Gen[NewVal] = for {
    symbol <- genSymbol
    typ <- genTyp
    init <- genExpr
  } yield NewVal(symbol, typ, init)

  def genNewVar(depth: Int): Gen[NewVar] = Gen.oneOf(genNewVarNone, genNewVarSome(depth))

  def genNewVarNone: Gen[NewVar] = for {
    symbol <- genSymbol
    typ <- genTyp
  } yield NewVar(symbol, typ, None)

  def genNewVarSome(depth: Int): Gen[NewVar] = for {
    symbol <- genSymbol
    typ <- genTyp
    init <- genExpr
  } yield NewVar(symbol, typ, Some(init))

  def genNewArray(depth: Int): Gen[NewArray] = for {
    symbol <- genSymbol
    typ <- genTfArray
    init <- genArrayInitValue(depth)
  } yield NewArray(symbol, typ, init)

  def genArrayInit(depth: Int): Gen[ArrayInit] = Gen.oneOf(genArrayInitValue(depth), genArrayInitCapacity)

  def genArrayInitValue(depth: Int): Gen[ArrayInitValue] = for {
    values <- Gen.listOf(genExpr)
  } yield ArrayInitValue(values)

  def genArrayInitCapacity: Gen[ArrayInitCapacity] = for {
    cap <- genLiteral
  } yield ArrayInitCapacity(cap)
}

class ApdlDefineGenerator(maxExprSize: Int = 4, maxBlockSize: Int = 10) extends ApdlStatementGenerators(maxExprSize, maxBlockSize) {

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

  def defineTransformGen: Gen[DefineTransform] = for {
    funcDecl <- genFunctionDecl
  } yield DefineTransform(funcDecl)

  /* APDL Transform DSL case class Generator */
}