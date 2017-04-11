package apdl.parser

sealed trait ApdlTfSyntax

sealed trait Expr extends ApdlTfSyntax
case class Add(left: Expr, right: Expr) extends Expr
case class Mul(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
case class Literal(number: Number) extends Expr
case class Constant(name: String) extends Expr
case class Symbol(name: String) extends Expr
case class Number(value: String) extends Expr
case class FunctionCall(funcName: String, args: List[Expr]) extends Expr

sealed trait Assignement extends Statement
case class VarAssignement(name: String, value: Expr) extends Assignement
case class ArrayAssignement(name : String, field : Expr, value : Expr) extends Assignement

sealed trait BooleanExpr extends Expr
case class True() extends BooleanExpr
case class False() extends BooleanExpr
case class Or(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
case class And(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
case class Not(booleanExpr: BooleanExpr) extends BooleanExpr
case class BooleanSymbol(name: String) extends BooleanExpr

sealed trait MathFunction extends Expr
case class Log(expr: Expr) extends MathFunction
case class Log10(expr: Expr) extends MathFunction
case class Sin(expr: Expr) extends MathFunction
case class Cos(expr: Expr) extends MathFunction
case class Tan(expr: Expr) extends MathFunction
case class Asin(expr: Expr) extends MathFunction
case class Acos(expr: Expr) extends MathFunction
case class Atan(expr: Expr) extends MathFunction
case class Exp(expr: Expr) extends MathFunction
case class Sqrt(expr: Expr) extends MathFunction
case class Ceil(expr: Expr) extends MathFunction
case class Floor(expr: Expr) extends MathFunction
case class Pow(n: Expr, power: Expr) extends MathFunction
case class Round(expr: Expr) extends MathFunction
case class Abs(expr: Expr) extends MathFunction
case class Max(left: Expr, right: Expr) extends MathFunction
case class Min(left: Expr, right: Expr) extends MathFunction
case class Tanh(expr: Expr) extends MathFunction
case class Sinh(expr: Expr) extends MathFunction
case class Cosh(expr: Expr) extends MathFunction

sealed trait TfRetTyp
sealed trait TfTyp extends TfRetTyp
sealed trait TfPrimitivesTyp extends TfTyp
sealed trait TfNumericTyp extends TfPrimitivesTyp
sealed trait TfIntegralTyp extends TfNumericTyp
sealed trait TfFloatingPointTyp extends TfNumericTyp
case class TfBoolean() extends TfPrimitivesTyp
case class TfInt() extends TfIntegralTyp
case class TfLong() extends TfIntegralTyp
case class TfByte() extends TfIntegralTyp
case class TfShort() extends TfIntegralTyp
case class TfChar() extends TfIntegralTyp
case class TfDouble() extends TfFloatingPointTyp
case class TfFloat() extends TfFloatingPointTyp
case class TfArray(typ: TfTyp) extends TfTyp
case class TfVoid() extends TfRetTyp

case class TypedIdentifier(name: String, typ: TfPrimitivesTyp)

sealed trait Declaration extends ApdlTfSyntax
case class FunctionDecl(header: FunctionHeader, body: FunctionBody) extends Declaration
case class FunctionHeader(resultType: TfRetTyp, identifier: String, parameters: List[TypedIdentifier])
case class FunctionBody(body: Block)

case class NewVal(identifier: String, typ: TfTyp, init: Expr) extends Declaration
case class NewVar(identifier: String, typ: TfTyp, init: Option[Expr]) extends Declaration
case class NewArray(identifier: String, typ: TfArray, init: ArrayInit) extends Declaration
case class ArrayInit(values : List[TypedIdentifier])


sealed trait Statement extends ApdlTfSyntax
case class ExpressionStatement(expression: Expr) extends Statement
case class While(cond: BooleanExpr, statement: Statement) extends Statement
case class DoWhile(cond: BooleanExpr, statement: Statement) extends Statement
case class IfThenElse(cond: BooleanExpr, trueBranch: Statement, falseBranch: Statement) extends Statement
case class IfThen(cond: BooleanExpr, ifTrue: Statement) extends Statement
case class Return(expr: Expr) extends Statement
case class Break() extends Statement
case class Continue() extends Statement
case class Block(statements: List[Statement]) extends Statement
