package apdl.parser

sealed trait Expr

case class Add(left: Expr, right: Expr) extends Expr
case class Mul(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr

case class Literal(number: Number) extends Expr
case class Symbol(name: String) extends Expr
case class Number(value: String) extends Expr
case class FunctionCall(funcName: String, args: List[Expr]) extends Expr

sealed trait BooleanExpr extends Expr
case class True() extends BooleanExpr
case class False() extends BooleanExpr
case class Or(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
case class And(left: BooleanExpr, right: BooleanExpr) extends BooleanExpr
case class Not(booleanExpr: BooleanExpr) extends BooleanExpr
case class BooleanSymbol(name: String) extends BooleanExpr
case class Greater(left: Expr, right: Expr) extends BooleanExpr
case class Smaller(left: Expr, right: Expr) extends BooleanExpr
case class GreaterEquals(left: Expr, right: Expr) extends BooleanExpr
case class SmallerEquals(left: Expr, right: Expr) extends BooleanExpr
case class Equals(left: Expr, right: Expr) extends BooleanExpr
case class NotEquals(left: Expr, right: Expr) extends BooleanExpr

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

sealed trait Declaration extends Statement
case class FunctionDecl(header: FunctionHeader, body: FunctionBody) extends Declaration
case class FunctionHeader(resultType: TfRetTyp, identifier: String, parameters: List[TypedIdentifier])
case class FunctionBody(body: Block)

case class NewVal(identifier: String, typ: TfTyp, init: Expr) extends Declaration
case class NewVar(identifier: String, typ: TfTyp, init: Option[Expr]) extends Declaration

case class NewArray(identifier: String, typ: TfArray, init: ArrayInit) extends Declaration
case class ArrayInit(values: List[TypedIdentifier])

sealed trait Statement
case class ExpressionStatement(expression: Expr) extends Statement
case class While(cond: BooleanExpr, statement: Statement) extends Statement
case class DoWhile(cond: BooleanExpr, statement: Statement) extends Statement
case class IfThenElse(cond: BooleanExpr, trueBranch: Statement, falseBranch: Statement) extends Statement
case class IfThen(cond: BooleanExpr, ifTrue: Statement) extends Statement
case class Return(expr: Expr) extends Statement
case class Break() extends Statement
case class Continue() extends Statement
case class Block(statements: List[Statement]) extends Statement

sealed trait Assignement extends Statement
case class VarAssignement(name: String, value: Expr) extends Assignement
case class ArrayAssignement(name: String, field: Expr, value: Expr) extends Assignement

// Companion object

object Add {
  def apply(l: AnyVal, r: AnyVal): Add = new Add(Literal(Number(l.toString)), Literal(Number(r.toString)))
  def apply(l: String, r: AnyVal): Add = new Add(Symbol(l), Literal(Number(r.toString)))
  def apply(l: AnyVal, r: String): Add = new Add(Literal(Number(l.toString)), Symbol(r))
  def apply(l: Expr, r: AnyVal): Add = new Add(l, Literal(Number(r.toString)))
  def apply(l: AnyVal, r: Expr): Add = new Add(Literal(Number(l.toString)), r)
  def apply(l: Expr, r: String): Add = new Add(l, Symbol(r))
  def apply(l: String, r: Expr): Add = new Add(Symbol(l), r)
  def apply(l: String, r: String): Add = new Add(Symbol(l), Symbol(r))
}

object Mul {
  def apply(l: AnyVal, r: AnyVal): Mul = new Mul(Literal(Number(l.toString)), Literal(Number(r.toString)))
  def apply(l: String, r: AnyVal): Mul = new Mul(Symbol(l), Literal(Number(r.toString)))
  def apply(l: AnyVal, r: String): Mul = new Mul(Literal(Number(l.toString)), Symbol(r))
  def apply(l: Expr, r: AnyVal): Mul = new Mul(l, Literal(Number(r.toString)))
  def apply(l: AnyVal, r: Expr): Mul = new Mul(Literal(Number(l.toString)), r)
  def apply(l: Expr, r: String): Mul = new Mul(l, Symbol(r))
  def apply(l: String, r: Expr): Mul = new Mul(Symbol(l), r)
  def apply(l: String, r: String): Mul = new Mul(Symbol(l), Symbol(r))
}

object Sub {
  def apply(l: AnyVal, r: AnyVal): Sub = new Sub(Literal(Number(l.toString)), Literal(Number(r.toString)))
  def apply(l: String, r: AnyVal): Sub = new Sub(Symbol(l), Literal(Number(r.toString)))
  def apply(l: AnyVal, r: String): Sub = new Sub(Literal(Number(l.toString)), Symbol(r))
  def apply(l: Expr, r: AnyVal): Sub = new Sub(l, Literal(Number(r.toString)))
  def apply(l: AnyVal, r: Expr): Sub = new Sub(Literal(Number(l.toString)), r)
  def apply(l: Expr, r: String): Sub = new Sub(l, Symbol(r))
  def apply(l: String, r: Expr): Sub = new Sub(Symbol(l), r)
  def apply(l: String, r: String): Sub = new Sub(Symbol(l), Symbol(r))
}

object Div {
  def apply(l: AnyVal, r: AnyVal): Div = new Div(Literal(Number(l.toString)), Literal(Number(r.toString)))
  def apply(l: String, r: AnyVal): Div = new Div(Symbol(l), Literal(Number(r.toString)))
  def apply(l: AnyVal, r: String): Div = new Div(Literal(Number(l.toString)), Symbol(r))
  def apply(l: Expr, r: AnyVal): Div = new Div(l, Literal(Number(r.toString)))
  def apply(l: AnyVal, r: Expr): Div = new Div(Literal(Number(l.toString)), r)
  def apply(l: Expr, r: String): Div = new Div(l, Symbol(r))
  def apply(l: String, r: Expr): Div = new Div(Symbol(l), r)
  def apply(l: String, r: String): Div = new Div(Symbol(l), Symbol(r))
}
