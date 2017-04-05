package apdl.core

sealed trait ApdlTokens

sealed trait Expr extends ApdlTokens
case class Add(left: Expr, right: Expr) extends Expr
case class Mul(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
case class Literal(number: Number) extends Expr
case class Constant(identifier: Identifier) extends Expr
case class Symbol(identifier: Identifier) extends Expr
case class Number(value: String) extends Expr
case class Identifier(name: String) extends Expr
case class FunctionCall() extends Expr

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

sealed trait Typ extends ApdlTokens
case class TypInt() extends Typ
case class TypFloat() extends Typ
case class TypLong() extends Typ
case class TypDouble() extends Typ

case class Arg(identifier: Identifier, typ: Typ) extends ApdlTokens

sealed trait Statement extends ApdlTokens
case class Def(identifier: Identifier, args: List[Arg], retType: Typ, body: FunctionBody) extends Statement
case class NewVal(identifier: Identifier, init: Expr, typ: Typ) extends Statement

case class FunctionBody(statements: List[Statement], ret: Expr) extends ApdlTokens