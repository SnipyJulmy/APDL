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

sealed trait TfTyp extends ApdlTfSyntax
case class TfInt() extends TfTyp
case class TfFloat() extends TfTyp
case class TfLong() extends TfTyp
case class TfDouble() extends TfTyp
case class TfVoid() extends TfTyp

case class Arg(name: String, typ: TfTyp) extends ApdlTfSyntax

sealed trait Statement extends ApdlTfSyntax
// case class TfDef(name: String, args: List[Arg], retType: TfTyp, statements: List[Statement], ret: Expr) extends Statement
case class TfDef(name: String, args: List[Arg], retType: TfTyp,statements : List[Statement] ,ret: Expr) extends Statement
case class TfNewVal(name : String, typ : TfTyp, init : Expr) extends Statement


// TODO : new val, const, etc...
