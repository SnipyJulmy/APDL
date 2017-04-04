package apdl.internal

sealed trait Expr {
  override def toString: String = this match {
    case Add(left, right) => s"($left + $right)"
    case Mul(left, right) => s"($left * $right)"
    case Sub(left, right) => s"($left - $right)"
    case Div(left, right) => s"($left / $right)"
    case Literal(number) => s" $number "
    case Constant(identifier) => s" $identifier "
    case Symbol(identifier) => s" $identifier "
    case Number(value) => s" $value "
    case Identifier(name) => s" $name "
  }
}
case class Add(left: Expr, right: Expr) extends Expr
case class Mul(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
case class Literal(number: Number) extends Expr
case class Constant(identifier: Identifier) extends Expr
case class Symbol(identifier: Identifier) extends Expr
case class Number(value: String) extends Expr
case class Identifier(name: String) extends Expr

sealed trait Typ {
  override def toString: String = this match {
    case TypInt() => "int"
    case TypFloat() => "float"
    case TypLong() => "long"
    case TypDouble() => "double"
  }
}
case class TypInt() extends Typ
case class TypFloat() extends Typ
case class TypLong() extends Typ
case class TypDouble() extends Typ

case class Arg(identifier: Identifier, typ: Typ) {
  override def toString: String = s" $typ $identifier "
}

sealed trait Statement {
  override def toString: String = this match {
    case Def(identifier, args, retType, body) => s"$retType $identifier(${args.mkString(",")}) {$body}"
    case NewVal(identifier, init, typ) => s"$typ $identifier = $init;"
  }
}
case class Def(identifier: Identifier, args: List[Arg], retType: Typ, body: FunctionBody) extends Statement
case class NewVal(identifier: Identifier, init: Expr, typ: Typ) extends Statement
case class FunctionBody(statements: List[Statement], ret: Expr) {
  override def toString: String = {
    s"${statements.mkString(";\n")};\nreturn $ret;"
  }
}