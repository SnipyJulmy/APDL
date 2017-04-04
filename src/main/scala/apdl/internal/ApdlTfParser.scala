package apdl.internal

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class ApdlTfParser extends RegexParsers {

  override protected val whiteSpace: Regex = "[ \t\r\f\n]+".r

  override def skipWhitespace: Boolean = true

  def _def: Parser[Def] = "def" ~ _ident ~ _funArgs ~ ":" ~ _typ ~ "=" ~ "{" ~ _functionBody ~ "}" ^^ {
    case ("def" ~ id ~ args ~ ":" ~ typ ~ "=" ~ "{" ~ body ~ "}") => Def(id, args, typ, body)
  }
  def _functionBody: Parser[FunctionBody] = {
    rep(_statement) ~ _expr ^^ { x => FunctionBody(x._1, x._2) }
  }
  def _statement: Parser[Statement] = _def | _newVal
  def _newVal: Parser[NewVal] = "val" ~ _ident ~ ":" ~ _typ ~ "=" ~ _expr ^^ {
    case ("val" ~ id ~ ":" ~ typ ~ "=" ~ expr) => NewVal(id, expr, typ)
  }
  def _funArgs: Parser[List[Arg]] = "(" ~> rep(_arg) <~ ")"
  def _arg: Parser[Arg] = _ident ~ ":" ~ _typ ^^ { case (x ~ ":" ~ y) => Arg(x, y) }
  def _typ: Parser[Typ] = _int | _float | _double | _long
  def _int: Parser[TypInt] = "Int" ^^ { _ => TypInt() }
  def _float: Parser[TypFloat] = "Float" ^^ { _ => TypFloat() }
  def _long: Parser[TypLong] = "Long" ^^ { _ => TypLong() }
  def _double: Parser[TypDouble] = "Double" ^^ { _ => TypDouble() }
  def _number: Parser[Number] = """[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)""".r ^^ (value => Number(value))
  def _literal: Parser[Literal] = _number ^^ Literal
  def _symbol: Parser[Symbol] = _ident ^^ { x => Symbol(x) }
  def _ident: Parser[Identifier] = "[a-zA-Z][a-zA-Z0-9]*".r ^^ { x => Identifier(x) }
  def _operand: Parser[Expr] = _literal | _symbol | _parExpr
  def _parExpr: Parser[Expr] = "(" ~ _expr ~ ")" ^^ { x => x._1._2 }
  def _expr: Parser[Expr] = _op1
  def _op1: Parser[Expr] = {
    _op2 ~ rep("+" ~ _op2) ^^ {
      case op ~ list => list.foldLeft(op) {
        case (x, "+" ~ y) => Add(x, y)
      }
    } |
      _op2 ~ rep("-" ~ _op2) ^^ {
        case op ~ list => list.foldLeft(op) {
          case (x, "-" ~ y) => Add(x, y)
        }
      }
  }

  def _op2: Parser[Expr] = {
    _operand ~ rep("*" ~ _operand) ^^ {
      case op ~ list => list.foldLeft(op) {
        case (x, "*" ~ y) => Mul(x, y)
      }
    } |
      _operand ~ rep("/" ~ _operand) ^^ {
        case op ~ list => list.foldLeft(op) {
          case (x, "/" ~ y) => Div(x, y)
        }
      }
  }
}
