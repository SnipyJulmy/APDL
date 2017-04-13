package apdl.parser

import scala.language.postfixOps
import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

//noinspection ForwardReference
class ApdlParser extends RegexParsers with PackratParsers {

  override protected val whiteSpace: Regex = "[ \t\r\f\n]+".r
  override def skipWhitespace: Boolean = true

  def program: Parser[List[Entity]] = entity ~ rep(entity) ^^ { x => x._1 :: x._2 }

  def entity: Parser[Entity] = {
    server | source | transform
  }
  def server: Parser[Server] = influxdb
  def influxdb: Parser[InfluxDb] = {
    "influxdb" ~ entity_name ~ ":" ~ influxdb_property ^^ { case ("influxdb" ~ name ~ ":" ~ prop) => InfluxDb(name, prop) }
  }

  def source: Parser[Source] = generic_source
  def generic_source: Parser[GenericSource] = "source" ~ entity_name ~ board_id ~ ":" ~ rep(property_ip | property_mac | input | send) ^^ {
    case ("source" ~ entity_name ~ board_id ~ ":" ~ xs) =>
      GenericSource(
        entity_name,
        board_id,
        xs.find(o => o.isInstanceOf[Mac]) match {
          case Some(value) => value.asInstanceOf[Mac]
          case None => throw new ApdlParserException(s"Mac property for $entity_name entity not set")
        },
        xs.find(o => o.isInstanceOf[Ip]) match {
          case Some(value) => value.asInstanceOf[Ip]
          case None => throw new ApdlParserException(s"IP property for $entity_name entity not set")
        },
        xs.filter(o => o.isInstanceOf[ApdlInput]).map(o => o.asInstanceOf[ApdlInput]),
        xs.filter(o => o.isInstanceOf[Send]).map(o => o.asInstanceOf[Send])
      )
  }

  def input: Parser[ApdlInput] = {
    generic_input
  }
  def generic_input: Parser[GenericInput] = {
    "input" ~ input_name ~ apdl_type ^^ {
      case ("input" ~ name ~ input_type) => GenericInput(name, input_type)
    }
  }

  def input_name: Parser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => str }

  def transform: Parser[Transformater] = {
    "transform" ~ tf_def ^^ { case (_ ~ _tf_def) => Transformater(_tf_def) }
  }


  def send: Parser[Send] = generic_send | tf_send
  def generic_send: Parser[GenericSend] = "send" ~ "to" ~ entity_name ~ input_name ~ sampling_value ^^ {
    case ("send" ~ "to" ~ target ~ input ~ sampling) =>
      GenericSend(target, input, sampling)
  }

  def tf_send: Parser[TfSend] = "send" ~ "to" ~ entity_name ~ tf_identifier ~ input_name ~ sampling_value ^^ {
    case ("send" ~ "to" ~ entity_name ~ tf_name ~ input ~ sampling_value) =>
      TfSend(entity_name, tf_name, input, sampling_value)
  }

  def sampling_value: Parser[Int] = "[0-9]+".r ^^ {
    _.toInt
  }

  def apdl_type: Parser[ApdlTyp] = _int | _float | _double | _long
  def _int: Parser[ApdlInt] = "int" ^^ { _ => ApdlInt() }
  def _float: Parser[ApdlFloat] = "float" ^^ { _ => ApdlFloat() }
  def _double: Parser[ApdlDouble] = "double" ^^ { _ => ApdlDouble() }
  def _long: Parser[ApdlLong] = "long" ^^ { _ => ApdlLong() }

  def board_id: Parser[BoardId] = "\"" ~ "[a-z0-9_][a-z0-9_]*".r ~ "\"" ^^ { case ("\"" ~ id ~ "\"") => BoardId(id) }

  def entity_name: Parser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => str }
  def influxdb_property: Parser[InfluxDbProperty] = {
    property_ip ~ property_port ~ property_database ^^ { case (ip ~ port ~ db) => InfluxDbProperty(ip, port, db) } |
      property_ip ~ property_database ~ property_port ^^ { case (ip ~ db ~ port) => InfluxDbProperty(ip, port, db) } |
      property_port ~ property_ip ~ property_database ^^ { case (port ~ ip ~ db) => InfluxDbProperty(ip, port, db) } |
      property_port ~ property_database ~ property_ip ^^ { case (port ~ db ~ ip) => InfluxDbProperty(ip, port, db) } |
      property_database ~ property_ip ~ property_port ^^ { case (db ~ ip ~ port) => InfluxDbProperty(ip, port, db) } |
      property_database ~ property_port ~ property_ip ^^ { case (db ~ port ~ ip) => InfluxDbProperty(ip, port, db) }
  }
  def property_ip: Parser[Ip] = "ip" ~ "([0-9]{1,3}.){3}.([0-9]{1,3})".r ^^ { ip => Ip(ip._2) }
  def property_mac: Parser[Mac] = "mac" ~ "([0-9A-Fa-f]{2}[:-]){5}([0-9A-Fa-f]{2})".r ^^ { mac => Mac(mac._2) }
  def property_port: Parser[Port] = "port" ~ "[0-9]+".r ^^ { port => Port(port._2.toInt) }
  def property_database: Parser[Database] = "database" ~ "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { db => Database(db._2) }

  def property_key: Parser[String] = "[a-z_][a-z_]*".r ^^ { str => str }
  def property_value: Parser[String] = "[a-zA-Z0-9]+".r ^^ { str => str }

  /* Transformater script syntax */

  // Types
  lazy val tf_ret_type: PackratParser[TfRetTyp] = tf_void | tf_typ
  lazy val tf_typ: PackratParser[TfTyp] = tf_array_typ | tf_primitives_typ
  lazy val tf_primitives_typ: PackratParser[TfPrimitivesTyp] = tf_boolean_typ | tf_numeric_typ
  lazy val tf_boolean_typ: PackratParser[TfBoolean] = "bool" ^^ { _ => TfBoolean() }
  lazy val tf_numeric_typ: PackratParser[TfNumericTyp] = tf_integral_typ | tf_floating_point_typ
  lazy val tf_integral_typ: PackratParser[TfIntegralTyp] = tf_int | tf_short | tf_long | tf_byte | tf_char
  lazy val tf_int: PackratParser[TfInt] = "int" ^^ { _ => TfInt() }
  lazy val tf_long: PackratParser[TfLong] = "long" ^^ { _ => TfLong() }
  lazy val tf_byte: PackratParser[TfByte] = "byte" ^^ { _ => TfByte() }
  lazy val tf_short: PackratParser[TfShort] = "short" ^^ { _ => TfShort() }
  lazy val tf_char: PackratParser[TfChar] = "char" ^^ { _ => TfChar() }
  lazy val tf_floating_point_typ: PackratParser[TfFloatingPointTyp] = tf_float | tf_double
  lazy val tf_float: PackratParser[TfFloat] = "float" ^^ { _ => TfFloat() }
  lazy val tf_double: PackratParser[TfDouble] = "double" ^^ { _ => TfDouble() }
  lazy val tf_array_typ: PackratParser[TfArray] = tf_typ <~ "[" ~ "]" ^^ { typ => TfArray(typ) }
  lazy val tf_void: PackratParser[TfVoid] = "void" ^^ { _ => TfVoid() }

  // Expressions

  lazy val tf_constant_expr: PackratParser[Expr] = tf_logical_or_expr

  lazy val tf_logical_or_expr: PackratParser[Expr] = {
    tf_logical_or_expr ~ ("||" ~> tf_logical_and_expr) ^^ { case (l ~ r) => Or(l, r) } |
      tf_logical_and_expr
  }

  lazy val tf_logical_and_expr: PackratParser[Expr] = {
    tf_logical_and_expr ~ ("&&" ~> tf_equality_expr) ^^ { case (l ~ r) => And(l, r) } |
      tf_equality_expr
  }

  lazy val tf_equality_expr: PackratParser[Expr] = {
    tf_equality_expr ~ ("==" ~> tf_relational_expr) ^^ { case (l ~ r) => Equals(l, r) } |
      tf_equality_expr ~ ("!=" ~> tf_relational_expr) ^^ { case (l ~ r) => NotEquals(l, r) } |
      tf_relational_expr
  }

  lazy val tf_relational_expr: PackratParser[Expr] = {
    tf_relational_expr ~ (">" ~> tf_additive_expr) ^^ { case (l ~ r) => Greater(l, r) } |
      tf_relational_expr ~ ("<" ~> tf_additive_expr) ^^ { case (l ~ r) => Smaller(l, r) } |
      tf_relational_expr ~ (">=" ~> tf_additive_expr) ^^ { case (l ~ r) => GreaterEquals(l, r) } |
      tf_relational_expr ~ ("<=" ~> tf_additive_expr) ^^ { case (l ~ r) => SmallerEquals(l, r) } |
      tf_additive_expr
  }

  lazy val tf_additive_expr: PackratParser[Expr] = {
    tf_additive_expr ~ ("+" ~> tf_multiplicative_expr) ^^ { case (l ~ r) => Add(l, r) } |
      tf_additive_expr ~ ("-" ~> tf_multiplicative_expr) ^^ { case (l ~ r) => Sub(l, r) } |
      tf_multiplicative_expr
  }

  lazy val tf_multiplicative_expr: PackratParser[Expr] = {
    tf_multiplicative_expr ~ ("*" ~> tf_postfix_expr) ^^ { case (l ~ r) => Mul(l, r) } |
      tf_multiplicative_expr ~ ("/" ~> tf_postfix_expr) ^^ { case (l ~ r) => Div(l, r) } |
      tf_postfix_expr
  }

  lazy val tf_postfix_expr: PackratParser[Expr] = {
    tf_function_call |
      tf_array_assign |
      tf_primary_expr
  }

  lazy val tf_primary_expr: PackratParser[Expr] = {
    tf_atom | tf_symbol | tf_literal | lp ~> tf_expr <~ rp
  }

  lazy val tf_atom : PackratParser[Expr] = {
    "true" ^^ {_ => True()} |
      "false" ^^ {_ => False()}
  }

  lazy val tf_expr: PackratParser[Expr] = {
    tf_assign_expr
  }

  lazy val tf_assign_expr: PackratParser[Expr] = {
    tf_postfix_expr ~ ("=" ~> tf_assign_expr) ^^ { case (l ~ r) => VarAssignement(l, r) } |
      tf_logical_or_expr
  }

  lazy val tf_symbol: PackratParser[Symbol] = {
    tf_identifier ^^ { x => Symbol(x) }
  }

  lazy val tf_literal: PackratParser[Literal] = {
    """[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)""".r ^^ Literal
  }

  lazy val tf_identifier: PackratParser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => str }

  lazy val tf_function_call: PackratParser[FunctionCall] =
    tf_identifier ~ lp ~ tf_function_call_arg ~ rp ^^ {
      case (id ~ _ ~ args ~ _) => FunctionCall(id, args)
    }

  lazy val tf_function_call_arg: PackratParser[List[Expr]] = {
    (tf_expr ?) ~ rep("," ~ tf_expr) ^^ { exprs =>
      exprs._1 match {
        case Some(value) => value :: exprs._2.map(_._2)
        case None => List()
      }
    }
  }

  lazy val tf_args: PackratParser[List[TypedIdentifier]] = tf_arg ~ rep("," ~ tf_arg) ^^ { x => x._1 :: x._2.map(_._2) }
  lazy val tf_arg: PackratParser[TypedIdentifier] = {
    tf_identifier ~ ":" ~ tf_primitives_typ ^^ { case (id ~ _ ~ typ) => TypedIdentifier(id, typ) }
  }

  lazy val tf_body: PackratParser[(List[Declaration], List[Statement], Return)] = {
    lb ~> tf_return <~ rb ^^ { ret => (Nil, Nil, ret) } |
      lb ~> rep(tf_decl | tf_statement) ~ tf_return <~ rb ^^ { body =>
        val declarations = body._1.filter(_.isInstanceOf[Declaration]).map(_.asInstanceOf[Declaration])
        val statements = body._1.filterNot(_.isInstanceOf[Declaration])
        (declarations, statements, body._2)
      }
  }


  lazy val tf_assign: PackratParser[Assignement] = tf_var_assign | tf_array_assign
  lazy val tf_var_assign: PackratParser[VarAssignement] = {
    tf_identifier ~ "=" ~ tf_constant_expr ^^ { case (id ~ _ ~ expr) => VarAssignement(Symbol(id), expr) }
  }
  lazy val tf_array_assign: PackratParser[ArrayAssignement] = {
    tf_identifier ~ "[" ~ tf_expr ~ "]" ~ "=" ~ tf_constant_expr ^^ {
      case (id ~ _ ~ field ~ _ ~ _ ~ expr) => ArrayAssignement(Symbol(id), field, expr)
    }
  }

  lazy val tf_block: PackratParser[Block] = lb ~> tf_statements <~ rb ^^ { statements => Block(statements) }

  lazy val tf_statements: PackratParser[List[Statement]] = tf_statement ~ (rep(tf_statement) ?) ^^ { statements =>
    statements._2 match {
      case Some(value) => statements._1 :: value
      case None => statements._1 :: Nil
    }
  }

  lazy val tf_statement: PackratParser[Statement] = {
    tf_block | tf_selection_statement | tf_loop | tf_jump | tf_decl | tf_assign  |tf_expr_statement
  }

  lazy val tf_expr_statement: PackratParser[ExpressionStatement] = tf_expr ^^ { expr => ExpressionStatement(expr) }

  lazy val tf_loop: PackratParser[Statement] = tf_while | tf_dowhile
  lazy val tf_while: PackratParser[While] = "while" ~ lp ~ tf_expr ~ rp ~ tf_statement ^^ {
    case (_ ~ _ ~ cond ~ _ ~ statement) => While(cond, statement)
  }
  lazy val tf_dowhile: PackratParser[DoWhile] = "do" ~ tf_statement ~ "while" ~ lp ~ tf_expr ~ rp ^^ {
    case (_ ~ statement ~ _ ~ _ ~ cond ~ _) => DoWhile(cond, statement)
  }

  lazy val tf_decl: PackratParser[Declaration] = {
    tf_new_val | tf_new_var | tf_def
  }

  lazy val tf_def: PackratParser[FunctionDecl] = "def" ~> tf_def_header ~ tf_def_body ^^ { case (h ~ b) => FunctionDecl(h, b) }
  lazy val tf_def_header: Parser[FunctionHeader] = tf_identifier ~ lp ~ tf_args ~ rp ~ "->" ~ tf_ret_type ^^ {
    case (id ~ _ ~ parameters ~ _ ~ "->" ~ ret_type) => FunctionHeader(ret_type, id, parameters)
  }
  lazy val tf_def_body: PackratParser[FunctionBody] = tf_block ^^ { b => FunctionBody(b) }

  lazy val tf_new_var: PackratParser[NewVar] = {
    "var" ~ tf_identifier ~ ":" ~ tf_primitives_typ ~ "=" ~ tf_expr ^^ {
      case (_ ~ id ~ _ ~ typ ~ _ ~ init) => NewVar(id, typ, Some(init))
    } |
      "var" ~ tf_identifier ~ ":" ~ tf_primitives_typ ^^ {
        case (_ ~ id ~ _ ~ typ) => NewVar(id, typ, None)
      }
  }
  lazy val tf_new_val: PackratParser[NewVal] = {
    "val" ~ tf_identifier ~ ":" ~ tf_primitives_typ ~ "=" ~ tf_expr ^^ {
      case (_ ~ id ~ _ ~ typ ~ _ ~ init) => NewVal(id, typ, init)
    }
  }

  lazy val tf_selection_statement: PackratParser[Statement] = tf_if_then_else | tf_if_then
  lazy val tf_if_then_else: PackratParser[IfThenElse] = "if" ~ lp ~ tf_expr ~ rp ~ tf_statement ~ "else" ~ tf_statement ^^ {
    case (_ ~ _ ~ cond ~ _ ~ trueBranch ~ _ ~ falseBranch) => IfThenElse(cond, trueBranch, falseBranch)
  }
  lazy val tf_if_then: PackratParser[IfThen] = "if" ~ lp ~ tf_expr ~ rp ~ tf_statement ^^ {
    case (_ ~ _ ~ cond ~ _ ~ statement) => IfThen(cond, statement)
  }

  lazy val tf_jump: PackratParser[Statement] = {
    tf_break | tf_continue | tf_return
  }

  lazy val tf_return: PackratParser[Return] = "return" ~> tf_constant_expr ^^ { expr => Return(expr) }
  lazy val tf_break: PackratParser[Break] = "break" ^^ { _ => Break() }
  lazy val tf_continue: PackratParser[Continue] = "continue" ^^ { _ => Continue() }

  lazy val lp = "("
  lazy val rp = ")"
  lazy val lb = "{"
  lazy val rb = "}"
}
