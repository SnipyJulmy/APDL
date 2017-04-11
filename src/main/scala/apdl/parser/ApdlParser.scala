package apdl.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

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
    "transform" ~ tf_def ^^ { case (_ ~ tf_def) => Transformater(tf_def) }
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
  def tf_ret_type: Parser[TfRetTyp] = tf_void | tf_typ
  def tf_typ: Parser[TfTyp] = tf_primitives_typ | tf_array_typ
  def tf_primitives_typ: Parser[TfPrimitivesTyp] = tf_boolean_typ | tf_numeric_typ
  def tf_boolean_typ: Parser[TfBoolean] = "bool" ^^ { _ => TfBoolean() }
  def tf_numeric_typ: Parser[TfNumericTyp] = tf_integral_typ | tf_floating_point_typ
  def tf_integral_typ: Parser[TfIntegralTyp] = tf_int | tf_short | tf_long | tf_byte | tf_char
  def tf_int: Parser[TfInt] = "int" ^^ { _ => TfInt() }
  def tf_long: Parser[TfLong] = "long" ^^ { _ => TfLong() }
  def tf_byte: Parser[TfByte] = "byte" ^^ { _ => TfByte() }
  def tf_short: Parser[TfShort] = "short" ^^ { _ => TfShort() }
  def tf_char: Parser[TfChar] = "char" ^^ { _ => TfChar() }
  def tf_floating_point_typ: Parser[TfFloatingPointTyp] = tf_float | tf_double
  def tf_float: Parser[TfFloat] = "float" ^^ { _ => TfFloat() }
  def tf_double: Parser[TfDouble] = "double" ^^ { _ => TfDouble() }
  def tf_array_typ: Parser[TfArray] = tf_typ <~ "[" ~ "]" ^^ { typ => TfArray(typ) }
  def tf_void: Parser[TfVoid] = "void" ^^ { _ => TfVoid() }

  // Expressions

  def tf_expr: Parser[Expr] = {
    tf_term ~ "+" ~ tf_expr ^^ { case (l ~ _ ~ r) => Add(l, r) } |
      tf_term ~ "-" ~ tf_expr ^^ { case (l ~ _ ~ r) => Sub(l, r) } |
      tf_term
  }

  // TODO explain in report evaluation order
  def tf_term: Parser[Expr] = {
    tf_factor ~ "*" ~ tf_term ^^ { case (l ~ _ ~ r) => Mul(l, r) } |
      tf_factor ~ "/" ~ tf_term ^^ { case (l ~ _ ~ r) => Div(l, r) } |
      tf_factor
  }

  def tf_factor: Parser[Expr] = literal | symbol | parExpr

  def tf_function_call: Parser[FunctionCall] = {
    tf_identifier ~ lp ~ tf_function_call_arg ~ rp ^^ {
      case (id ~ _ ~ args ~ _) => FunctionCall(id, args)
    }
  }

  def tf_function_call_arg: Parser[List[Expr]] = (tf_expr ?) ~ rep("," ~ tf_expr) ^^ { exprs =>
    exprs._1 match {
      case Some(value) => value :: exprs._2.map(_._2)
      case None => List()
    }
  }

  def symbol: Parser[Symbol] = tf_identifier ^^ { x => Symbol(x) }
  def parExpr: Parser[Expr] = "(" ~> tf_expr <~ ")"
  def number: Parser[Number] = """[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)""".r ^^ (value => Number(value))
  def literal: Parser[Literal] = number ^^ Literal

  // Primitives
  def tf_identifier: Parser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => str }

  def tf_args: Parser[List[TypedIdentifier]] = tf_arg ~ rep("," ~ tf_arg) ^^ { x => x._1 :: x._2.map(_._2) }
  def tf_arg: Parser[TypedIdentifier] = {
    tf_identifier ~ ":" ~ tf_primitives_typ ^^ { case (id ~ _ ~ typ) => TypedIdentifier(id, typ) }
  }

  def tf_body: Parser[(List[Declaration], List[Statement], Return)] = {
    lb ~> tf_return <~ rb ^^ { ret => (Nil, Nil, ret) } |
      lb ~> rep(tf_decl | tf_statement) ~ tf_return <~ rb ^^ { body =>
        val declarations = body._1.filter(_.isInstanceOf[Declaration]).map(_.asInstanceOf[Declaration])
        val statements = body._1.filter(_.isInstanceOf[Statement]).map(_.asInstanceOf[Statement])
        (declarations, statements, body._2)
      }
  }

  def tf_block: Parser[Block] = lb ~> tf_statements <~ rb ^^ { statements => Block(statements) }
  def tf_statements: Parser[List[Statement]] = {
    tf_statement ~ (rep(tf_statement) ?) ^^ { statements =>
      statements._2 match {
        case Some(value) => statements._1 :: value
        case None => statements._1 :: Nil
      }
    }
  }

  def tf_assign: Parser[Assignement] = tf_var_assign | tf_array_assign
  def tf_var_assign: Parser[VarAssignement] = tf_identifier ~ "=" ~ tf_expr ^^ { case (id ~ _ ~ expr) => VarAssignement(id, expr) }
  def tf_array_assign: Parser[ArrayAssignement] = {
    tf_identifier ~ "[" ~ tf_expr ~ "]" ~ "=" ~ tf_expr ^^ { case (id ~ _ ~ field ~ _ ~ _ ~ expr) => ArrayAssignement(id, field, expr) }
  }

  def tf_statement: Parser[Statement] = {
    tf_new_val | tf_assign | tf_block | tf_loop | tf_jump | tf_expr_statement
  }
  def tf_expr_statement: Parser[ExpressionStatement] = tf_expr ^^ { expr => ExpressionStatement(expr) }
  def tf_loop: Parser[Statement] = tf_while | tf_dowhile
  def tf_while: Parser[While] = "while" ~ lp ~ tf_boolean_expr ~ rp ~ tf_statement ^^ {
    case (_ ~ _ ~ cond ~ _ ~ statement) => While(cond, statement)
  }
  def tf_dowhile: Parser[DoWhile] = "do" ~ tf_statement ~ "while" ~ lp ~ tf_boolean_expr ~ rp ^^ {
    case (_ ~ statement ~ _ ~ _ ~ cond ~ _) => DoWhile(cond, statement)
  }

  def tf_decl: Parser[Declaration] = {
    tf_new_val | tf_new_var | tf_def
  }

  def tf_def: Parser[FunctionDecl] = "def" ~> tf_def_header ~ tf_def_body ^^ { case (h ~ b) => FunctionDecl(h, b) }
  def tf_def_header: Parser[FunctionHeader] = tf_identifier ~ lp ~ tf_args ~ rp ~ "->" ~ tf_ret_type ^^ {
    case (id ~ _ ~ parameters ~ _ ~ "->" ~ ret_type) => FunctionHeader(ret_type, id, parameters)
  }
  def tf_def_body: Parser[FunctionBody] = tf_block ^^ { b => FunctionBody(b) }

  def tf_new_var: Parser[NewVar] = {
    "var" ~ tf_identifier ~ ":" ~ tf_primitives_typ ~ "=" ~ tf_expr ^^ {
      case (_ ~ id ~ _ ~ typ ~ _ ~ init) => NewVar(id, typ, Some(init))
    } |
      "var" ~ tf_identifier ~ ":" ~ tf_primitives_typ ^^ {
        case (_ ~ id ~ _ ~ typ) => NewVar(id, typ, None)
      }
  }
  def tf_new_val: Parser[NewVal] = {
    "val" ~ tf_identifier ~ ":" ~ tf_primitives_typ ~ "=" ~ tf_expr ^^ {
      case (_ ~ id ~ _ ~ typ ~ _ ~ init) => NewVal(id, typ, init)
    }
  }

  def tf_selection_statement: Parser[Statement] = tf_if_then_else | tf_if_then
  def tf_if_then_else: Parser[IfThenElse] = "if" ~ lp ~ tf_boolean_expr ~ rp ~ tf_statement ~ "else" ~ tf_statement ^^ {
    case (_ ~ _ ~ cond ~ _ ~ trueBranch ~ _ ~ falseBranch) => IfThenElse(cond, trueBranch, falseBranch)
  }
  def tf_if_then: Parser[IfThen] = "if" ~ lp ~ tf_boolean_expr ~ rp ~ tf_statement ^^ {
    case (_ ~ _ ~ cond ~ _ ~ statement) => IfThen(cond, statement)
  }

  def tf_jump: Parser[Statement] = {
    tf_break | tf_continue | tf_return
  }
  def tf_return: Parser[Return] = "return" ~ tf_expr ^^ { expr => Return(expr._2) }
  def tf_break: Parser[Break] = "break" ^^ { _ => Break() }
  def tf_continue: Parser[Continue] = "continue" ^^ { _ => Continue() }

  def lp = "("
  def rp = ")"
  def lb = "{"
  def rb = "}"


  def tf_boolean_expr: Parser[BooleanExpr] = tf_par_boolean_expr | tf_boolean_op1
  def tf_par_boolean_expr: Parser[BooleanExpr] = lp ~> tf_boolean_expr <~ rp

  def tf_boolean_op1: Parser[BooleanExpr] = {
    tf_boolean_op2 ~ rep("||" ~ tf_boolean_op2) ^^ {
      case op ~ list => list.foldLeft(op) {
        case (x, "||" ~ y) => Or(x, y)
      }
    }
  }

  def tf_boolean_op2: Parser[BooleanExpr] = {
    bool_operand ~ rep("&&" ~ bool_operand) ^^ {
      case op ~ list => list.foldLeft(op) {
        case (x, "&&" ~ y) => And(x, y)
      }
    }
  }

  def bool_operand: Parser[BooleanExpr] = bool_not | bool_literal | bool_symbol | tf_par_boolean_expr

  def bool_literal: Parser[BooleanExpr] = tf_true | tf_false
  def bool_not: Parser[Not] = "!" ~ tf_boolean_expr ^^ { bool => Not(bool._2) }
  def bool_symbol: Parser[BooleanSymbol] = tf_identifier ^^ { id => BooleanSymbol(id) }

  def tf_true: Parser[True] = "true" ^^ { _ => True() }
  def tf_false: Parser[False] = "false" ^^ { _ => False() }


}
