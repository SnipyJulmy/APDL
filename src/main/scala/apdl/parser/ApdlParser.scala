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

  def tf_def: Parser[TfDef] = {
    tf_name ~ arg_signature ~ "=" ~ tf_body ^^ {
      case (tf_name ~ arg_signature ~ "=" ~ tf_body) =>
        TfDef(tf_name, arg_signature._1, arg_signature._2, tf_body._2)
    }
  }

  def tf_name: Parser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => str }

  def arg_signature: Parser[(List[Arg], TfTyp)] = lp ~ tf_args ~ rp ~ "->" ~ tf_ret_type ^^ {
    case (_ ~ args ~ _ ~ _ ~ ret_type) => (args, ret_type)
  }

  def tf_ret_type: Parser[TfTyp] = tf_typ | lp ~> tf_typ <~ rp

  def tf_typ: Parser[TfTyp] = tf_int | tf_float | tf_long | tf_double | tf_void
  def tf_int: Parser[TfInt] = "int" ^^ { _ => TfInt() }
  def tf_float: Parser[TfFloat] = "float" ^^ { _ => TfFloat() }
  def tf_double: Parser[TfDouble] = "double" ^^ { _ => TfDouble() }
  def tf_long: Parser[TfLong] = "long" ^^ { _ => TfLong() }
  def tf_void: Parser[TfVoid] = "void" ^^ { _ => TfVoid() }

  def tf_args: Parser[List[Arg]] = tf_arg ~ rep("," ~ tf_arg) ^^ { x => x._1 :: x._2.map(_._2) }
  def tf_arg: Parser[Arg] = tf_identifier ~ ":" ~ tf_typ ^^ { case (id ~ _ ~ typ) => Arg(id, typ) }
  def tf_identifier: Parser[String] = tf_name
  def tf_body: Parser[(List[Statement], Expr)] = tf_return ^^ { x => (List(), x) }
  def tf_statements: Parser[List[Statement]] = rep(tf_statement)
  def tf_statement: Parser[Statement] = tf_new_val
  def tf_return: Parser[Expr] = tf_expr
  def tf_new_val: Parser[TfNewVal] = "val" ~ tf_identifier ~ ":" ~ tf_typ ~ "=" ~ tf_expr ^^ {
    case (_ ~ id ~ _ ~ typ ~ _ ~ init) => TfNewVal(id, typ, init)
  }

  def lp = "("
  def rp = ")"
  def send: Parser[Send] = generic_send | tf_send
  def generic_send: Parser[GenericSend] = "send" ~ "to" ~ entity_name ~ input_name ~ sampling_value ^^ {
    case ("send" ~ "to" ~ target ~ input ~ sampling) =>
      GenericSend(target, input, sampling)
  }

  def tf_send: Parser[TfSend] = "send" ~ "to" ~ entity_name ~ tf_name ~ input_name ~ sampling_value ^^ {
    case ("send" ~ "to" ~ entity_name ~ tf_name ~ input ~ sampling_value) =>
      TfSend(entity_name, tf_name, input, sampling_value)
  }

  def sampling_value: Parser[Int] = "[0-9]+".r ^^ {
    _.toInt
  }

  def apdl_type: Parser[Typ] = _int | _float | _double | _long
  def _int: Parser[int] = "int" ^^ { _ => int() }
  def _float: Parser[float] = "float" ^^ { _ => float() }
  def _double: Parser[double] = "double" ^^ { _ => double() }
  def _long: Parser[long] = "long" ^^ { _ => long() }

  def board_id: Parser[BoardId] = "\"" ~ "[a-z0-9_]+".r ~ "\"" ^^ { case ("\"" ~ id ~ "\"") => BoardId(id) }

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

  // Expr

  def tf_expr: Parser[Expr] = tf_op1
  def tf_op1: Parser[Expr] = {
    tf_p2 ~ rep("+" ~ tf_p2) ^^ {
      case op ~ list => list.foldLeft(op) {
        case (x, "+" ~ y) => Add(x, y)
      }
    } |
      tf_p2 ~ rep("-" ~ tf_p2) ^^ {
        case op ~ list => list.foldLeft(op) {
          case (x, "-" ~ y) => Add(x, y)
        }
      }
  }

  def tf_p2: Parser[Expr] = {
    operand ~ rep("*" ~ operand) ^^ {
      case op ~ list => list.foldLeft(op) {
        case (x, "*" ~ y) => Mul(x, y)
      }
    } |
      operand ~ rep("/" ~ operand) ^^ {
        case op ~ list => list.foldLeft(op) {
          case (x, "/" ~ y) => Div(x, y)
        }
      }
  }

  def operand: Parser[Expr] = literal | symbol | parExpr
  def symbol: Parser[Symbol] = tf_identifier ^^ { x => Symbol(x) }
  def parExpr: Parser[Expr] = "(" ~> tf_expr <~ ")"
  def number: Parser[Number] = """[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)""".r ^^ (value => Number(value))
  def literal: Parser[Literal] = number ^^ Literal
}




















