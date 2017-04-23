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
    server | source | transform | visualisation
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
    pin_input | generic_input
  }

  def generic_input: Parser[GenericInput] = {
    "input" ~ input_name ~ apdl_type ^^ {
      case ("input" ~ name ~ input_type) => GenericInput(name, input_type)
    }
  }

  def pin_input: Parser[PinInput] = {
    "input" ~> input_name ~ apdl_type ~ ("from" ~ "pin" ~> "[0-9]+".r) ^^ {
      case (id ~ typ ~ pin) => PinInput(id, typ, pin.toInt)
    }
  }

  def input_name: Parser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => str }

  def transform: Parser[Transformater] = {
    "transform" ~ tfFunctionDeclaration ^^ { case (_ ~ _tf_def) =>
      if (_tf_def.header.resultType == TfVoid())
        throw new ApdlParserException("void transform aren't supported for the moment")
      Transformater(_tf_def)
    }
  }

  def send: Parser[Send] = generic_send | tf_send
  def generic_send: Parser[GenericSend] = "send" ~ input_name ~ "to" ~ entity_name ~ sampling ^^ {
    case (_ ~ input ~ _ ~ target ~ sampling) =>
      GenericSend(target, input, sampling)
  }

  def tf_send: Parser[TfSend] = "send" ~ identifier ~ input_name ~ "to" ~ entity_name ~ sampling ^^ {
    case (_ ~ tf_name ~ input ~ _ ~ target ~ sampling) =>
      TfSend(target, tf_name, input, sampling)
  }

  def sampling: Parser[Sampling] = periodic_sampling | update_sampling
  def periodic_sampling: Parser[PeriodicSampling] = "each" ~> sampling_value ~ timeunit ^^ {
    case (s ~ t) => PeriodicSampling(s, t)
  }
  def update_sampling: Parser[UpdateSampling] = "on" ~ "update" ^^ {
    _ => UpdateSampling()
  }

  def timeunit: Parser[TimeUnit.EnumVal] = {
    "ms" ^^ { _ => TimeUnit.MilliSecond } |
      "s" ^^ { _ => TimeUnit.Second } |
      "m" ^^ { _ => TimeUnit.Minutes } |
      "h" ^^ { _ => TimeUnit.Hours } |
      "d" ^^ { _ => TimeUnit.Day }
  }

  def sampling_value: Parser[Int] = "[0-9]+".r ^^ {
    _.toInt
  }

  def apdl_type: Parser[ApdlTyp] = int | float | double | long | short | byte | char | bool
  lazy val int: Parser[ApdlInt.type] = "int" ^^ { _ => ApdlInt }
  lazy val float: Parser[ApdlFloat.type] = "float" ^^ { _ => ApdlFloat }
  lazy val double: Parser[ApdlDouble.type] = "double" ^^ { _ => ApdlDouble }
  lazy val long: Parser[ApdlLong.type] = "long" ^^ { _ => ApdlLong }
  lazy val char: Parser[ApdlChar.type] = "char" ^^ { _ => ApdlChar }
  lazy val byte: Parser[ApdlByte.type] = "byte" ^^ { _ => ApdlByte }
  lazy val short: Parser[ApdlShort.type] = "short" ^^ { _ => ApdlShort }
  lazy val bool: Parser[ApdlBool.type] = "bool" ^^ { _ => ApdlBool }

  def board_id: Parser[BoardId] = "\"" ~> "[a-zA-Z0-9_][a-zA-Z0-9_-]*".r <~ "\"" ^^ { case (id) => BoardId(id) }

  def entity_name: Parser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => str }
  def influxdb_property: Parser[InfluxDbProperty] = {
    property_ip ~ property_port ~ property_database ^^ { case (ip ~ port ~ db) => InfluxDbProperty(ip, port, db) } |
      property_ip ~ property_database ~ property_port ^^ { case (ip ~ db ~ port) => InfluxDbProperty(ip, port, db) } |
      property_port ~ property_ip ~ property_database ^^ { case (port ~ ip ~ db) => InfluxDbProperty(ip, port, db) } |
      property_port ~ property_database ~ property_ip ^^ { case (port ~ db ~ ip) => InfluxDbProperty(ip, port, db) } |
      property_database ~ property_ip ~ property_port ^^ { case (db ~ ip ~ port) => InfluxDbProperty(ip, port, db) } |
      property_database ~ property_port ~ property_ip ^^ { case (db ~ port ~ ip) => InfluxDbProperty(ip, port, db) }
  }
  def property_ip: Parser[Ip] = "ip" ~> "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}".r ^^ { ip => Ip(ip) }
  def property_mac: Parser[Mac] = "mac" ~> "([0-9A-Fa-f]{2}[:-]){5}([0-9A-Fa-f]{2})".r ^^ { mac => Mac(mac) }
  def property_port: Parser[Port] = "port" ~> "[0-9]+".r ^^ { port =>
    val portValue = port.toInt
    if (portValue < 1)
      throw new ApdlDslException("Port is to small")
    if (portValue > 65535)
      throw new ApdlDslException("Port is to big")
    Port(portValue)
  }
  def property_database: Parser[Database] = "database" ~ "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { db => Database(db._2) }

  def visualisation: Parser[Visualisation] = graph
  def graph: Parser[Graph] = {
    "graph" ~> identifier ~ ("from" ~> identifier) ~ (":" ~> aggregator ~ plotTypes ~ unitSI ~ min ~ max) ^^ {
      case (dataId ~ sourceId ~ (aggregator ~ plotTypes ~ unit ~ min ~ max)) => Graph(
        dataId,
        sourceId,
        aggregator,
        plotTypes,
        unit,
        min,
        max
      )
    }
  }

  def aggregator: Parser[Aggregator] = {
    "aggregate" ~> aggregateRange ~ aggregateFunction ^^ { case (r ~ f) => Aggregator(r, f) }
  }
  def aggregateRange: Parser[AggregateRange] = {
    lp ~> sampling_value ~ timeunit <~ rp ^^ { case (v ~ t) => AggregateRange(v, t) }
  }
  def aggregateFunction: Parser[AggregateFunction.EnumVal] = {
    lp ~> (average | count | maximum | median | minimum | mode | sum) <~ rp
  }
  def average: Parser[AggregateFunction.EnumVal] = "average" ^^ { _ => AggregateFunction.Average }
  def count: Parser[AggregateFunction.EnumVal] = "count" ^^ { _ => AggregateFunction.Count }
  def maximum: Parser[AggregateFunction.EnumVal] = "maximum" ^^ { _ => AggregateFunction.Maximum }
  def median: Parser[AggregateFunction.EnumVal] = "median" ^^ { _ => AggregateFunction.Median }
  def minimum: Parser[AggregateFunction.EnumVal] = "minimum" ^^ { _ => AggregateFunction.Minimum }
  def mode: Parser[AggregateFunction.EnumVal] = "mode" ^^ { _ => AggregateFunction.Mode }
  def sum: Parser[AggregateFunction.EnumVal] = "sum" ^^ { _ => AggregateFunction.Sum }

  def plotTypes: Parser[List[PlotType.EnumVal]] = "plot" ~> rep(plotType)
  def plotType: Parser[PlotType.EnumVal] = bar | line | point
  def bar: Parser[PlotType.Bar.type] = "bar" ^^ { _ => PlotType.Bar }
  def line: Parser[PlotType.Line.type] = "line" ^^ { _ => PlotType.Line }
  def point: Parser[PlotType.Point.type] = "point" ^^ { _ => PlotType.Point }

  def unitSI: Parser[String] = "unit" ~> "[a-zA-Z0-9_-]+".r ^^ { str => str }
  def min: Parser[Double] = "min" ~> "[-+]?[0-9]+.?[0-9]*".r ^^ { str => str.toDouble }
  def max: Parser[Double] = "max" ~> "[-+]?[0-9]+.?[0-9]*".r ^^ { str => str.toDouble }

  /* Transformater script syntax */

  // Types
  lazy val tfRetType: PackratParser[TfRetTyp] = tfVoid | tfTyp
  lazy val tfTyp: PackratParser[TfTyp] = tfArrayTyp | tfPrimitivesTyp
  lazy val tfPrimitivesTyp: PackratParser[TfPrimitivesTyp] = tfBooleanTyp | tfNumericTyp
  lazy val tfBooleanTyp: PackratParser[TfBoolean] = "bool" ^^ { _ => TfBoolean() }
  lazy val tfNumericTyp: PackratParser[TfNumericTyp] = tfIntegralTyp | tfFloatingPointTyp
  lazy val tfIntegralTyp: PackratParser[TfIntegralTyp] = tfInt | tfShort | tfLong | tfByte | tfChar
  lazy val tfInt: PackratParser[TfInt] = "int" ^^ { _ => TfInt() }
  lazy val tfLong: PackratParser[TfLong] = "long" ^^ { _ => TfLong() }
  lazy val tfByte: PackratParser[TfByte] = "byte" ^^ { _ => TfByte() }
  lazy val tfShort: PackratParser[TfShort] = "short" ^^ { _ => TfShort() }
  lazy val tfChar: PackratParser[TfChar] = "char" ^^ { _ => TfChar() }
  lazy val tfFloatingPointTyp: PackratParser[TfFloatingPointTyp] = tfFloat | tfDouble
  lazy val tfFloat: PackratParser[TfFloat] = "float" ^^ { _ => TfFloat() }
  lazy val tfDouble: PackratParser[TfDouble] = "double" ^^ { _ => TfDouble() }
  lazy val tfArrayTyp: PackratParser[TfArray] = tfTyp <~ "[" ~ "]" ^^ { typ => TfArray(typ) }
  lazy val tfVoid: PackratParser[TfVoid] = "void" ^^ { _ => TfVoid() }

  // Expressions

  lazy val tfConstantExpr: PackratParser[Expr] = tfLogicalOrExpr

  lazy val tfLogicalOrExpr: PackratParser[Expr] = {
    tfLogicalOrExpr ~ ("||" ~> tfLogicalAndExpr) ^^ { case (l ~ r) => Or(l, r) } |
      tfLogicalAndExpr
  }

  lazy val tfLogicalAndExpr: PackratParser[Expr] = {
    tfLogicalAndExpr ~ ("&&" ~> tfEqualityExpr) ^^ { case (l ~ r) => And(l, r) } |
      tfEqualityExpr
  }

  lazy val tfEqualityExpr: PackratParser[Expr] = {
    tfEqualityExpr ~ ("==" ~> tfRelationalExpr) ^^ { case (l ~ r) => Equals(l, r) } |
      tfEqualityExpr ~ ("!=" ~> tfRelationalExpr) ^^ { case (l ~ r) => NotEquals(l, r) } |
      tfRelationalExpr
  }

  lazy val tfRelationalExpr: PackratParser[Expr] = {
    tfRelationalExpr ~ (">" ~> tfAdditiveExpr) ^^ { case (l ~ r) => Greater(l, r) } |
      tfRelationalExpr ~ ("<" ~> tfAdditiveExpr) ^^ { case (l ~ r) => Smaller(l, r) } |
      tfRelationalExpr ~ (">=" ~> tfAdditiveExpr) ^^ { case (l ~ r) => GreaterEquals(l, r) } |
      tfRelationalExpr ~ ("<=" ~> tfAdditiveExpr) ^^ { case (l ~ r) => SmallerEquals(l, r) } |
      tfAdditiveExpr
  }

  lazy val tfAdditiveExpr: PackratParser[Expr] = {
    tfAdditiveExpr ~ ("+" ~> tfMultiplicativeExpr) ^^ { case (l ~ r) => Add(l, r) } |
      tfAdditiveExpr ~ ("-" ~> tfMultiplicativeExpr) ^^ { case (l ~ r) => Sub(l, r) } |
      tfMultiplicativeExpr
  }

  lazy val tfMultiplicativeExpr: PackratParser[Expr] = {
    tfMultiplicativeExpr ~ ("*" ~> tfCastExpr) ^^ { case (l ~ r) => Mul(l, r) } |
      tfMultiplicativeExpr ~ ("/" ~> tfCastExpr) ^^ { case (l ~ r) => Div(l, r) } |
      tfCastExpr
  }

  lazy val tfCastExpr: PackratParser[Expr] = {
    (lp ~> tfPrimitivesTyp <~ rp) ~ tfCastExpr ^^ { case (t ~ expr) => Cast(t, expr) } |
      tfPostfixExpr
  }

  lazy val tfPostfixExpr: PackratParser[Expr] = {
    tfFunctionCall |
      tfArrayAccess |
      tfPrimaryExpr
  }

  lazy val tfPrimaryExpr: PackratParser[Expr] = {
    tfAtom | tfSymbol | tfLiteral | lp ~> tfExpr <~ rp
  }

  lazy val tfArrayAccess: PackratParser[Expr] = {
    identifier ~ rep1("[" ~> tfExpr <~ "]") ^^ { case (id ~ expr) =>
      expr.tail.foldLeft(ArrayAccess(Symbol(id), expr.head))((acc, elt) => ArrayAccess(acc, elt))
    }
  }

  lazy val tfAtom: PackratParser[Expr] = {
    "true" ^^ { _ => True() } |
      "false" ^^ { _ => False() }
  }

  lazy val tfExpr: PackratParser[Expr] = {
    tfAssignExpr
  }

  lazy val tfAssignExpr: PackratParser[Expr] = {
    tfPostfixExpr ~ ("=" ~> tfAssignExpr) ^^ { case (l ~ r) => VarAssignement(l, r) } |
      tfLogicalOrExpr
  }

  lazy val tfSymbol: PackratParser[Symbol] = {
    identifier ^^ { x => Symbol(x) }
  }

  lazy val tfLiteral: PackratParser[Literal] = {
    """[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)""".r ^^ Literal
  }

  lazy val identifier: PackratParser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => str }

  lazy val tfFunctionCall: PackratParser[FunctionCall] =
    identifier ~ lp ~ tfFunctionCallArg ~ rp ^^ {
      case (id ~ _ ~ args ~ _) => FunctionCall(id, args)
    }

  lazy val tfFunctionCallArg: PackratParser[List[Expr]] = {
    (tfExpr ?) ~ rep("," ~ tfExpr) ^^ { exprs =>
      exprs._1 match {
        case Some(value) => value :: exprs._2.map(_._2)
        case None => List()
      }
    }
  }

  lazy val tfArgs: PackratParser[List[TypedIdentifier]] = tfArg ~ rep("," ~> tfArg) ^^ {
    case (a ~ as) => a :: as
  }

  lazy val tfArg: PackratParser[TypedIdentifier] = {
    identifier ~ ":" ~ tfTyp ^^ { case (id ~ _ ~ typ) => TypedIdentifier(id, typ) }
  }

  lazy val tfVarAssign: PackratParser[VarAssignement] = {
    identifier ~ "=" ~ tfConstantExpr ^^ { case (id ~ _ ~ expr) => VarAssignement(Symbol(id), expr) }
  }

  lazy val tfBlock: PackratParser[Block] = lb ~> tfStatements <~ rb ^^ { statements => Block(statements) }

  lazy val tfStatements: PackratParser[List[Statement]] = rep(tfStatement)

  lazy val tfStatement: PackratParser[Statement] = {
    tfBlock | tfSelectionStatement | tfLoop | tfJump | tfDeclaration | tfExprStatement
  }

  lazy val tfExprStatement: PackratParser[ExpressionStatement] = tfExpr ^^ { expr => ExpressionStatement(expr) }

  lazy val tfLoop: PackratParser[Statement] = tfWhile | tfDoWhile
  lazy val tfWhile: PackratParser[While] = "while" ~ lp ~ tfExpr ~ rp ~ tfStatement ^^ {
    case (_ ~ _ ~ cond ~ _ ~ statement) => While(cond, statement)
  }
  lazy val tfDoWhile: PackratParser[DoWhile] = "do" ~ tfStatement ~ "while" ~ lp ~ tfExpr ~ rp ^^ {
    case (_ ~ statement ~ _ ~ _ ~ cond ~ _) => DoWhile(cond, statement)
  }

  lazy val tfDeclaration: PackratParser[Declaration] = {
    tfNewVal | tfNewArray | tfNewVar | tfFunctionDeclaration
  }

  lazy val tfFunctionDeclaration: PackratParser[FunctionDecl] = "def" ~> tfFunctionHeader ~ tfFunctionBody ^^ { case (h ~ b) => FunctionDecl(h, b) }
  lazy val tfFunctionHeader: Parser[FunctionHeader] =
    identifier ~ lp ~ tfArgs ~ rp ~ "->" ~ tfRetType ^^ {
      case (id ~ _ ~ parameters ~ _ ~ "->" ~ ret_type) => FunctionHeader(ret_type, id, parameters)
    } |
      identifier ~ (lp ~ rp ~ "->" ~> tfRetType) ^^ { case (id ~ typ) => FunctionHeader(typ, id, List()) }
  lazy val tfFunctionBody: PackratParser[FunctionBody] = tfBlock ^^ { b => FunctionBody(b) }

  lazy val tfNewArray: PackratParser[NewArray] = {
    "var" ~ tfSymbol ~ ":" ~ tfArrayTyp ~ "=" ~ tfArrayInit ^^ {
      case (_ ~ id ~ _ ~ typ ~ _ ~ init) => NewArray(id, typ, init)
    } |
      "var" ~ tfSymbol ~ ":" ~ tfArrayTyp ^^ { _ => throw new ApdlParserException("Uninitialised array") }
  }

  lazy val tfNewVar: PackratParser[NewVar] = {
    "var" ~ tfSymbol ~ ":" ~ tfPrimitivesTyp ~ "=" ~ tfExpr ^^ {
      case (_ ~ id ~ _ ~ typ ~ _ ~ init) => NewVar(id, typ, Some(init))
    } |
      "var" ~ tfSymbol ~ ":" ~ tfPrimitivesTyp ^^ {
        case (_ ~ id ~ _ ~ typ) => NewVar(id, typ, None)
      }
  }

  lazy val tfArrayInit: PackratParser[ArrayInit] = {
    "[" ~> tfLiteral <~ "]" ^^ ArrayInitCapacity |
      "{" ~> tfExpr ~ rep("," ~> tfExpr) <~ "}" ^^ { es =>
        ArrayInitValue(es._1 :: es._2)
      }
  }

  lazy val tfNewVal: PackratParser[NewVal] = {
    "val" ~ tfSymbol ~ ":" ~ tfPrimitivesTyp ~ "=" ~ tfExpr ^^ {
      case (_ ~ id ~ _ ~ typ ~ _ ~ init) => NewVal(id, typ, init)
    }
  }

  lazy val tfSelectionStatement: PackratParser[Statement] = tf_if_then_else | tfIfThen
  lazy val tf_if_then_else: PackratParser[IfThenElse] = "if" ~ lp ~ tfExpr ~ rp ~ tfStatement ~ "else" ~ tfStatement ^^ {
    case (_ ~ _ ~ cond ~ _ ~ trueBranch ~ _ ~ falseBranch) => IfThenElse(cond, trueBranch, falseBranch)
  }
  lazy val tfIfThen: PackratParser[IfThen] = "if" ~ lp ~ tfExpr ~ rp ~ tfStatement ^^ {
    case (_ ~ _ ~ cond ~ _ ~ statement) => IfThen(cond, statement)
  }

  lazy val tfJump: PackratParser[Statement] = {
    tfBreak | tfContinue | tfReturn
  }

  lazy val tfReturn: PackratParser[Return] = "return" ~> tfConstantExpr ^^ { expr => Return(expr) }
  lazy val tfBreak: PackratParser[Break] = "break" ^^ { _ => Break() }
  lazy val tfContinue: PackratParser[Continue] = "continue" ^^ { _ => Continue() }

  lazy val lp = "("
  lazy val rp = ")"
  lazy val lb = "{"
  lazy val rb = "}"
}
