package apdl.parser

import apdl.parser.ApdlType.{Id, Num, Str}

import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

class DefineParsers extends RegexParsers with PackratParsers {

  override protected val whiteSpace: Regex = "[ \t\r\f\n]+".r
  override def skipWhitespace: Boolean = true
  val ws: Regex = whiteSpace

  lazy val defines: PackratParser[List[Define]] = rep(define)
  lazy val define: PackratParser[Define] = "@define" ~> (defineComponent | defineInput)

  lazy val defineComponent: PackratParser[DefineComponent] = {
    "component" ~> identifier ~ parameters ~ lb ~ defineComponentBody ~ rb ^^ {
      case (i ~ params ~ _ ~ body ~ _) => DefineComponent(i, params, body._1, body._2, body._3)
    }
  }

  lazy val defineComponentBody: PackratParser[(List[Parameter], ApdlType, Map[String, Gen])] = {
    inputs ~ output ~ gens ^^ { case (i ~ o ~ g) => (i, o, g) }
  }

  lazy val inputs: PackratParser[List[Parameter]] = "@in" ~> parameters
  lazy val output: PackratParser[ApdlType] = "@out" ~> apdlType
  lazy val gens: PackratParser[Map[String, Gen]] = rep(gen) ^^ { gs => gs.toMap }
  lazy val gen: PackratParser[(String, Gen)] = "@gen" ~> identifier ~ (lb ~> genBody) <~ rb ^^ {
    case (i ~ b) => i -> b
  }

  lazy val genBody: PackratParser[Gen] = {
    global ~ setup ~ loop ~ expr ^^ { case (g ~ s ~ l ~ e) => Gen(g, s, l, e) }
  }

  lazy val global: PackratParser[String] = "global" ~ "=" ~ "\"" ~> literalString <~ "\"" ^^ { str => str }
  lazy val setup: PackratParser[String] = "setup" ~ "=" ~ "\"" ~> literalString <~ "\"" ^^ { str => str }
  lazy val loop: PackratParser[String] = "loop" ~ "=" ~ "\"" ~> literalString <~ "\"" ^^ { str => str }
  lazy val expr: PackratParser[String] = "expr" ~ "=" ~ "\"" ~> literalString <~ "\"" ^^ { str => str }
  lazy val literalString: PackratParser[String] = """(\\.|[^\\"])*""".r ^^ { str => str }

  lazy val defineInput: PackratParser[DefineInput] = "input" ~> identifier ~ parameters ~ (lb ~> gens <~ rb) ^^ {
    case (defId ~ defParams ~ defGens) => DefineInput(defId, defParams, defGens)
  }

  lazy val apdlType: PackratParser[ApdlType] = num | str | id
  lazy val num: PackratParser[ApdlType.Num.type] = "num" ^^^ ApdlType.Num
  lazy val str: PackratParser[ApdlType.Str.type] = "str" ^^^ ApdlType.Str
  lazy val id: PackratParser[ApdlType.Id.type] = "id" ^^^ ApdlType.Id

  lazy val identifier: PackratParser[String] = "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => str }
  lazy val parameters: PackratParser[List[Parameter]] = rep(parameter)
  lazy val parameter: PackratParser[Parameter] = identifier ~ (":" ~> apdlType) ^^ { case (i ~ t) => Parameter(i, t) }
  lazy val lb = "{"
  lazy val rb = "}"

  lazy val number: PackratParser[String] = "[-+]?[0-9]+.?[0-9]*".r ^^ { str => str }
}

case class Inputs(parameters: List[Parameter])
case class Output(outputType: ApdlType)
case class Gen(global: String, setup: String, loop: String, expr: String)

sealed trait Define
case class DefineInput(name: String, parameters: List[Parameter], gens: Map[String, Gen]) extends Define
case class DefineComponent(name: String, parameters: List[Parameter], inputs: List[Parameter], outputType: ApdlType, gens: Map[String, Gen]) extends Define

case class Parameter(id: String, typ: ApdlType)

sealed trait ApdlType {
  override def toString: String = this match {
    case Num => "num"
    case Str => "str"
    case Id => "id"
  }
}

object ApdlType {
  // any number : 0.47, 3, 13E14,...
  case object Num extends ApdlType
  // any string : _AS)D SA)D,...
  case object Str extends ApdlType
  // any valid identifier : _id, asAdASDsa, id_ad_ASDS_ad
  case object Id extends ApdlType

  def values : Seq[ApdlType] = Seq(Num,Str,Id)
}
