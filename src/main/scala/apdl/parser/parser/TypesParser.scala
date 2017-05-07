package apdl.parser.parser

import apdl.parser._
import stoml.Toml._
import stoml.TomlParserApi._

object TypesParser {
  def apply(ast: TomlContent): Map[String, TypesParser#ApdlType] = (new TypesParser).parseTypes(ast)
}

class TypesParser {
  private def parseTypes(ast: TomlContent): Map[String, ApdlType] = {

    val types = ast.childOf("type").toList.map {
      case Pair(_) => throw new ApdlFormatException(s"Expected type in an array, not in pair")
      case Table((k, m)) =>
        k.toString -> ApdlType(
          format(m),
          requirements(m),
          declarators(m)
        )
    }
    types.toMap
  }

  def format(m: Map[String, Elem]): String = m(typeValueKey) match {
    case Str(str) => str
    case x => throw new ApdlFormatException(s"Expected string in the value, got ${x.getClass}")
  }

  def requirements(m: Map[String, Elem]): List[String] = m(typeRequireKey) match {
    case Arr(seq) => seq map {
      case Str(str) => str
      case x => throw new ApdlFormatException(s"Expected string in the require array value, got ${x.getClass}")
    } toList
    case x => throw new ApdlFormatException(s"Expected an array for require, got ${x.getClass}")
  }

  def declarators(m: Map[String, Elem]): Map[String, String] = m(typeDeclaratorsKey) match {
    case Arr(seq) => seq.map {
      case Arr(seq2) => // 2 field always
        assert(seq2.length == 2)
        val target = seq2.head match {
          case Str(str) => str
          case x3 => throw new ApdlFormatException(s"Expected a string for declarators key, got ${x3.getClass}")
        }
        val code = seq2.tail.head match {
          case Str(str) => str
          case x3 => throw new ApdlFormatException(s"Expected a string for declarators value, got ${x3.getClass}")
        }
        target -> code
      case x2 => throw new ApdlFormatException(s"Expected an array of array for declarators, got ${x2.getClass}")
    }.toMap
    case x => throw new ApdlFormatException(s"Expected an array for declarators, got ${x.getClass}")
  }

  private val typeValueKey = "value"
  private val typeRequireKey = "require"
  private val typeDeclaratorsKey = "declarators"

  case class ApdlType(format: String, requirement: List[String], declarators: Map[String, String])
}
