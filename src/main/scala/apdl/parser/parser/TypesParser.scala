package apdl.parser.parser

import com.moandjiezana.toml.Toml
import scala.collection.JavaConverters._

object TypesParser {
  private val typeValueKey = "value"
  private val typeRequireKey = "require"
  private val typeDeclaratorsKey = "declarators"

  def apply(toml: Toml): Map[String, ApdlType] = (new TypesParser).parseTypes(toml)
}

class TypesParser {

  import TypesParser._

  private def parseTypes(toml: Toml): Map[String, ApdlType] = {
    val types = toml.getTable("type")
    val keys = types.toMap.keySet().asScala.toList
    keys.map { k =>
      val value = types.getTable(k).getString(typeValueKey)
      val require = types.getTable(k).getList[String](typeRequireKey).asScala.toList
      val decl = types.getTable(k).getList[java.util.ArrayList[String]](typeDeclaratorsKey).asScala.toList
      val declarators = decl.map { arl =>
        val l = arl.asScala.toList
        assert(l.length == 2)
        l.head -> l.tail.head
      }.toMap
      k -> ApdlType(value, require, declarators)
    }.toMap
  }
}

case class ApdlType(format: String, requirement: List[String], declarators: Map[String, String])
