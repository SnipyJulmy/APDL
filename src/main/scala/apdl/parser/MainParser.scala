package apdl.parser

import java.io.StringWriter

import apdl.parser.parser.{ComponentsParser, TypesParser}
import fastparse.core.Parsed.{Failure, Success}
import stoml.Toml._
import stoml.TomlParserApi._

import scala.io.Source

case class ApdlProject(name: String,
                       dependencies: List[String])

class MainParser(apdlConfig: ApdlConfig) {
  /*
    1. Recover project name
    2. Recover dependencies
    3. Create the big file with dependencies
   */

  def parseFile(): ApdlProject = {
    val mainAst = parseToml(apdlConfig.mainFile) match {
      case Success(value, _) => value

      case failure: Failure[_, _] => throw new ApdlParserException(s"failed parse ${apdlConfig.mainFile.getPath} : $failure")
    }
    val project = ApdlProject(
      parseProjectName(mainAst),
      parseDependencies(mainAst)
    )

    val full = new StringWriter()
    full.append(Source.fromFile(apdlConfig.mainFile).mkString)
    project.dependencies.foreach { filename =>
      val path = apdlConfig.mainFile.getParentFile.getPath + "/" + filename
      full.append(Source.fromFile(path).mkString)
    }
    // The full toml ast
    val ast = parseToml(full.toString) match {
      case Success(value, _) => value
      case failure: Failure[_, _] => throw new ApdlParserException(s"failed parse full generated file : $failure")
    }
    println(ast)
    val types = TypesParser(ast)
    val components = ComponentsParser(ast)
    project
  }

  private def parseProjectName(ast: TomlContent): String = ast.lookup("project_name") match {
    case Some(value) => value match {
      case Pair((_, pairValue)) => pairValue match {
        case Str(projectName) => projectName
        case a => throw new ApdlFormatException(s"Expected string, found ${a.getClass}")
      }
      case Table(_) => throw new ApdlFormatException("Found table for project_name key, expected string")
    }
    case None => throw new ApdlFormatException("No project_name specified")
  }

  private def parseDependencies(ast: TomlContent): List[String] = {
    val dependencies: List[String] = ast.lookup("dependencies") match {
      case Some(value) => value match {
        case Pair((_, pairValue)) => pairValue match {
          case Arr(elems) => elems.map {
            case Str(str) => str
            case _ => throw new ApdlFormatException("Dependencies parameters type mismatch, expected string")
          }.toList
          case _ => throw new ApdlFormatException("Dependencies info ins't in an array")
        }
        case Table(_) => throw new ApdlFormatException("Found table, expected a pair")
      }
      case None => List()
    }
    dependencies
  }



}