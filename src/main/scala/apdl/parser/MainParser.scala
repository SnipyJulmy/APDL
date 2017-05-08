package apdl.parser

import java.io.StringWriter

import apdl.parser.parser.{ComponentsParser, TypesParser}
import com.moandjiezana.toml.Toml

import scala.collection.JavaConverters._
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
    val toml = new Toml().read(apdlConfig.mainFile)
    val dependencies = toml.getList[String]("dependencies").asScala.toList
    val projectName = toml.getString("project_name")

    val full = new StringWriter()
    full.append(Source.fromFile(apdlConfig.mainFile).mkString)
    dependencies.foreach { filename =>
      val path = apdlConfig.mainFile.getParentFile.getPath + "/" + filename
      full.append("\n" + Source.fromFile(path).mkString)
    }

    val fullToml = new Toml().read(full.toString)
    val types = TypesParser(fullToml)
    val components = ComponentsParser(fullToml,types)
    components.foreach(println)
    ApdlProject(projectName, dependencies)
  }
}