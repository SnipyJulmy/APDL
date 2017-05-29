package apdl

import java.io.{File, PrintWriter, StringWriter}

import apdl.ApdlUtils._
import apdl.parser.{ApdlDevice, ApdlProject, ApdlType}

import scala.collection.mutable

class ApdlPw(file: File) {
  require(file.exists())
  require(file.canWrite)

  private val function = new StringWriter
  private val global = new StringWriter
  private val setup = new StringWriter
  private val loop = new StringWriter

  val pw = new PrintWriter(file)

  def printlnFunction(str: String): Unit = {
    printFunction(s"$str\n")
  }

  def printFunction(str: String): Unit = {
    function.append(str)
    function.flush()
  }

  def printlnGlobal(str: String): Unit = {
    printGlobal(s"$str\n")
  }

  def printGlobal(str: String): Unit = {
    global.append(str)
    global.flush()
  }

  def printlnSetup(str: String): Unit = {
    printSetup(s"$str\n")
  }

  def printSetup(str: String): Unit = {
    setup.append(str)
    setup.flush()
  }

  def printlnLoop(str: String): Unit = {
    printLoop(s"$str\n")
  }

  def printLoop(str: String): Unit = {
    loop.append(str)
    loop.flush()
  }

  def close(): Unit = {
    pw.flush()
    pw.close()
  }
}

class CLikeCodeGenerator(project: ApdlProject, device: ApdlDevice)(implicit val debugEnable: Boolean) {

  def mkDevice(srcDir: File): Unit = {
    val ext = fileExtension(device.framework)
    val mainFile = new File(srcDir, s"${project.name}_name.$ext")
    if (!mainFile.createNewFile())
      throw new ApdlDirectoryException(s"Can't create file ${mainFile.getAbsolutePath}")
    debug(s"create file ${mainFile.getAbsolutePath}")
    implicit val mainPw = new ApdlPw(mainFile)
    generateInputs(mainPw)
    generateSerial(mainPw)
  }

  def fileExtension(framework: String): String = framework match {
    case "arduino" => "ino"
    case "mbed" => "c"
    case _ => throw new ApdlCodeGenerationException(s"Unknow framework : $framework")
  }

  def generateInputs(out: ApdlPw): Unit = device.inputs.foreach { input =>
    val defineInput = project.defineInputs
      .find(_.name == input.defineInputName)
      .getOrElse(throw new ApdlCodeGenerationException(s"Unknow define input : ${input.defineInputName}"))

    val gen = defineInput.gens.getOrElse(
      device.framework,
      throw new ApdlCodeGenerationException(s"Unknow framework for device ${device.name} : ${device.framework}")
    )

    // identifier variable
    implicit val symbols: mutable.Map[String, ApdlType] = mutable.Map("@id" -> ApdlType.Id)
    // input parameters variable
    defineInput.parameters.foreach { p =>
      symbols.put(s"@${p.id}", p.typ)
    }

    // Generate the code

  }

  private def replaceVariable(str: String, symbols: Map[String, ApdlType]): String = {
    ""
  }

  def generateSerial(out: ApdlPw): Unit = {

  }
}

object IdGenerator {
  var itr: Long = 0

  def nextVariable(id: String): String = {
    itr = itr + 1
    s"${id}_$itr"
  }
}