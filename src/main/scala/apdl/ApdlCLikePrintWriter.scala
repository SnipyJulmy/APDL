package apdl

import java.io.{File, PrintWriter, StringWriter}

class ApdlCLikePrintWriter(file: File) {
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
    pw.append(
      s"""
         | // Function
         | ${function.toString}
         | // Loop
         | ${loop.toString}
         | // Setup
         | ${setup.toString}
         | // Global
         | ${global.toString}
       """.stripMargin)
    pw.flush()
    pw.close()
  }
}
