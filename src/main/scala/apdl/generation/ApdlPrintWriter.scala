package apdl.generation

import java.io.{File, PrintWriter, StringWriter}

import apdl.ApdlFramework
import apdl.ApdlFramework.{Arduino, Mbed}

abstract class ApdlPrintWriter(file: File) {
  require(file.exists())
  require(file.canWrite)

  private[generation] val function = new StringWriter
  private[generation] val global = new StringWriter
  private[generation] val setup = new StringWriter
  private[generation] val loop = new StringWriter

  private[generation] val pw = new PrintWriter(file)

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

case class ApdlArduinoPrintWriter(file: File) extends ApdlPrintWriter(file) {
  val generateTimer: Boolean = true
  val generateSerial: Boolean = true

  override def close(): Unit = {
    pw.append {
      s"""
         |#include <stdbool.h>
         |${if (generateTimer) s"""#include "Timer.h""""}
         |
         |${if (generateTimer) s"Timer t;"}
         |
         |${global.toString}
         |
         |${function.toString}
         |
         |void loop() {
         |  ${if (generateTimer) s"t.update();"}
         |  ${loop.toString}
         |}
         |
         |void setup() {
         |  ${if (generateSerial) s"Serial.begin(9600);"}
         |  ${setup.toString}
         |}
         |
       """.stripMargin
    }
    pw.flush()
    pw.close()
  }
}

case class ApdlMbedPrintWriter(file: File) extends ApdlPrintWriter(file) {
  val generateTimer: Boolean = true
  val generateSerial: Boolean = true

  override def close(): Unit = {
    pw.append {
      s"""
         |#include <stdbool.h>
         |#include <mbed.h>
         |
         |${if (generateTimer) s"Ticker ticker;"}
         |
         |${if (generateSerial) s"Serial pc(USBTX, USBRX);"}
         |
         |${global.toString}
         |
         |${function.toString}
         |
         |int main(void) {
         |  ${if (generateSerial) s"pc.baud(4800);"}
         |  // Setup
         |  ${setup.toString}
         |
         |  // Loop
         |  while(1) {
         |    ${loop.toString}
         |  }
         |  return 0;
         |}
         |
       """.stripMargin
    }
    pw.flush()
    pw.close()
  }
}

object ApdlPrintWriter {
  def getPw(framework: ApdlFramework): (File) => ApdlPrintWriter = framework match {
    case Arduino => ApdlArduinoPrintWriter.apply
    case Mbed => ApdlMbedPrintWriter.apply
  }
}
