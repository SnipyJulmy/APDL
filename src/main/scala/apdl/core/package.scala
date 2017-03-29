package apdl

import java.io.{File, PrintWriter}

/**
  * Created by snipy
  * Project apdl
  */
package object math {
}

package object Utils {

  implicit class ApdlByte(byte: Byte) {
    def cHex: String = "0x%02X" format byte
  }

  implicit def byte2int(int: Int): Byte = int.toByte
}

package object ApdlStreamManager {
  private val directory = new File(s"apdl-gen")
  if (directory.exists()) directory.delete()
  directory.mkdirs()
  private val directoryPath = directory.getAbsolutePath

  // Main stream
  private val main = new File(s"$directoryPath/apdl-gen.ino")
  main.delete()
  private val ApdlMainStream = new PrintWriter(main)
  mainPrintln(s"// APDL MAIN")

  // Header stream
  private val header = new File(s"$directoryPath/apdl-header.h")
  header.delete()
  private val ApdlHeaderStream = new PrintWriter(header)
  headerPrintln(s"// APDL HEADER")

  // Stream for the Loop function from arduino
  private val loop = new File(s"$directoryPath/apdl-loop.h")
  loop.delete()
  private val ApdlLoopStream = new PrintWriter(loop)
  loopPrintln(s"// APDL LOOP")
  loopPrintln(s"timer.update();")

  // Stream for the Setup function from arduino
  private val setup = new File(s"$directoryPath/apdl-setup.h")
  setup.delete()
  private val ApdlSetupStream = new PrintWriter(setup)
  setupPrintln(s"// APDL SETUP")

  // Stream for the function of the Arduino, include after the header
  // for the global variable
  private val function = new File(s"$directoryPath/apdl-fun.h")
  function.delete()
  private val ApdlFunctionStream = new PrintWriter(function)
  functionPrintln(s"// APDL Function")

  private def apdlPrintln(stream: PrintWriter)(s: String): Unit = {
    stream.append(s + "\n").flush()
  }

  def mainPrintln(s: String): Unit = {
    apdlPrintln(ApdlMainStream)(s)
  }

  def headerPrintln(s: String): Unit = {
    apdlPrintln(ApdlHeaderStream)(s)
  }

  def loopPrintln(s: String): Unit = {
    apdlPrintln(ApdlLoopStream)(s)
  }

  def setupPrintln(s: String): Unit = {
    apdlPrintln(ApdlSetupStream)(s)
  }

  def functionPrintln(s : String) : Unit = {
    apdlPrintln(ApdlFunctionStream)(s)
  }

  def apdlHeaderStream = ApdlHeaderStream
  def apdlMainStream = ApdlMainStream
  def apdlLoopStream = ApdlLoopStream
  def apdlSetupStream = ApdlSetupStream
  def apdlFunctionStream = ApdlFunctionStream
}
