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
  // Main stream
  private val main = new File("apdl-main.c")
  main.delete()
  private val ApdlMainStream = new PrintWriter(main)
  mainPrintln(s"// APDL MAIN")

  // Header stream
  private val header = new File("apdl-gen.h")
  header.delete()
  private val ApdlHeaderStream = new PrintWriter(header)
  headerPrintln(s"// APDL HEADER")

  // Stream for the Loop function from arduino
  private val loop = new File("apdl-loop.h")
  loop.delete()
  private val ApdlLoopStream = new PrintWriter(loop)
  loopPrintln(s"// APDL LOOP")
  loopPrintln(s"timer.update();")

  // Stream for the Setup function from arduino
  private val setup = new File("apdl-setup.h")
  setup.delete()
  private val ApdlSetupStream = new PrintWriter(setup)
  setupPrintln(s"// APDL SETUP")

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

  def apdlHeaderStream = ApdlHeaderStream
  def apdlMainStream = ApdlMainStream
  def apdlLoopStream = ApdlLoopStream
  def apdlSetupStream = ApdlSetupStream
}
