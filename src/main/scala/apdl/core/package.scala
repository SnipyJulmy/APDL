package apdl

import java.io.{File, PrintWriter}

/**
  * Created by snipy
  * Project apdl
  */
package object math {
  def math_log(i: Int) : Double = scala.math.log(i)
  def math_log(i: Float) : Double = scala.math.log(i)
  def math_log(i: Double) : Double = scala.math.log(i)
  def math_log(i: Long) : Double = scala.math.log(i)
}

package object Utils {
  implicit class ApdlByte(byte: Byte) {
    def cHex : String = "0x%02X" format byte
  }

  implicit def byte2int(int : Int) : Byte = int.toByte
}

package object ApdlOutputStream {
  private val main = new File("apdl-main.ino")
  main.delete()
  private val ApdlMainStream = new PrintWriter(main)

  private val header = new File("apdl-gen.h")
  header.delete()
  private val ApdlHeaderStream = new PrintWriter(header)

  def headerPrintln(s : String) : Unit = {
    ApdlHeaderStream.append(s).flush()
  }

  def headerStream = ApdlHeaderStream
  def mainStream = ApdlMainStream
}