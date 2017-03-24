package apdl

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
