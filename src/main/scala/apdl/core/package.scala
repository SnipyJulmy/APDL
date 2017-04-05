package apdl

import scala.language.implicitConversions

package object Utils {
  // implicit conversion
  implicit def int2byte(int: Int): Byte = int.toByte
  implicit class ApdlByte(byte: Byte) {
    def cHex: String = "0x%02X" format byte
  }
}
