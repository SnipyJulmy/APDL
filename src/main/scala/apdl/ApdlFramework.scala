package apdl

sealed trait ApdlFramework
object ApdlFramework {
  case object Arduino extends ApdlFramework
  case object Mbed extends ApdlFramework

  def values = Seq(Arduino, Mbed)
}
