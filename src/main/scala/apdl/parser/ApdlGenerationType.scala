package apdl.parser

object ApdlGenerationType extends Enumeration {
  type ApdlGenerationType = Value
  val Debug,Production = Value
  def default = Debug
}
