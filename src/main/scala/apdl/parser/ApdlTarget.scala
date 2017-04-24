package apdl.parser

object ApdlTarget extends Enumeration {
  type ApdlTarget = Value
  val Arduino = Value
  def default : ApdlTarget = Arduino
}
