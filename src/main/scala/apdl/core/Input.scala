package apdl.core

// trait for all kind of input
trait Input

case class ArduinoAnalogInput(pin : Int) extends Input
case class ArduinoDigitalInput(pin : Int) extends Input
case class TransformedInput(input: Input, tf : Transformater) extends Input