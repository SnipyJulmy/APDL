package apdl.generation

import apdl.ApdlCodeGenerationException
import apdl.parser._

import scala.collection.mutable

class SymbolTable {
  val map: mutable.Map[String, SymbolTableElement] = mutable.Map()

  def add(symbol: String, elt: SymbolTableElement): Unit = {
    map.put(symbol, elt)
  }

  def getOption(symbol: String): Option[SymbolTableElement] = {
    map.get(symbol)
  }

  def get(symbol: String): SymbolTableElement = {
    getOption(symbol) match {
      case Some(value) => value
      case None => throw new ApdlCodeGenerationException(s"Unknow symbol $symbol")
    }
  }

  def contains(symbol: String): Boolean = map.contains(symbol)
}

sealed trait SymbolTableElement

case class Component(identifier: String, outputType: ApdlType, parameters: List[Parameter]) extends SymbolTableElement
case class Transform(functionDecl: FunctionDecl) extends SymbolTableElement

sealed trait Input extends SymbolTableElement
case class InputDefault(identifier: String, definition : ApdlDefineInput, args : List[String])
case class InputTransformed(identifier: String, definition: ApdlDefineTransform, input : Input)
case class InputComponented(identifier: String, definition : ApdlDefineComponent, inputs : List[Input])
