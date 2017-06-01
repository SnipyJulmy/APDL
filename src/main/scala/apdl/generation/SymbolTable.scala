package apdl.generation

import apdl.ApdlCodeGenerationException
import apdl.parser.{TfRetTyp, TfTyp}

import scala.collection.mutable

class SymbolTable {
  // TODO refactoring, encapstulation,...
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

  def contains(symbol: String) : Boolean = map.contains(symbol)
}

sealed trait SymbolTableElement
case class Transform(identifier: String, parameters: List[TfTyp], resultType: TfRetTyp) extends SymbolTableElement
case class Component(identifier: String, expr: String) extends SymbolTableElement
case class Input(identifier: String, expr: String) extends SymbolTableElement
