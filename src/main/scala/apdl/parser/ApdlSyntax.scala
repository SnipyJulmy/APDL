package apdl.parser

sealed trait Entity

sealed trait Server extends Entity
case class InfluxDb(name: String, prop: InfluxDbProperty) extends Server
case class InfluxDbProperty(ip: Ip, port: Port, database : Database)

sealed trait Source extends Entity
case class GenericSource(name: String, id: BoardId, mac: Mac, ip: Ip, inputs: List[ApdlInput], sends: List[Send]) extends Source

case class BoardId(id: String)

sealed trait ApdlInput
case class GenericInput(name: String, typ: Typ) extends ApdlInput

case class Transformater(function: TfDef) extends Entity

sealed trait Send
case class GenericSend(target: String, input: String, sampling: Int) extends Send
case class TfSend(target: String, tf: String, input: String, sampling: Int) extends Send

sealed trait Typ
case class int() extends Typ
case class float() extends Typ
case class double() extends Typ
case class long() extends Typ

sealed trait Property
case class GenericProperty(key: String, value: String) extends Property
case class Database(name : String) extends Property
case class Ip(address: Seq[Int]) extends Property {
  override def toString: String = {
    s"""Ip("${address mkString ","}")"""
  }
}
object Ip {
  def apply(address: String): Ip = {
    new Ip(address.split("\\.").map(_.toInt).toSeq)
  }
}
case class Port(number: Int) extends Property
case class Mac(address: Seq[String]) extends Property
object Mac {
  def apply(address: String): Mac = new Mac(address.split(":").toSeq)
}
