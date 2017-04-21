package apdl.parser

sealed trait Entity

sealed trait Server extends Entity
case class InfluxDb(name: String, prop: InfluxDbProperty) extends Server
case class InfluxDbProperty(ip: Ip, port: Port, database: Database)

sealed trait Source extends Entity
case class GenericSource(name: String, id: BoardId, mac: Mac, ip: Ip, inputs: List[ApdlInput], sends: List[Send]) extends Source

case class BoardId(id: String)

sealed trait ApdlInput
case class GenericInput(name: String, typ: ApdlTyp) extends ApdlInput
case class PinInput(name: String, typ: ApdlTyp, pin: Int) extends ApdlInput

sealed trait Sampling
case class UpdateSampling() extends Sampling
case class PeriodicSampling(value: Int, timeUnit: TimeUnit.EnumVal) extends Sampling

object TimeUnit {
  sealed abstract class EnumVal(val valueInMs: BigInt) {
    override def toString: String = this match {
      case MilliSecond => "ms"
      case Second => "s"
      case Minutes => "m"
      case Hours => "h"
      case Day => "d"
    }
  }
  case object MilliSecond extends EnumVal(1)
  case object Second extends EnumVal(1000)
  case object Minutes extends EnumVal(1000 * 60)
  case object Hours extends EnumVal(1000 * 60 * 60)
  case object Day extends EnumVal(1000 * 60 * 60 * 24)
  // (365.25 / 12) * (1000 * 60) = 30.4375 * 60000 = 1826250
  // Is just for remove cast from double to int
}

case class Transformater(function: FunctionDecl) extends Entity

sealed abstract class Send(target: String, sampling: Sampling)
case class GenericSend(target: String, input: String, sampling: Sampling) extends Send(target, sampling)
case class TfSend(target: String, tf: String, input: String, sampling: Sampling) extends Send(target, sampling)

sealed trait ApdlTyp
case object ApdlInt extends ApdlTyp
case object ApdlShort extends ApdlTyp
case object ApdlByte extends ApdlTyp
case object ApdlChar extends ApdlTyp
case object ApdlFloat extends ApdlTyp
case object ApdlDouble extends ApdlTyp
case object ApdlLong extends ApdlTyp
case object ApdlBool extends ApdlTyp

sealed trait Property
case class GenericProperty(key: String, value: String) extends Property
case class Database(name: String) extends Property
case class Ip(address: String) extends Property
case class Port(number: Int) extends Property
case class Mac(address: Seq[String]) extends Property
object Mac {
  def apply(address: String): Mac = new Mac(address.split(":").toSeq)
}
