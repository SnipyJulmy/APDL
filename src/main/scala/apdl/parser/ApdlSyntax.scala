package apdl.parser

sealed trait Entity

sealed trait Server extends Entity
case class InfluxDb(name: String, prop: InfluxDbProperty) extends Server
case class InfluxDbProperty(ip: Ip, port: Port, database: Database)

sealed trait ApdlSource extends Entity {
  val name : String
  val id : BoardId
}
case class GenericSource(name: String, id: BoardId, mac: Mac, ip: Ip, inputs: List[ApdlInput], sends: List[Send]) extends ApdlSource

case class BoardId(id: String)

sealed trait Visualisation extends Entity
case class Graph(title: String,
                 source: String,
                 aggregator: Aggregator,
                 plots: List[PlotType.EnumVal],
                 unit: String,
                 min: Double,
                 max: Double) extends Visualisation

case class Aggregator(aggregateRange: AggregateRange, aggregateFunction: AggregateFunction.EnumVal)

case class AggregateRange(value: Int, timeUnit: TimeUnit.EnumVal)

object AggregateFunction {
  sealed abstract class EnumVal {
    def toApdlKeyword: String = this match {
      case Average => "average"
      case Count => "count"
      case Maximum => "maximum"
      case Median => "median"
      case Minimum => "minimum"
      case Mode => "mode"
      case Sum => "sum"
    }
  }
  case object Average extends EnumVal
  case object Count extends EnumVal
  case object Maximum extends EnumVal
  case object Median extends EnumVal
  case object Minimum extends EnumVal
  case object Mode extends EnumVal
  case object Sum extends EnumVal

  def values: Seq[AggregateFunction.EnumVal] = Seq(Average, Count, Maximum, Median, Minimum, Mode, Sum)
}

object PlotType {
  sealed abstract class EnumVal {
    def toApdlKeyword: String = this match {
      case Bar => "bar"
      case Line => "line"
      case Point => "point"
    }
  }
  case object Bar extends EnumVal
  case object Line extends EnumVal
  case object Point extends EnumVal

  def values: Seq[PlotType.EnumVal] = Seq(Bar, Line, Point)
}

sealed trait ApdlInput
case class GenericInput(name: String, typ: ApdlTyp) extends ApdlInput
case class PinInput(name: String, typ: ApdlTyp, pin: Int) extends ApdlInput

sealed trait Sampling
case class UpdateSampling() extends Sampling
case class PeriodicSampling(value: Int, timeUnit: TimeUnit.EnumVal) extends Sampling

object TimeUnit {
  sealed abstract class EnumVal(val valueInMs: BigInt) {
    def toApdlKeyword: String = this match {
      case MilliSecond => "ms"
      case Second => "s"
      case Minutes => "m"
      case Hours => "h"
      case Day => "d"
    }
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

  def values: Seq[EnumVal] = Seq(MilliSecond, Second, Minutes, Hours, Day)
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
