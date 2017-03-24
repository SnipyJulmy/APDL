package apdl.core

import java.io.{File, PrintWriter}

import scala.language.implicitConversions
import scala.lms.common._
import scala.lms.internal.GenericNestedCodegen
import scala.reflect.SourceContext

import apdl.Utils._

/**
  * Created by snipy
  * Project apdl
  */
trait Arduino extends Base with PrimitiveOps with Functions {

  def analogInput(pin: Int)(implicit pos: SourceContext): Rep[Int]
  def digitalInput(pin: Int)(implicit pos: SourceContext): Rep[Int]

  def applyTransform[A: Typ, B: Typ](f: Rep[A => B], arg: Rep[A])(implicit pos: SourceContext): Rep[B]

  // For ethernet configuration
  def macAddress(address: Seq[Byte])(implicit pos: SourceContext): Rep[Unit]
  def ipAddress(address: Seq[Int])(implicit pos: SourceContext): Rep[Unit]
  def serverAddress(address: Seq[Int])(implicit pos: SourceContext): Rep[Unit]
  // TODO : abstraction !
  // def arduinoConfig(mac : Seq[Byte], ip : Seq[Int], server : Seq[Int]) : Rep[Unit]
  def serverPort(port: Int)(implicit pos: SourceContext): Rep[Unit]
  def bufferSize(size: Int)(implicit pos: SourceContext): Rep[Unit]

  // For sending data
  def sendToInfluxDB[A: Typ](data: Rep[A], dbName: String, source: String, fieldName: String)(implicit pos: SourceContext): Rep[Unit]

  // def sendDataToInfluxDB(dbName : String, source : String, )
}

trait ArduinoExp extends Arduino with EffectExp with FunctionsExp {

  /*
   *  Node declaration
   */

  case class AnalogInput(pin: Int) extends Def[Int]
  case class DigitalInput(pin: Int) extends Def[Int]
  case class ApplyTransform[A: Typ, B: Typ](f: Exp[A => B], arg: Exp[A]) extends Def[B] {
    //noinspection TypeAnnotation
    def mA = typ[A]
    //noinspection TypeAnnotation
    def mB = typ[B]
  }

  case class MacAddress(address: Seq[Byte]) extends Def[Unit]
  case class IpAddress(address: Seq[Int]) extends Def[Unit]
  case class ServerAddress(address: Seq[Int]) extends Def[Unit]
  case class ServerPort(port: Int) extends Def[Unit]
  case class BufferSize(size: Int) extends Def[Unit]
  case class SendToInfluxDB[A: Typ](data: Exp[A], dbName: String, source: String, fieldName: String) extends Def[Unit]

  /*
   *  Dsl implementation
   */

  // Input
  override def analogInput(pin: Int)(implicit pos: SourceContext) = AnalogInput(pin)
  override def digitalInput(pin: Int)(implicit pos: SourceContext) = DigitalInput(pin)

  // Transform
  override def applyTransform[A: Typ, B: Typ](f: Exp[A => B], arg: Exp[A])(implicit pos: SourceContext): Exp[B] = {
    val x1 = unbox(arg)
    f match {
      case Def(Lambda(_, _, y)) => reflectEffect(ApplyTransform(f, x1), summarizeEffects(y))
      case _ => reflectEffect(ApplyTransform(f, x1))
    }
  }

  // Ethernet config
  override def macAddress(address: Seq[Byte])(implicit pos: SourceContext): Exp[Unit] = reflectEffect(MacAddress(address))
  override def ipAddress(address: Seq[Int])(implicit pos: SourceContext): Exp[Unit] = reflectEffect(IpAddress(address))
  override def serverAddress(address: Seq[Int])(implicit pos: SourceContext): Exp[Unit] = reflectEffect(ServerAddress(address))
  override def serverPort(port: Int)(implicit pos: SourceContext): Exp[Unit] = reflectEffect(ServerPort(port))
  override def bufferSize(size: Int)(implicit pos: SourceContext): Exp[Unit] = reflectEffect(BufferSize(size))

  // Data send
  override def sendToInfluxDB[A: Typ](data: Exp[A], dbName: String, source: String, fieldName: String)(implicit pos: SourceContext): Exp[Unit] = {
    SendToInfluxDB(data, dbName, source, fieldName)
  }
}

trait BaseGenArduino extends GenericNestedCodegen {
  val IR: ArduinoExp
}

trait CGenArduino extends CGenEffect with BaseGenArduino {
  val ApdlHeaderStream = new PrintWriter(new File("apdl-gen.h"))
  val IR: ArduinoExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case AnalogInput(pin) => emitValDef(sym, s"analogRead($pin)")
    case DigitalInput(pin) => emitValDef(sym, s"digitalRead($pin)")
    case ApplyTransform(f, arg) =>
      emitValDef(sym, quote(f) + "(" + quote(arg) + ")")
    //noinspection ZeroIndexToHead
    case MacAddress(address) =>
      ApdlHeaderStream.append(s"byte mac[] = {" +
        s"${address(0).cHex}," +
        s"${address(1).cHex}," +
        s"${address(2).cHex}," +
        s"${address(3).cHex}," +
        s"${address(4).cHex}," +
        s"${address(5).cHex}};").flush()
    case IpAddress(address) =>
    case ServerAddress(address) =>
    case ServerPort(port) =>
    case BufferSize(address) =>
    case SendToInfluxDB(data, dbName, source, fieldName) =>
    case _ => super.emitNode(sym, rhs)
  }
}




















