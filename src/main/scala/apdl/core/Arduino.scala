package apdl.core

import apdl.ApdlStreamManager._
import apdl.Utils._

import scala.language.implicitConversions
import scala.lms.common._
import scala.lms.internal.GenericNestedCodegen
import scala.reflect.SourceContext

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
  def serverPort(port: Int)(implicit pos: SourceContext): Rep[Unit]
  def bufferSize(size: Int)(implicit pos: SourceContext): Rep[Unit]

  // For sending data
  def sendIntToInfluxDb(data: Rep[Int], dbName: String, source: String, fieldName: String, sampling: Int)(implicit pos: SourceContext): Rep[Unit]
  def sendFloatToInfluxDb(data: Rep[Float], dbName: String, source: String, fieldName: String, sampling: Int)(implicit pos: SourceContext): Rep[Unit]
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
  case class SendIntToInfluxDB(data: Exp[Int], dbName: String, source: String, fieldName: String, sampling: Int) extends Def[Unit]
  case class SendFloatToInfluxDB(data: Exp[Float], dbName: String, source: String, fieldName: String, sampling: Int) extends Def[Unit]

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
  override def macAddress(address: Seq[Byte])(implicit pos: SourceContext): Exp[Unit] = {
    reflectEffect(MacAddress(address))
  }
  override def ipAddress(address: Seq[Int])(implicit pos: SourceContext): Exp[Unit] = {
    reflectEffect(IpAddress(address))
  }
  override def serverAddress(address: Seq[Int])(implicit pos: SourceContext): Exp[Unit] = {
    reflectEffect(ServerAddress(address))
  }
  override def serverPort(port: Int)(implicit pos: SourceContext): Exp[Unit] = {
    reflectEffect(ServerPort(port))
  }
  override def bufferSize(size: Int)(implicit pos: SourceContext): Exp[Unit] = {
    reflectEffect(BufferSize(size))
  }

  // Sending data
  override def sendIntToInfluxDb(data: Exp[Int], dbName: String, source: String, fieldName: String, sampling: Int)(implicit pos: SourceContext): Exp[Unit] = {
    reflectEffect(SendIntToInfluxDB(data, dbName, source, fieldName, sampling))
  }
  override def sendFloatToInfluxDb(data: Exp[Float], dbName: String, source: String, fieldName: String, sampling: Int)(implicit pos: SourceContext): Exp[Unit] = {
    reflectEffect(SendFloatToInfluxDB(data, dbName, source, fieldName, sampling))
  }
}

trait BaseGenArduino extends GenericNestedCodegen {
  val IR: ArduinoExp
}

trait CGenArduino extends CGenEffect with BaseGenArduino {
  val IR: ArduinoExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case AnalogInput(pin) => emitValDef(sym, s"analogRead($pin)")
    case DigitalInput(pin) => emitValDef(sym, s"digitalRead($pin)")
    case ApplyTransform(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")
    //noinspection ZeroIndexToHead
    case MacAddress(address) =>
      headerPrintln(s"byte mac[] = {" +
        s"${address(0).cHex}," +
        s"${address(1).cHex}," +
        s"${address(2).cHex}," +
        s"${address(3).cHex}," +
        s"${address(4).cHex}," +
        s"${address(5).cHex}};\n")
    //noinspection ZeroIndexToHead
    case IpAddress(address) =>
      headerPrintln(s"IPAddress ip(" +
        s"${address(0)}," +
        s"${address(1)}," +
        s"${address(2)}," +
        s"${address(3)}" +
        s");\n")
    //noinspection ZeroIndexToHead
    case ServerAddress(address) =>
      headerPrintln(s"IPAddress server(" +
        s"${address(0)}," +
        s"${address(1)}," +
        s"${address(2)}," +
        s"${address(3)}" +
        s");\n")
    case ServerPort(port) =>
      headerPrintln(s"const int eht_port = $port;\n")
    case BufferSize(size) =>
      headerPrintln(s"const int bufferSize = $size;\nchar buf[bufferSize] = {'\\0'}\n")
    // Send the data to influxdb, create a function which send the data
    case SendIntToInfluxDB(data, dbName, source, fieldName, sampling) =>
      headerPrintln {
        s"""
           |void sendData$fieldName(int ${quote(data)}) {
           |  int numChars = 0;
           |  numChars += sprintf(buf,"$dbName,");
           |  numChars += sprintf(&buf[numChars],"SOURCE=$source");
           |  numChars += sprintf(&buf[numChars],"$fieldName=%d" ,${quote(data)});
           |  sendData(buf,numChars);
           |  memset(buf,'\\0',bufferSize);
           |}
         """.stripMargin
      }
      stream.println(s"sendData$fieldName(${quote(data)})")
      headerPrintln {
        s"int sampling$fieldName = $sampling;"
      }
    case SendFloatToInfluxDB(data, dbName, source, fieldName, sampling) =>
      headerPrintln {
        s"""
           |void sendData$fieldName(float ${quote(data)}){
           |  int numChars = 0;
           |  numChars += sprintf(buf,"$dbName,");
           |  numChars += sprintf(&buf[numChars],"SOURCE=$source");
           |  numChars += sprintf(&buf[numChars],"$fieldName=%f" ,${quote(data)});
           |  sendData(buf,numChars);
           |  memset(buf,'\\0',bufferSize);
           |}
         """.stripMargin
      }
      stream.println(s"sendData$fieldName(${quote(data)})")
      headerPrintln {
        s"int sampling$fieldName = $sampling;"
      }
    case _ => super.emitNode(sym, rhs)
  }
}




















