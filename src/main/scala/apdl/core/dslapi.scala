package apdl.core

import java.io.PrintWriter

import scala.lms.common._
import scala.reflect.SourceContext

// TODO : refactor this file
trait UtilOps extends Base {
  this: Dsl =>
  def infix_HashCode[T: Typ](o: Rep[T])(implicit pos: SourceContext): Rep[Long]

  def infix_HashCode(o: Rep[String], len: Rep[Int])(implicit v: Overloaded1, pos: SourceContext): Rep[Long]
}

trait UtilOpsExp extends UtilOps with BaseExp {
  this: DslExp =>

  case class ObjHashCode[T: Typ](o: Rep[T])(implicit pos: SourceContext) extends Def[Long] {
    def m: Typ[T] = typ[T]
  }

  case class StrSubHashCode(o: Rep[String], len: Rep[Int])(implicit pos: SourceContext) extends Def[Long]

  def infix_HashCode[T: Typ](o: Rep[T])(implicit pos: SourceContext) = ObjHashCode(o)

  def infix_HashCode(o: Rep[String], len: Rep[Int])(implicit v: Overloaded1, pos: SourceContext) = StrSubHashCode(o, len)

  override def mirror[A: Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@ObjHashCode(a) => infix_HashCode(f(a))(e.m, pos)
    case e@StrSubHashCode(o, len) => infix_HashCode(f(o), f(len))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]
}

trait CGenUtilOps extends CGenBase {
  val IR: UtilOpsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case StrSubHashCode(o, len) => emitValDef(sym, src"hash($o,$len)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait Dsl extends PrimitiveOps
  with NumericOps with BooleanOps with LiftString
  with LiftPrimitives with LiftNumeric with LiftBoolean
  with IfThenElse with Equal with RangeOps
  with OrderingOps with MiscOps with ArrayOps
  with StringOps with Functions
  with While with StaticData with Variables
  with LiftVariables with ObjectOps with UtilOps
  with MathOps with MathOpsExp
  with Arduino {
  // implicit def repStrToSeqOps(a: Rep[String]) = new SeqOpsCls(a.asInstanceOf[Rep[Seq[Char]]])

  override def infix_&&(lhs: Rep[Boolean], rhs: => Rep[Boolean])(implicit pos: scala.reflect.SourceContext): Rep[Boolean] =
    __ifThenElse(lhs, rhs, unit(false))

  def generate_comment(l: String): Rep[Unit]

  def comment[A: Typ](l: String, verbose: Boolean = true)(b: => Rep[A]): Rep[A]
}

trait DslExp extends Dsl with PrimitiveOpsExpOpt with NumericOpsExpOpt
  with BooleanOpsExp with IfThenElseExpOpt with EqualExpBridgeOpt
  with RangeOpsExp with OrderingOpsExp with MiscOpsExp
  with EffectExp with ArrayOpsExpOpt with StringOpsExp
  with SeqOpsExp with FunctionsRecursiveExp with WhileExp
  with StaticDataExp with VariablesExpOpt with ObjectOpsExpOpt
  with UtilOpsExp with MathOpsExp with ArduinoExp {

  override def boolean_or(lhs: Exp[Boolean], rhs: Exp[Boolean])(implicit pos: SourceContext): Exp[Boolean] = lhs match {
    case Const(false) => rhs
    case _ => super.boolean_or(lhs, rhs)
  }

  override def boolean_and(lhs: Exp[Boolean], rhs: Exp[Boolean])(implicit pos: SourceContext): Exp[Boolean] = lhs match {
    case Const(true) => rhs
    case _ => super.boolean_and(lhs, rhs)
  }

  override def unboxedFresh[A: Typ]: Exp[A] = fresh[A]

  case class GenerateComment(l: String) extends Def[Unit]

  def generate_comment(l: String): Exp[Unit] = reflectEffect(GenerateComment(l))

  case class Comment[A: Typ](l: String, verbose: Boolean, b: Block[A]) extends Def[A]

  def comment[A: Typ](l: String, verbose: Boolean)(b: => Rep[A]): Rep[A] = {
    val br = reifyEffects(b)
    val be = summarizeEffects(br)
    reflectEffect[A](Comment(l, verbose, br), be)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Comment(_, _, b) => effectSyms(b)
    case _ => super.boundSyms(e)
  }

  override def array_apply[T: Typ](x: Exp[Array[T]], n: Exp[Int])(implicit pos: SourceContext): Exp[T] = (x, n) match {
    case (Def(StaticData(x: Array[T])), Const(c)) =>
      val y = x(c)
      if (y.isInstanceOf[Int]) unit(y) else staticData(y)
    case _ => super.array_apply(x, n)
  }
}

trait DslGenC extends CGenNumericOps with CGenArduino with APDLCGenFunctions
  with CGenPrimitiveOps with CGenBooleanOps with CGenIfThenElse
  with CGenEqual with CGenRangeOps with CGenOrderingOps
  with CGenMiscOps with CGenArrayOps with CGenStringOps
  with CGenSeqOps with CGenWhile with ArduinoGenMath
  with CGenStaticData with CGenVariables
  with CGenObjectOps with CGenUtilOps {
  val IR: DslExp

  import IR._

  def getMemoryAllocString(count: String, memType: String): String = {
    "(" + memType + "*)malloc(" + count + " * sizeof(" + memType + "));"
  }

  override def remap[A](m: Typ[A]): String = m.toString match {
    case "java.lang.String" => "char*"
    case "Array[Char]" => "char*"
    case "Char" => "char"
    case _ => super.remap(m)
  }

  override def format(s: Exp[Any]): String = {
    remap(s.tp) match {
      case "uint16_t" => "%c"
      case "bool" | "int8_t" | "int16_t" | "int32_t" => "%d"
      case "int64_t" => "%ld"
      case "float" | "double" => "%f"
      case "string" => "%s"
      case "char*" => "%s"
      case "char" => "%c"
      case "void" => "%c"
      case _ =>
        import scala.lms.internal.GenerationFailedException
        throw new GenerationFailedException("CGenMiscOps: cannot print type " + remap(s.tp))
    }
  }

  override def quoteRawString(s: Exp[Any]): String = {
    remap(s.tp) match {
      case "string" => quote(s) + ".c_str()"
      case _ => quote(s)
    }
  }

  // we treat string as a primitive type to prevent memory management on strings
  // strings are always stack allocated and freed automatically at the scope exit
  override def isPrimitiveType(tpe: String): Boolean = {
    tpe match {
      case "char*" => true
      case "char" => true
      case _ => super.isPrimitiveType(tpe)
    }
  }

  override def quote(x: Exp[Any]): String = x match {
    case Const(s: String) => "\"" + s.replace("\"", "\\\"") + "\"" // TODO: more escapes?
    case Const('\n') if x.tp == typ[Char] => "'\\n'"
    case Const('\t') if x.tp == typ[Char] => "'\\t'"
    case Const(0) if x.tp == typ[Char] => "'\\0'"
    case _ => super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case a@ArrayNew(n) =>
      val arrType = remap(a.m)
      stream.println(arrType + "* " + quote(sym) + " = " + getMemoryAllocString(quote(n), arrType))
    case ArrayApply(x, n) => emitValDef(sym, quote(x) + "[" + quote(n) + "]")
    case ArrayUpdate(x, n, y) => stream.println(quote(x) + "[" + quote(n) + "] = " + quote(y) + ";")
    case PrintLn(s) => stream.println("printf(\"" + format(s) + "\\n\"," + quoteRawString(s) + ");")
    case StringCharAt(s, i) => emitValDef(sym, "%s[%s]".format(quote(s), quote(i)))
    case Comment(s, verbose, b) =>
      stream.println("//#" + s)
      if (verbose) {
        stream.println("// generated code for " + s.replace('_', ' '))
      } else {
        stream.println("// generated code")
      }
      emitBlock(b)
      emitValDef(sym, quote(getBlockResult(b)))
      stream.println("//#" + s)
    // Add Mathematical operator !
    case _ => super.emitNode(sym, rhs)
  }

  //noinspection TypeAnnotation
  override def emitSource[A: Typ](args: List[Sym[_]], body: Block[A], functionName: String, out: PrintWriter) = {
    val sA = remap(typ[A])

    withStream(out) {
      stream.println {
        """
          |/*
          | * Start of C Generated Code
          | */
        """.stripMargin
      }

      stream.println(sA + " " + functionName + "() {")

      emitBlock(body)

      val y = getBlockResult(body)
      if (remap(y.tp) != "void")
        stream.println("return " + quote(y) + ";")

      stream.println("}")
      stream.println {
        """
          |/*
          | * End of C Generated Code
          | */
        """.stripMargin
      }
      Nil
    }
  }

}

abstract class ApdlProg extends Dsl {
  def inputs(): Seq[(Rep[Unit] => Rep[Unit], String)]
  def apdlMain(x: Rep[Unit]): Rep[Unit]
}

abstract class ApdlDriver extends ApdlProg with DslExp {
  q =>
  val codegen = new DslGenC {
    val IR: q.type = q
  }
  def genCode(): Unit = {
    // Generate include
    apdl.ApdlStreamManager.mainPrintln {
      """
        |#include <Ethernet.h>
        |#include <Timer.h>
        |#include <stdio.h>
        |#include <stdlib.h>
        |#include <string.h>
        |#include <stdbool.h>
        |
        |EthernetClient client;
        |Timer timer;
        |
        |#include "apdl-header.h"
        |#include "apdl-fun.h"
        |
        |void loop() {
        |  #include "apdl-loop.h"
        |}
        |
        |void setup() {
        |  #include "apdl-setup.h"
        |}
      """.stripMargin
    }

    // Generate info for the setup
    apdl.ApdlStreamManager.setupPrintln {
      s"""
         |Serial.begin(115200);
         |delay(1000);
         |connectToInflux();
       """.stripMargin
    }

    // Connecting to Influx arduino function
    apdl.ApdlStreamManager.functionPrintln {
      s"""
         |void connectToInflux() {
         |  if (Ethernet.begin(mac) == 0) {
         |    Serial.println("Failed to configure Ethernet using DHCP");
         |    // no point in carrying on, so do nothing forevermore:
         |    // try to congifure using IP address instead of DHCP:
         |    Ethernet.begin(mac, ip);
         |  }
         |  delay(2000); // give time to allow connection
         |
         |  //do a fast test if we can connect to server
         |  int conState = client.connect(server, eth_port);
         |
         |  if (conState > 0) {
         |    Serial.println("Connected to InfluxDB server");
         |    client.stop();
         |  }
         |
         |  //print the error number and return false
         |  Serial.print("Could not connect to InfluxDB Server, Error #");
         |  Serial.println(conState);
         |}
       """.stripMargin
    }

    // Sending data function for Arduino
    apdl.ApdlStreamManager.functionPrintln {
      s"""
         |void sendData(char* data, int dataSize) {
         |  //first we need to connect to InfluxDB server
         |  int conState = client.connect(server, eth_port);
         |
         |  if (conState <= 0) { //check if connection to server is stablished
         |    Serial.print("Could not connect to InfluxDB Server, Error #");
         |    Serial.println(conState);
         |    return;
         |  }
         |
         |  //Send HTTP header and buffer
         |  client.println("POST /write?db=arduino HTTP/1.1");
         |  client.println("Host: www.embedonix.com");
         |  client.println("User-Agent: Arduino/1.0");
         |  client.println("Connection: close");
         |  client.println("Content-Type: application/x-www-form-urlencoded");
         |  client.print("Content-Length: ");
         |  client.println(dataSize);
         |  client.println();
         |  client.println(data);
         |
         |  delay(50); //wait for server to process data
         |
         |  //Now we read what server has replied and then we close the connection
         |  Serial.println("Reply from InfluxDB");
         |  while (client.available()) { //receive char
         |    Serial.print((char)client.read());
         |  }
         |  Serial.println(); //empty line
         |
         |  client.stop();
         |}
       """.stripMargin
    }
    inputs().foreach { f =>
      apdl.ApdlStreamManager.setupPrintln(s"timer.every(sampling${f._2},callbackSend_${f._2});")
      codegen.emitSource(f._1, s"callbackSend_${f._2}", apdl.ApdlStreamManager.apdlMainStream)
    }
    codegen.emitSource(apdlMain, "Main", apdl.ApdlStreamManager.apdlMainStream)
  }
}
