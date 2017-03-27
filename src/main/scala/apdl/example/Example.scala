package apdl.example

import java.io.File

import apdl.Utils._
import apdl.core._

/**
  * Created by snipy
  * Project apdl
  */
trait Example extends APDL {

  def ExampleMain = {

    // TODO from string to Seq[Byte] for DSL
    // TODO specify what is available to the DSL user and what is not : bufferSize ???
    macAddress(Seq(0x98b, 0x4F, 0xEE, 0x00, 0x81, 0x54))
    ipAddress(Seq(172, 16, 0, 100))
    serverAddress(Seq(160, 98, 61, 150))
    serverPort(8086)
    bufferSize(2084)

    val dbName = "arduino-sensor"
    val source = "arduino_1"
    val tempTopic = "temp"
    val lumTopic = "light"

    def tf: Exp[(Int) => Float] = fun { a: Rep[Int] =>
      val B = 3975
      val res = (1023 - a) * 10000 / a
      val log = math_log(res / 10000) / B + 1 / 298.15
      val inv_log = 1 / log.toInt
      val temperature = inv_log - 273.15.toInt
      temperature
    }
    val temp = applyTransform(tf, analogInput(1))
    val lum = analogInput(0)
    sendIntToInfluxDb(lum, dbName, source, lumTopic, 1000)
    sendFloatToInfluxDb(temp, dbName, source, tempTopic, 1000)
  }
}

object Test extends App {
  def compiler(): DslDriverC[Unit, Unit] = new DslDriverC[Unit, Unit] with Example {
    def snippet(a: Rep[Unit]): Rep[Unit] = ExampleMain
  }
  val f = new File("apdl-gen.h")
  f.delete()
  println(compiler().code)
}
