package apdl.example

import apdl.Utils._
import apdl.core._

/**
  * Created by snipy
  * Project apdl
  */
trait Example extends APDL {

  val dbName = "arduino-sensor"
  val source = "arduino_1"
  val tempTopic = "temp"
  val lumTopic = "light"

  def DataTemp: Exp[Unit] = {
    def tf = fun { a: Rep[Int] =>
      val B = 3975
      val res = (1023 - a) * 10000 / a
      val log = math_log(res / 10000) / B + 1 / 298.15
      val inv_log = 1 / log.toInt
      val temperature = inv_log - 273.15.toInt
      temperature

    }
    val temp = applyTransform(tf, analogInput(1))
    val tempSampling = 1000 // ms
    sendFloatToInfluxDb(temp, dbName, source, tempTopic,tempSampling)
  }

  def DataLum: Exp[Unit] = {
    val lum = analogInput(0)
    val lumSampling = 1000 // ms
    sendIntToInfluxDb(lum, dbName, source, lumTopic, lumSampling)
  }

  def Config: Exp[Unit] = {
    // TODO from string to Seq[Byte] for DSL
    // TODO specify what is available to the DSL user and what is not : bufferSize ???
    macAddress(Seq(0x98b, 0x4F, 0xEE, 0x00, 0x81, 0x54))
    ipAddress(Seq(172, 16, 0, 100))
    serverAddress(Seq(160, 98, 61, 150))
    serverPort(8086)
    bufferSize(2084)
  }
}

object ExampleApp extends App {
  def compiler(): ApdlDriver = new ApdlDriver with Example {
    override def inputs(): Seq[((Exp[Unit]) => (Exp[Unit]), String)] = List(
      ((_: Exp[Unit]) => DataTemp, "temp"),
      ((_: Exp[Unit]) => DataLum, "light")
    )
    override def apdlMain(x: Exp[Unit]): Exp[Unit] = Config
  }
  compiler().genCode()
}
