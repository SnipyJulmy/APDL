package apdl.example

import apdl.core.APDL

import scala.io.Source

object Main extends APDL{

  val dbName = "arduino-sensor"
  val sourceName = "arduino_1"
  val tfTempTopic = InfluxTopic(dbName, sourceName, "tftemp")
  val rawTempTopic = InfluxTopic(dbName, sourceName, "rawtemp")

  macAddress(Seq(0x98b, 0x4F, 0xEE, 0x00, 0x81, 0x54))
  ipAddress(Seq(172, 16, 0, 100))
  serverAddress(Seq(160, 98, 61, 150))
  serverPort(8086)
  bufferSize(2084)

  // homemade transformater
  val tf = Transformater[Int,Float](Source.fromFile("src/main/resources/tf.scala").mkString)

  tf.emitCSource()

  /*
  tfTempTopic.sendInt(tf(analogInput(0)), 1000)
  rawTempTopic.sendInt(analogInput(0), 1000)
  */
}
