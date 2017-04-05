package apdl.core

import scala.language.implicitConversions

trait APDL extends App {

  case class Transformater[-A, +B](code: String) extends ApdlTfParser {
    lazy val f: Def = parse(_def, code) match {
      case Success(result, _) => result
      case n: NoSuccess => throw new Exception(s"parser error ! : $n")
    }
    lazy val compiledCode: String = {
      f.toString
    }
    def emitCSource(): Unit = {
      println(cSource)
    }
    def cSource: String = {
      // TODO change to a backend
      f.toString
    }
  }

  case class InfluxSender(dbName: String, sourceName: String) {
    def send[A](data: A, sampling: TypInt): Unit = println(s"send data : ${data.getClass}")
    def withTopic(topic: String): InfluxTopic = InfluxTopic(dbName, sourceName, topic)
  }

  case class InfluxTopic(dbName: String, sourceName: String, topic: String) {
    def send[A](data: A, sampling: TypInt): Unit = println(s"send data : ${data.getClass}")
    def withoutTopic(): InfluxSender = InfluxSender(dbName, sourceName)
  }

  // For ethernet configuration
  def macAddress(address: Seq[Byte]): Unit = println(s"macAddress : ${address.map(_.cHex).mkString(":")}")
  def ipAddress(address: Seq[Int]): Unit = println(s"ipAddress : ${address.mkString(":")}")
  def serverAddress(address: Seq[Int]): Unit = println(s"server : ${address.mkString(":")}")

  // other config
  def serverPort(port: Int): Unit = println(s"server port : $port")
  def bufferSize(size: Int): Unit = println(s"buffer size : $size")

}
