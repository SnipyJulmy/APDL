package apdl.core

/**
  * Created by snipy
  * Project apdl
  */
trait APDL extends Dsl {

  case class InfluxSender(dbName: String, sourceName: String) {
    def sendInt(data: Rep[Int], topic: String, sampling: Int): Exp[Unit] = sendIntToInfluxDb(data, dbName, sourceName, topic, sampling)
    def sendFloat(data: Rep[Float], topic: String, sampling: Int): Exp[Unit] = sendFloatToInfluxDb(data, dbName, sourceName, topic, sampling)
  }

  case class InfluxTopic(dbName: String, sourceName: String, topic: String) {
    def sendInt(data: Rep[Int], sampling: Int): Exp[Unit] = sendIntToInfluxDb(data, dbName, sourceName, topic, sampling)
    def sendFloat(data: Rep[Float], sampling: Int): Exp[Unit] = sendFloatToInfluxDb(data, dbName, sourceName, topic, sampling)
  }
}



