package apdl.generation

import java.io.{File, PrintWriter, StringWriter}

import apdl.parser.ApdlProject
import apdl.{ApdlCodeGenerationException, ApdlConfig}
import scala.Function._

import sys.process._

class ApdlHandler(val project: ApdlProject)(implicit config: ApdlConfig) {

  private val outputString: StringWriter = new StringWriter()

  generateSource()

  def generateSerials(): List[HandlerSerial] = {
    val serials = project.devices.flatMap(d => d.serials.map(s => (d,s)))
    serials map tupled { (d,s) =>
      val id = d.name
      val topic = s.inputName
      val port = d.port
      HandlerSerial(id,topic,port)
    }
  }

  def generateSource(): Unit = {
    val serials: List[HandlerSerial] = generateSerials()
    println(
      s"""
import serial

from influxdb import InfluxDBClient

def process(serial, client, compValue, deviceName):
  data = serial.readline()
  asciiData: str = data.decode('ascii')
  array = asciiData.split(":")
  topic: str = array[0]
  value: str = array[1]
  value = value.replace("\\n", "", 1)
  value = value.replace("\\r", "", 1)
  value = value.replace(" ", "")
  topic = topic.replace(" ", "")
  if compValue not in topic:
    return
  print("Send to influxdb == " + topic + " : " + value)
  json = [
      {
          "measurement": topic + deviceName,
          "fields": {
              "value": int(value)
          }
      }
  ]
  client.write_points(json)

def main():
  ${serials.map(_.mkSerialDeclaration).mkString("\n  ")}

  user = 'root'
  password = 'root'
  dbname = 'apdl-default'

  client = InfluxDBClient('localhost', 8086, user, password, dbname)
  client.drop_database(dbname)
  print("Create database")
  client.create_database(dbname)

  # The format is
  # topic : value

  while True:
    try:
      ${serials.map(_.mkProcessCall).mkString("\n    ")}
    except IndexError:
      continue
    except serial.serialutil.SerialException:
      continue

if __name__ == "__main__":
  main()
""")
  }

  def mkFile(rootDirectory: File): Unit = {
    val file = new File(rootDirectory.getAbsolutePath + "/handler.py")
    if (file.exists()) {
      if (config.overrideExistingProject) {
        file.delete()
        file.createNewFile()
      }
      else {
        throw new ApdlCodeGenerationException("Handler file already exist")
      }
    }
    val pw = new PrintWriter(file)
    pw.append(outputString.toString)
    pw.flush()
    pw.close()
    formatSource(file)
  }

  private def println(string: String): Unit = {
    outputString.append(string + "\n")
    outputString.flush()
  }

  private def formatSource(file : File): Unit = {
    val filename = file.getAbsolutePath
    val command = s"astyle --delete-empty-lines --suffix=none --style=linux --break-blocks=all $filename"
    println(s"Invoke : $command")

    val result = command !!

    println(result)
  }
}

case class HandlerSerial(id: String, topicName: String, port: String, baud: Int = 9600, byteSize: Int = 8, timeout: Int = 1) {
  def mkSerialDeclaration: String = s"""$id = serial.Serial('$port',$baud,bytesize=$byteSize,timeout=$timeout)"""

  def mkProcessCall: String = s"""process($id,client,"$topicName","$id")"""
}