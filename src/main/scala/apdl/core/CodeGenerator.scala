package apdl.core

import java.io.{File, PrintWriter}

import apdl.Utils._

trait CodeGenerator {
  val model: Model.type = Model

  def genCode(): Unit
}

class ArduinoGenerator(directoryPath: String) extends CodeGenerator {

  private val outputDirectory = new File(directoryPath)
  if (outputDirectory.exists()) outputDirectory.delete()
  outputDirectory.mkdir()
  private val header = new PrintWriter(new File(s"$directoryPath/header.h"))
  private val setup = new PrintWriter(new File(s"$directoryPath/setup.h"))
  private val function = new PrintWriter(new File(s"$directoryPath/function.h"))
  private val loop = new PrintWriter(new File(s"$directoryPath/loop.h"))

  {
    val main = new PrintWriter(new File(s"$directoryPath/main.ino"))
    main.println(base)
    main.flush()
    main.close()
  }

  private def headerPrintln(string: String): Unit = {
    header.println(string)
    header.flush()
  }
  private def setupPrintln(string: String): Unit = {
    setup.println(string)
    setup.flush()
  }
  private def loopPrintln(string: String): Unit = {
    loop.println(string)
    loop.flush()
  }
  private def functionPrintln(string: String): Unit = {
    function.println(string)
    function.flush()
  }

  private def close(): Unit = {
    header.flush()
    header.close()
    loop.flush()
    loop.close()
    setup.flush()
    setup.close()
    function.flush()
    function.close()
  }

  private val base =
    """
      |#include <Ethernet.h>
      |#include <Timer.h>
      |
      |EthernetClient client;
      |
      |#include "header.h"
      |#include "function.h"
      |
      |void setup() {
      | #include "setup.h"
      |}
      |
      |void loop() {
      | #include "loop.h"
      |}
    """.stripMargin

  override def genCode(): Unit = {
    // Network information
    headerPrintln {
      s"""
         |byte mac[] = {${Model.macAddress.get.map(b => b.cHex) mkString ","}};
         |IPAddress ip (${Model.ipAddress.get mkString ","});
         |IPAddress server (${Model.serverAddress.get mkString ","});
         |const int eth_port = ${Model.serverPort.getOrElse("Error")};
         |const int bufferSize = ${Model.bufferSize.getOrElse(2048)} ;
         |char buf[bufferSize]= {'\0'};
       """.stripMargin
    }

    close()
  }
}
