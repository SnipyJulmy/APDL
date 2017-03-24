package apdl.example

import java.io.File

import apdl.Utils._
import apdl.core._
import apdl.internal.DslDriverC

/**
  * Created by snipy
  * Project apdl
  */
trait Example extends APDL {

  def ExampleMain = {

    // TODO from string to Seq[Byte] for DSL
    macAddress(Seq(0x98b, 0x4F, 0xEE, 0x00, 0x81, 0x54))
    ipAddress(Seq(172, 16, 0, 100))
    serverAddress(Seq(160, 98, 61, 150))
    serverPort(8086)
    bufferSize(2084)

    def tf: Exp[(Int) => Float] = fun { a: Rep[Int] =>
      val B = 3975
      val res = (1023 - a) * 10000 / a
      val log = math_log(res / 10000) / B + 1 / 298.15
      val inv_log = 1 / log.toInt
      val temperature = inv_log - 273.15.toInt
      temperature
    }
    val temp = applyTransform(tf, analogInput(1))
    temp
  }
}

object Test extends App {

  def compiler(): DslDriverC[Unit, Float] = new DslDriverC[Unit, Float] with Example {
    def snippet(a: Rep[Unit]): Rep[Float] = ExampleMain
  }
  val f = new File("apdl-gen.h")
  f.delete()
  println(compiler().code)
}
