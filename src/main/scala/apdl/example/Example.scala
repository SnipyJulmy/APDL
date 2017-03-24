package apdl.example

import apdl.core._
import apdl.internal.DslDriverC
import java.io.File

/**
  * Created by snipy
  * Project apdl
  */
trait Example extends APDL {

  def ExampleMain = {

    macAddress(Seq(
      0x98.toByte,
      0x4F.toByte,
      0xEE.toByte,
      0x00.toByte,
      0x81.toByte,
      0x54.toByte))

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
