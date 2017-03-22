package function

import scala.lms.common._

trait TempTransform extends Dsl with MathOps with MathOpsExp {
  def temperature(): Exp[(Int) => Int] = fun { a : Rep[Int] =>
    val B = 3975
    val res = (1023 - a) * 10000 / a
    val log = math_log(res / 10000) / B + 1 / 298.15
    val inv_log  = 1 / log.toInt
    val temperature =  inv_log - 273.15.toInt
    temperature
  }
}

object Usage extends App {
  def specialize(): DslDriverC[Int, Float] = new DslDriverC[Int, Float] with TempTransform {
    def snippet(n: Rep[Int]): Rep[Float] = temperature()(n)
  }
  println(specialize().code)
}


