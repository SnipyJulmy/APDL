package apdl.core

import scala.lms.common.{BaseGenMathOps, CGenEffect, MathOpsExp}

trait APDLCGenMath extends CGenEffect with BaseGenMathOps {
  val IR: MathOpsExp

  import IR._

  // TODO : complete math operator
  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case MathLog(x) => emitValDef(sym, s"log(${quote(x)})")
    case _ => super.emitNode(sym, rhs)
  }
}
