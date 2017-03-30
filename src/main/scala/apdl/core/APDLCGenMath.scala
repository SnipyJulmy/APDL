package apdl.core

import scala.lms.common.{BaseGenMathOps, CGenEffect, MathOpsExp}

trait APDLCGenMath extends CGenEffect with BaseGenMathOps {
  val IR: MathOpsExp

  import IR._

  // TODO implement all the arduino function ?
  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case MathLog(x) => emitValDef(sym, s"log(${quote(x)})")
    case MathCeil(x) => emitValDef(sym, s"ceil(${quote(x)})")
    case MathFloor(x) => emitValDef(sym, s"floor(${quote(x)})")
    case MathExp(x) => emitValDef(sym, s"exp(${quote(x)})")
    case MathPow(x, y) => emitValDef(sym, s"pow(${quote(x)},${quote(y)})")
    case MathAbs(x) => emitValDef(sym, s"fabs(${quote(x)})")
    case MathSin(x) => emitValDef(sym, s"sin(${quote(x)})")
    case MathCos(x) => emitValDef(sym, s"cos(${quote(x)})")
    case MathAcos(x) => emitValDef(sym, s"acos(${quote(x)})")
    case MathSqrt(x) => emitValDef(sym, s"sqrt(${quote(x)})")
    case MathAtan2(x, y) => emitValDef(sym, s"atan2(${quote(x)},${quote(y)})")
    case MathMin(x, y) => emitValDef(sym, s"fmin(${quote(x)},${quote(y)})")
    case MathMax(x, y) => emitValDef(sym, s"fmax(${quote(x)},${quote(y)})")
    case MathLog10(x) => emitValDef(sym, s"log10(${quote(x)})")
    case MathSinh(x) => emitValDef(sym, s"sinh(${quote(x)})")
    case MathAsin(x) => emitValDef(sym, s"asin(${quote(x)})")
    case MathCosh(x) => emitValDef(sym, s"cosh(${quote(x)})")
    case MathTan(x) => emitValDef(sym, s"tan(${quote(x)})")
    case MathTanh(x) => emitValDef(sym, s"atan(${quote(x)})")
    case _ => super.emitNode(sym, rhs)
  }
}
