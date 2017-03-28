package apdl.core

import scala.lms.common.{BaseGenFunctions, CGenEffect, FunctionsExp}
import apdl.ApdlStreamManager._

/**
  * Created by snipy
  * APDL Project
  */
trait APDLCGenFunctions extends CGenEffect with BaseGenFunctions {

  val IR: FunctionsExp

  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case e@Lambda(fun, x, y) =>

      // we temporaly change the stream
      val bak = stream
      stream = apdlHeaderStream

      val retType = remap(getBlockResult(y).tp)

      stream.println(s"$retType ${quote(sym)}(${remap(x.tp)} ${quote(x)}) {")
      emitBlock(y)
      val z = getBlockResult(y)
      if (retType != "void") stream.println("return " + quote(z) + ";")
      stream.println(s"}")

      // put back the original stream
      stream.flush()
      stream = bak
    case Apply(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")
    case _ => super.emitNode(sym, node)
  }
}
