package apdl

import com.github.SnipyJulmy.scalacolor.ScalaColor._

object ApdlUtils {
  def debug(str : String)(implicit debugEnable : Boolean) : Unit = {
    if(debugEnable) {
      println(str.blue)
    }
  }

  def warning(str : String)(implicit debugEnable : Boolean) : Unit = {
    if(debugEnable) {
      println(str.red)
    }
  }

  def exitOnFailure() : Unit = {
    println(s"EXIT with code 1".red)
    System.exit(1)
  }

  def exitOnFailure(msg : String)(implicit debugEnable : Boolean) : Unit = {
    if(debugEnable)
      println(msg.red)
    println(s"EXIT with code 1".red)
    System.exit(1)
  }
}
