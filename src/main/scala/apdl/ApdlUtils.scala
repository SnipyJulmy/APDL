package apdl

object ApdlUtils {
  def debug(str : String)(implicit debugEnable : Boolean) : Unit = {
    if(debugEnable) {
      println(str)
    }
  }

  def warning(str : String)(implicit debugEnable : Boolean) : Unit = {
    if(debugEnable) {
      println(str)
    }
  }

  def exitOnFailure() : Unit = {
    println(s"EXIT with code 1")
    System.exit(1)
  }

  def exitOnFailure(msg : String)(implicit debugEnable : Boolean) : Unit = {
    if(debugEnable)
      println(msg)
    println(s"EXIT with code 1")
    System.exit(1)
  }
}
