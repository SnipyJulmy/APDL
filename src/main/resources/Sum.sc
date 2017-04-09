case class Num(num: Num)

val one = Num(null)
val two = Num(Num(null))

def sumNum(a: Num, b: Num): Num = {
  if (a == null) b
  else if (b == null) a
  else Num(sumNum(a.num,b))
}

def toString(num: Num) : String = {
  def inner(n : Num) : Int = n match {
    case null => 0
    case Num(null) => 1
    case Num(n) => 1 + inner(n)
  }
  s"${inner(num)}"
}

toString(one)
toString(two)
toString(sumNum(one,one))
toString(sumNum(sumNum(two,two),sumNum(two,one)))