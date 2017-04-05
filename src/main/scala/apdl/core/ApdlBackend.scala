package apdl.core

trait ApdlBackend {
  def code(apdlTokens: ApdlTokens): String
}

class ArduinoBackend extends ApdlBackend {
  override def code(apdlTokens: ApdlTokens): String = apdlTokens match {
    case e: Expr => e match {
      case Add(left, right) => s"(${code(left)} + ${code(right)})"
      case Mul(left, right) => s"(${code(left)} * ${code(right)})"
      case Sub(left, right) => s"(${code(left)} - ${code(right)})"
      case Div(left, right) => s"(${code(left)} / ${code(right)})"
      case Literal(number) => s"${code(number)}"
      case Constant(identifier) => s"${code(identifier)}"
      case Symbol(identifier) => s"${code(identifier)}"
      case Number(value) => s"$value"
      case Identifier(name) => s"$name"
      case m: MathFunction => m match {
        case Log(expr) => s"log${code(expr)}"
        case Log10(expr) => s"log10${code(expr)}"
        case Sin(expr) => s"sin${code(expr)}"
        case Cos(expr) => s"cos${code(expr)}"
        case Tan(expr) => s"tan${code(expr)}"
        case Asin(expr) => s"asin${code(expr)}"
        case Acos(expr) => s"acos${code(expr)}"
        case Atan(expr) => s"atan${code(expr)}"
        case Exp(expr) => s"exp${code(expr)}"
        case Sqrt(expr) => s"sqrt${code(expr)}"
        case Ceil(expr) => s"ceil${code(expr)}"
        case Floor(expr) => s"floor${code(expr)}"
        case Pow(n, power) => s"pow(${code(n)},${code(power)})"
        case Round(expr) => s"round${code(expr)}"
        case Abs(expr) => s"abs${code(expr)}"
        case Max(left, right) => s"max(${code(left)},${code(right)})"
        case Min(left, right) => s"min(${code(left)},${code(right)})"
        case Tanh(expr) => s"tanh${code(expr)}"
        case Sinh(expr) => s"sinh${code(expr)}"
        case Cosh(expr) => s"cosh${code(expr)}"
      }
    }
    case t: Typ => t match {
      case TypInt() => "int"
      case TypFloat() => "float"
      case TypLong() => "long"
      case TypDouble() => "double"
    }
    case Arg(identifier, typ) => s"${code(typ)} ${code(identifier)}"
    case s: Statement => s match {
      case Def(identifier, args, retType, body) =>
        s"""
           |${code(retType)} ${code(identifier)}(${args.map(code).mkString(",")}) {
           | ${code(body)}
           |}
         """.stripMargin
      case NewVal(identifier, init, typ) => s"${code(typ)} ${code(identifier)} = ${code(init)};"
    }
    case FunctionBody(statements, ret) =>
      s"""
         |${statements.map(code).mkString("\n")}
         |return ${code(ret)};
       """.stripMargin
  }
}
