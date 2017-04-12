package apdl.parser

import java.io.StringWriter

trait ApdlBackendGenerator {
  def generate(entities: List[Entity]): String
}

class ArduinoGenerator extends ApdlBackendGenerator {
  override def generate(entities: List[Entity]): String = {
    val main = new StringWriter
    val loop = new StringWriter
    val setup = new StringWriter
    val function = new StringWriter
    val header = new StringWriter

    val servers = entities.filter(_.isInstanceOf[Server]).map(_.asInstanceOf[Server])
    val sources = entities.filter(_.isInstanceOf[Source]).map(_.asInstanceOf[Source])
    val transformaters = entities.filter(_.isInstanceOf[Transformater]).map(_.asInstanceOf[Transformater])

    // generate servers info
    // the database is global or not for all server ?
    servers.foreach {
      case InfluxDb(name, prop) => header.write {
        s"""
           |IPAddress ${name}_ip(${prop.ip.address mkString ","});
           |const int ${name}_port = ${prop.port.number};
           |const char* ${name}_database_name = "${prop.database.name}";
         """.stripMargin
      }
    }

    // generate sources info
    // TODO multiple source for now, assume just one
    assert(sources.length == 1)
    sources.foreach {
      case GenericSource(name, id, mac, ip, inputs, sends) => header.write {
        s"""
           |IPAddress ${name}_ip(${ip.address mkString ","});
           |byte ${name}_mac[] {${mac.address.map(value => s"0x$value") mkString ","}};
         """.stripMargin
      }
    }

    // generate transformater
    transformaters.foreach { tf =>
      function.write {
        generate(tf.function)
      }
    }

    println(s"$main \n $header \n $function \n $loop \n $setup")

    // TODO include previous StringWriter
    s"""
       |
     """.stripMargin
  }

  def generate(expr: Expr): String = expr match {
    case Add(left, right) => s"(${generate(left)} + ${generate(right)})"
    case Mul(left, right) => s"(${generate(left)} * ${generate(right)})"
    case Sub(left, right) => s"(${generate(left)} - ${generate(right)})"
    case Div(left, right) => s"(${generate(left)} / ${generate(right)})"
    case Literal(number) => s"${generate(number)}"
    case Symbol(name) => s" $name "
    case Number(value) => s" $value "
    case FunctionCall(funcName, args) => s"$funcName(${args map generate mkString ","})"
    case b: BooleanExpr => b match {
      case True() => "true"
      case False() => "false"
      case Or(left, right) => s"(${generate(left)} || ${generate(right)})"
      case And(left, right) => s"(${generate(left)} && ${generate(right)})"
      case Not(bool_expr) => s"!(${generate(bool_expr)})"
      case Greater(left, right) => s"${generate(left)} > ${generate(right)}"
      case Smaller(left, right) => s"${generate(left)} < ${generate(right)}"
      case GreaterEquals(left, right) => s"${generate(left)} >= ${generate(right)}"
      case SmallerEquals(left, right) => s"${generate(left)} <= ${generate(right)}"
      case Equals(left, right) => s"${generate(left)} == ${generate(right)}"
      case NotEquals(left, right) => s"${generate(left)} != ${generate(right)}"
    }
  }

  def generate(retType: TfRetTyp): String = retType match {
    case t: TfTyp => t match {
      case p: TfPrimitivesTyp => p match {
        case n: TfNumericTyp => n match {
          case int: TfIntegralTyp => int match {
            case TfInt() => "int"
            case TfLong() => "long"
            case TfByte() => "byte"
            case TfShort() => "short"
            case TfChar() => "char"
          }
          case float: TfFloatingPointTyp => float match {
            case TfDouble() => "double"
            case TfFloat() => "float"
          }
        }
        case TfBoolean() => "bool"
      }
      case TfArray(typ) => s"${generate(typ)}*"
    }
    case TfVoid() => "void"
  }

  def generateParameters(args: List[TypedIdentifier]): String = args map (a => s"${generate(a.typ)} ${a.name}") mkString ","

  def generate(statement: Statement): String = statement match {
    case ExpressionStatement(expression) => s"${generate(expression)}"
    case While(cond, loopBody) =>
      s"""
         |while(${generate(cond)})
         |  ${generate(loopBody)}
       """.stripMargin
    case DoWhile(cond, loopBody) =>
      s"""
         |do ${generate(loopBody)} while (${generate(cond)});
       """.stripMargin
    case VarAssignement(name, newVar) => s"$name = ${generate(newVar)};"
    case ArrayAssignement(id, field, value) => s"$id[${generate(field)}] = ${generate(value)};"
    case IfThenElse(cond, trueBranch, falseBranch) =>
      s"""
         | if(${generate(cond)}) ${generate(trueBranch)} else ${generate(falseBranch)}
       """.stripMargin
    case IfThen(cond, trueBranch) =>
      s"""
         | if(${generate(cond)}) ${generate(trueBranch)}
       """.stripMargin
    case Return(expr) => s"return ${generate(expr)};"
    case Break() => "break;"
    case Continue() => "continue;"
    case Block(statements) => statements match {
      case Nil => ""
      case _ =>
        s"""|{
            |  ${statements map generate mkString "\n"}
            |}""".stripMargin
    }
    case d: Declaration => generate(d)
  }

  def generate(declaration: Declaration): String = declaration match {
    case FunctionDecl(FunctionHeader(resultType, identifier, parameters), FunctionBody(body)) =>
      s"""
         |${generate(resultType)} $identifier (${generateParameters(parameters)})
         |  ${generate(body)}
       """.stripMargin
    case NewVal(name, typ, init) =>
      s"const ${generate(typ)} $name = ${generate(init)};"
    case NewVar(name, typ, init) => init match {
      case Some(value) => s"${generate(typ)} $name = ${generate(value)};"
      case None => s"${generate(typ)} $name;"
    }
    case NewArray(identifier, typ, init) =>
      s"${generate(typ)} $identifier = malloc(sizeof(${generate(typ)}) * ${init.values.length});"
  }
}
