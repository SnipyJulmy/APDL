package apdl.generation

import java.io.File

import apdl.ApdlUtils._
import apdl._
import apdl.parser.DefineUtils._
import apdl.parser._

import scala.Function._

class CLikeCodeGenerator(project: ApdlProject, device: ApdlDevice)(implicit val config: ApdlConfig) {

  private val defines: List[ApdlDefine] = project.defineInputs ::: project.defineTransforms ::: project.defineComponents
  private val transformCodeGen: CLikeTransformCodeGenerator = new CLikeTransformCodeGenerator
  private val framework: ApdlFramework = ApdlFramework.valueOf(device.framework) match {
    case Some(value) => value
    case None => throw new ApdlProjectException(s"Unknow framework ${device.framework}")
  }

  def mkDevice(srcDir: File): Unit = {
    val ext = fileExtension(device.framework)
    val mainFile = new File(srcDir, s"${project.name}_name.$ext")
    if (!mainFile.createNewFile())
      throw new ApdlDirectoryException(s"Can't create file ${mainFile.getAbsolutePath}")
    debug(s"create file ${mainFile.getAbsolutePath}")
    val mainPw = ApdlPrintWriter.getPw(framework)(mainFile)
    generateInputs(mainPw)
    generateSerial(mainPw)
    mainPw.close()
  }

  def fileExtension(framework: String): String = framework match {
    case "arduino" => "ino"
    case "mbed" => "c"
    case _ => throw new ApdlCodeGenerationException(s"Unknow framework : $framework")
  }

  def zipArgWithIdentifier(args: List[String], defineParameters: List[Parameter], inputsParameters: List[Parameter]): Map[String, String] = {
    val params = defineParameters ::: inputsParameters
    require(args.length == params.length, s"parameter size and arguments size are not the same : ${args.length} != ${params.length}")
    (args zip params) map tupled((s, p) => p.id -> s) toMap
  }

  def generateInputs(out: ApdlPrintWriter): Unit = device.inputs.foreach { input =>

    // Find the define component (like the type of the input)
    val definition = defines.defineFromString(input.defineInputName)

    definition match {

      // Input
      case ApdlDefineInput(name, parameters, gens) =>
        val gen = gens.get(device.framework) match {
          case Some(value) => value
          case None => throw new ApdlCodeGenerationException(s"Unknow framework for define inputs $name")
        }

        // associate the args with the parameters name
        implicit val namedArgs = zipArgWithIdentifier(input.args, parameters, List())

        out.printlnGlobal(gen.global.replaceWithArgs)
        out.printlnSetup(gen.setup.replaceWithArgs)
        out.printlnLoop(gen.loop.replaceWithArgs)

        SymbolTable.add(input.identifier, Input(input.identifier, gen.expr.replaceWithArgs))

      // Component
      case ApdlDefineComponent(name, parameters, inputs, outputType, gens) =>
        val gen = gens.get(device.framework) match {
          case Some(value) => value
          case None => throw new ApdlCodeGenerationException(s"Unknow framework for define component $name")
        }

        implicit val namedArgs = zipArgWithIdentifier(input.args, parameters, inputs.parameters)

        out.printlnGlobal(gen.global.replaceWithArgs)
        out.printlnSetup(gen.setup.replaceWithArgs)
        out.printlnLoop(gen.loop.replaceWithArgs)

        SymbolTable.add(input.identifier, Component(input.identifier, gen.expr.replaceWithArgs))

      // Transform
      case ApdlDefineTransform(functionDecl) =>
        out.printFunction(transformCodeGen(functionDecl))
        SymbolTable.add(functionDecl.header.identifier,
          Transform(
            functionDecl.header.identifier, functionDecl.header.parameters.map(_.typ), functionDecl.header.resultType)
        )
    }

    // Generate the code
  }

  def generateSerial(out: ApdlPrintWriter): Unit = {

  }

  implicit class ApdlScriptString(string: String) {
    def replaceWithArgs(implicit args: Map[String, String]): String = {
      def inner(arguments: Map[String, String], acc: String): String = {
        if (arguments.isEmpty)
          acc
        else {
          val (id, value) = arguments.head
          inner(arguments.tail, acc.replace("@" + id, value))
        }
      }

      inner(args, string)
    }
  }
}

object IdGenerator {
  var itr: Long = 0

  def nextVariable(id: String): String = {
    itr = itr + 1
    s"${id}_$itr"
  }
}

class CLikeTransformCodeGenerator {
  def apply(apdlAst: ApdlAst): String = apdlAst match {
    case e: Expr => e match {
      case Add(left, right) => s"(${apply(left)} + ${apply(right)})"
      case Mul(left, right) => s"(${apply(left)} * ${apply(right)})"
      case Sub(left, right) => s"(${apply(left)} - ${apply(right)})"
      case Div(left, right) => s"(${apply(left)} / ${apply(right)})"
      case Cast(tfTyp, expr) => s"((${apply(tfTyp)})${apply(expr)})"
      case Literal(value) => s"$value"
      case Symbol(name) => s"$name"
      case FunctionCall(funcName, args) => s"$funcName(${args map apply mkString ","})"
      case ArrayAccess(array, field) => s"${apply(array)}[${apply(field)}]"
      case VarAssignement(target, value) => s"${apply(target)} = ${apply(value)}"
      case True() => s"1"
      case False() => s"0"
      case Or(left, right) => s"(${apply(left)} || ${apply(right)})"
      case And(left, right) => s"(${apply(left)} && ${apply(right)})"
      case Not(booleanExpr) => s"(! ${apply(booleanExpr)})"
      case Greater(left, right) => s"(${apply(left)} > ${apply(right)})"
      case Smaller(left, right) => s"(${apply(left)} < ${apply(right)})"
      case GreaterEquals(left, right) => s"(${apply(left)} >= ${apply(right)})"
      case SmallerEquals(left, right) => s"(${apply(left)} <= ${apply(right)})"
      case Equals(left, right) => s"(${apply(left)} == ${apply(right)})"
      case NotEquals(left, right) => s"(${apply(left)} != ${apply(right)})"
    }
    case t: TfRetTyp => t match {
      case TfInt => "int"
      case TfLong => "long"
      case TfByte => "byte"
      case TfShort => "short"
      case TfChar => "char"
      case TfDouble => "double"
      case TfFloat => "float"
      case TfVoid => "void"
      case TfBoolean => "bool"
      case TfArray(typ: TfTyp) => s"${apply(typ)}*"
    }
    case TypedIdentifier(name, typ) => s"$name ${apply(typ)}"
    case s: Statement => s match {
      case While(cond, statement) =>
        s"""
           |while(${apply(cond)})
           |  ${apply(statement)}
         """.stripMargin
      case DoWhile(cond, statement) =>
        s"""
           |do
           |  ${apply(statement)}
           |while (${apply(cond)});
         """.stripMargin
      case IfThenElse(cond, trueBranch, falseBranch) =>
        s"""
           |if(${apply(cond)})
           |  ${apply(trueBranch)}
           |else
           |  ${apply(falseBranch)}
         """.stripMargin
      case IfThen(cond, ifTrue) =>
        s"""
           |if(${apply(cond)})
           |  ${apply(ifTrue)}
         """.stripMargin
      case Return(expr) => s"return ${apply(expr)};"
      case Break() => s"break;"
      case Continue() => s"continue;"
      case Block(statements) =>
        s"""
           |{
           |  ${statements map apply mkString "\n"}
           |}
         """.stripMargin
      case ExpressionStatement(expression) => s"${apply(expression)};"
      case decl: Declaration => decl match {
        // No { or } because the body is always a block and the block already generate the {}
        case FunctionDecl(header, body) =>
          s"""
             |${apply(header.resultType)} ${header.identifier} (${header.parameters map apply mkString ","})
             |  ${apply(body.body)}
           """.stripMargin
        case NewVal(symbol, typ, init) => s"${apply(typ)} ${apply(symbol)} = ${apply(init)};"
        case NewVar(symbol, typ, init) => init match {
          case Some(value) => s"${apply(typ)} ${apply(symbol)} = ${apply(value)};"
          case None => s"${apply(typ)} ${apply(symbol)};"
        }
        case NewArray(symbol, typ, init) => s"${apply(typ)} ${apply(symbol)} = ${apply(init)};"
      }
    }
    case arrayInit: ArrayInit => arrayInit match {
      case ArrayInitValue(values) => s"{${values map apply mkString ","}"
      case ArrayInitCapacity(capacity) => s"[${apply(capacity)}]"
    }
  }
}