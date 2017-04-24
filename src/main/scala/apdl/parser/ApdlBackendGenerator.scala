package apdl.parser

import java.io.{File, StringWriter}

trait ApdlBackendGenerator {
  def generate(outputDir: File, entities: List[Entity]): Unit
}

// TODO Refactor
class ArduinoGenerator extends ApdlBackendGenerator {

  def processOutputDir(outputDir: File, overrideExistingOutput: Boolean = true): Unit = {
    if (outputDir.exists()) {
      if (outputDir.isDirectory) {
        if (overrideExistingOutput) {
          if (!outputDir.delete())
            throw new ApdlBackendException(s"Unable to delete ${outputDir.getPath} directory")
        }
      } else {
        throw new ApdlBackendException(s"File ${outputDir.getPath} is not a directory")
      }
    } else {
      if (!outputDir.mkdirs()) {
        throw new ApdlBackendException(s"Unable to create ${outputDir.getPath}")
      }
    }
  }

  override def generate(outputDir: File, entities: List[Entity]): Unit = {
    val main = new StringWriter
    val loop = new StringWriter
    val setup = new StringWriter
    val function = new StringWriter
    val header = new StringWriter

    val servers = entities.filter(_.isInstanceOf[Server]).map(_.asInstanceOf[Server])
    val sources = entities.filter(_.isInstanceOf[Source]).map(_.asInstanceOf[Source])
    val transformaters = entities.filter(_.isInstanceOf[Transformater]).map(_.asInstanceOf[Transformater])

    // TODO fix var usage
    // global info
    var ethPort: String = ""
    var server: String = ""
    var ip: String = ""
    var mac: String = ""

    // Pre-generation

    loop write "timer.update();\n"

    // generate transformater
    transformaters.foreach { tf =>
      function write generate(tf.function)
    }

    // generate servers info
    // the database is global or not for all server ?
    // TODO multiple server
    servers.foreach {
      case InfluxDb(name, prop) =>
        server = s"${name}_ip"
        ethPort = s"${name}_port"
        header.write {
          s"""
             |IPAddress $server(${prop.ip.address mkString ","});
             |const int $ethPort = ${prop.port.number};
             |const char* ${name}_database_name = "${prop.database.name}";
             |const char* ${name}_database_name_eth = "${prop.database.name},";
         """.stripMargin
        }
    }

    // generate sources info
    // TODO multiple source ; for now,we assume that there is just one
    assert(sources.length == 1)
    sources.foreach {
      case GenericSource(name, id, macAddress, ipAddress, inputs, sends) =>
        ip = s"${name}_ip"
        mac = s"${name}_mac"
        header.write {
          s"""
             |IPAddress $ip(${ipAddress.address mkString ","});
             |byte $mac[] {${macAddress.address.map(value => s"0x$value") mkString ","}};
         """.stripMargin
        }
        inputs.foreach {
          case GenericInput(_, _) =>
            throw new ApdlDslException("""Arduino don't support generic input, use the "from pin" functionality""")
          case PinInput(n, typ, pin) =>
            header.write(s"${generate(typ)} ${n}_pin = $pin;\n")
        }
        sends.foreach {
          case GenericSend(target, inputId, sampling) =>
            val input = {
              inputs.find {
                case _: GenericInput =>
                  throw new ApdlDslException("""Arduino don't support generic input, use the "from pin" functionality""")
                case PinInput(n, _, _) => n == inputId
              } match {
                case Some(value) => value.asInstanceOf[PinInput]
                case None =>
                  throw new ApdlDslException(s"""No input $inputId found for send""")
              }
            }

            val _target = {
              servers.find {
                case InfluxDb(n, _) => n == target
              } match {
                case Some(value) => value
                case None => throw new ApdlDslException(s"""No target server $target found for send""")
              }
            }

            val dbName = _target match {
              case InfluxDb(n, prop) => s"${n}_database_name_eth"
            }

            function.write {
              s"""
                 |void send_${input.name}() {
                 |  ${generate(input.typ)} data = analogRead(${input.name}_pin);
                 |  int numChars = 0;
                 |  numChars = sprintf(buf,$dbName);
                 |  numChars += sprintf(&buf[numChars],"SOURCE=$name ");
                 |  numChars += sprintf(&buf[numChars],"$inputId=${cFormat(input.typ)},");
                 |  sendData(buf,numChars);
                 |  memset(buf,'\\0',bufferSize);
                 |  // delay(1000); // some small delay
                 |}
             """.stripMargin
            }

            sampling match {
              case UpdateSampling() =>
                throw new ApdlDslException("Arduino generation does not support update sampling for the moment")
              case PeriodicSampling(value, timeUnit) =>
                setup write s"timer.every(${value * timeUnit.valueInMs},send_${input.name});\n"
            }

          case TfSend(target, tf, inputId, sampling) =>
            val input = {
              inputs.find {
                case _: GenericInput =>
                  throw new ApdlDslException("""Arduino don't support generic input, use the "from pin" functionality""")
                case PinInput(n, _, _) => n == inputId
              } match {
                case Some(value) => value.asInstanceOf[PinInput]
                case None =>
                  throw new ApdlDslException(s"""No input $inputId found for send""")
              }
            }

            val _target = {
              servers.find {
                case InfluxDb(n, _) => n == target
              } match {
                case Some(value) => value
                case None => throw new ApdlDslException(s"""No target server $target found for send""")
              }
            }

            val dbName = _target match {
              case InfluxDb(n, prop) => s"${n}_database_name_eth"
            }

            val _tf = transformaters
              .find(t => t.function.header.identifier == tf)
              .getOrElse(throw new ApdlDslException("Unspecified header"))

            function.write {
              s"""
                 |void send_${input.name}() {
                 |  ${generate(input.typ)} rawData = analogRead(${input.name}_pin);
                 |  ${generate(_tf.function.header.resultType)} data = $tf(rawData);
                 |  int numChars = 0;
                 |  numChars = sprintf(buf,$dbName);
                 |  numChars += sprintf(&buf[numChars],"SOURCE=$name ");
                 |  numChars += sprintf(&buf[numChars],"$input=${cFormat(input.typ)},");
                 |  sendData(buf,numChars);
                 |  memset(buf,'\\0',bufferSize);
                 |  // delay(1000); // some small delay
                 |}
             """.stripMargin
            }

            sampling match {
              case UpdateSampling() =>
                throw new ApdlDslException("Arduino generation does not support update sampling for the moment")
              case PeriodicSampling(value, timeUnit) =>
                setup write s"timer.every(${value * timeUnit.valueInMs},send_${input.name});\n"
            }

        }
    }

    function.write {
      s"""
         |void sendData(char* data, int dataSize) {
         |  //first we need to connect to InfluxDB server
         |  int conState = client.connect($server, $ethPort);
         |
         |  if (conState <= 0) { //check if connection to server is stablished
         |    Serial.print("Could not connect to InfluxDB Server, Error #");
         |    Serial.println(conState);
         |    return;
         |  }
         |
         |  //Send HTTP header and buffer
         |  client.println("POST /write?db=arduino HTTP/1.1");
         |  client.println("Host: www.embedonix.com");
         |  client.println("User-Agent: Arduino/1.0");
         |  client.println("Connection: close");
         |  client.println("Content-Type: application/x-www-form-urlencoded");
         |  client.print("Content-Length: ");
         |  client.println(dataSize);
         |  client.println();
         |  client.println(data);
         |
         |  delay(50); //wait for server to process data
         |
         |  //Now we read what server has replied and then we close the connection
         |  Serial.println("Reply from InfluxDB");
         |  while (client.available()) { //receive char
         |    Serial.print((char)client.read());
         |  }
         |  Serial.println(); //empty line
         |
         |  client.stop();
         |}
         |
         |void connectToInflux() {
         |  if (Ethernet.begin($mac) == 0) {
         |    Serial.println("Failed to configure Ethernet using DHCP");
         |    // no point in carrying on, so do nothing forevermore:
         |    // try to congifure using IP address instead of DHCP:
         |    Ethernet.begin($mac, $ip);
         |  }
         |  delay(2000); // give time to allow connection
         |
         |  //do a fast test if we can connect to server
         |  int conState = client.connect($server, $ethPort);
         |
         |  if (conState > 0) {
         |    Serial.println("Connected to InfluxDB server");
         |    client.stop();
         |  }
         |
         |  //print the error number and return false
         |  Serial.print("Could not connect to InfluxDB Server, Error #");
         |  Serial.println(conState);
         |}
     """.stripMargin
    }

    // TODO generate apdl project

      s"""
         |#include <Ethernet.h>
         |#include <Timer.h>
         |
       |EthernetClient client;
         |Timer timer;
         |
       |const int bufferSize = 2048;
         |char buf[bufferSize] = {'\\0'};
         |
       |$header
         |
       |$function
         |
       |void setup() {
         |  $setup
         |}
         |
       |void loop() {
         |  $loop
         |}
     """.stripMargin
  }

  def generate(apdlTyp: ApdlTyp): String = apdlTyp match {
    case ApdlInt => "int"
    case ApdlShort => "short"
    case ApdlByte => "byte"
    case ApdlChar => "char"
    case ApdlFloat => "float"
    case ApdlDouble => "double"
    case ApdlLong => "long"
    case ApdlBool => "bool"
  }

  def generate(expr: Expr): String = expr match {
    case Symbol(s) => s"$s"
    case Add(left, right) => s"(${generate(left)} + ${generate(right)})"
    case Mul(left, right) => s"(${generate(left)} * ${generate(right)})"
    case Sub(left, right) => s"(${generate(left)} - ${generate(right)})"
    case Div(left, right) => s"(${generate(left)} / ${generate(right)})"
    case ArrayAccess(symbol, field) => s"${generate(symbol)}[${generate(field)}]"
    case Cast(typ, e) => s"(${generate(typ)})${generate(e)}"
    case Literal(value) => s"$value"
    case FunctionCall(funcName, args) => s"$funcName(${args map generate mkString ","})"
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
    case VarAssignement(symbol, value) => s"${generate(symbol)} = ${generate(value)};"
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
    case NewVal(symbol, typ, init) =>
      s"const ${generate(typ)} ${generate(symbol)} = ${generate(init)};"
    case NewVar(symbol, typ, init) => init match {
      case Some(value) => s"${generate(typ)} ${generate(symbol)} = ${generate(value)};"
      case None => s"${generate(typ)} ${generate(symbol)};"
    }
    case NewArray(identifier, typ, init) =>
      init match {
        case ArrayInitValue(values) => s"${generate(typ)} $identifier[] = {${values map generate mkString ","}};"
        case ArrayInitCapacity(capacity) => s"${generate(typ)} $identifier* = malloc(sizeof(${generate(typ)}) * $capacity);"
      }
  }

  def cFormat(apdlTyp: ApdlTyp): String = apdlTyp match {
    case ApdlInt => "%d"
    case ApdlShort => "%d"
    case ApdlByte => "%d"
    case ApdlChar => "%c"
    case ApdlFloat => "%f"
    case ApdlDouble => "%f"
    case ApdlLong => "%d"
    case ApdlBool => "%d" // TODO maybe need to be fix
  }
}
