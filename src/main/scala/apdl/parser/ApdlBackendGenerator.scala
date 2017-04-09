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

    val servers = entities.filter(o => o.isInstanceOf[Server]).map(o => o.asInstanceOf[Server])
    val sources = entities.filter(_.isInstanceOf[Source]).map(_.asInstanceOf[Source])

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

    println(s"$main \n $header \n $function \n $loop \n $setup")

    // TODO include previous StringWriter
    s"""
       |
     """.stripMargin
  }
}
