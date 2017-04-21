package apdl.unit_test

import apdl.parser._
import org.scalacheck.Gen
import org.scalacheck.Prop._
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

import scala.io.Source
import scala.util.Try
import scala.util.parsing.input.CharSequenceReader

class ApdlDslTest extends FlatSpec with Checkers {

  implicit override val generatorDrivenConfig = PropertyCheckConfig(
    minSize = 100,
    maxSize = 300,
    workers = 4
  )

  val parser = new ApdlParser

  import parser._

  def parse[A](code: String, astParser: Parser[A]): A = {
    parser.parse(astParser, new PackratReader[Char](new CharSequenceReader(code))) match {
      case Success(result, next) =>
        if (!next.atEnd) throw new ApdlParserException(s"Unable to completely parse $code")
        result
      case n: NoSuccess =>
        if(code != "") throw new ApdlParserException(s"Unable to parse $code: $n")
        else throw new ApdlParserException(s"Unable to parse '': $n")
    }
  }

  behavior of s"property_ip"

  it should "convert a valid ip to is AST type" in {
    val ast = parse("ip 198.213.21.32", property_ip)
    assert(ast == Ip("198.213.21.32"))
    assert(ast.toString == Ip("198.213.21.32").toString)
  }

  it should "completely parse valid ip address" in {
    Try {
      parse("ip 127.0.0.1", property_ip)
      parse("ip 172.31.255.255", property_ip)
      parse("ip 0.0.0.0", property_ip)
      parse("ip 1.1.1.1", property_ip)
      parse("ip 999.999.999.999", property_ip)
      parse("ip 0.1.0.1", property_ip)
      parse("ip0.1.0.1", property_ip)
    } match {
      case util.Failure(f) =>
        println(f)
        fail
      case util.Success(_) => succeed
    }
  }

  it should "throw ApdlParserException for invalid ip property" in {
    assertThrows[ApdlParserException](parse("ips 123.2.2.1", property_ip))
    assertThrows[ApdlParserException](parse("ip 2.2.1", property_ip))
    assertThrows[ApdlParserException](parse("ip 123..2.1", property_ip))
    assertThrows[ApdlParserException](parse("ip 123...0", property_ip))
    assertThrows[ApdlParserException](parse("ip 1.2.2.3.1", property_ip))
    assertThrows[ApdlParserException](parse("ip 1.1.1. 1", property_ip))
    assertThrows[ApdlParserException](parse("ip 1.a1.1.1", property_ip))
    assertThrows[ApdlParserException](parse("ip 1.1.*1.1", property_ip))
    assertThrows[ApdlParserException](parse("ip 1.1.1.-1", property_ip))
    assertThrows[ApdlParserException](parse("ip 0.0.0", property_ip))
    assertThrows[ApdlParserException](parse("ip f.f.f.f", property_ip))
    assertThrows[ApdlParserException](parse("ips1.1.1.1", property_ip))
    assertThrows[ApdlParserException](parse("ip 12321213213.123213213213213.21321321321321321.321321321313", property_ip))
  }

  it should "correctly parse or throws ApdlParserException on some various input" in {
    val ipGen = for {
      a <- Gen.choose(-2000, 2000)
      b <- Gen.choose(-2000, 2000)
      c <- Gen.choose(-2000, 2000)
      d <- Gen.choose(-2000, 2000)
    } yield (a, b, c, d)

    check {
      forAll(ipGen) { ip =>
        val code = s"ip ${ip._1}.${ip._2}.${ip._3}.${ip._4}"
        val ipSeq = Seq(ip._1, ip._2, ip._3, ip._4)
        if (ipSeq.forall(n => n >= 0 && n <= 999))
          parse(code, property_ip) == Ip(ipSeq mkString ".")
        else
          throws(classOf[ApdlParserException])(parse(code, property_ip))
      }
    }
  }

  behavior of "property_mac"

  it should "parse a correctly formatted mac address" in {
    val ast = parse("mac 4f:3c:55:33:3a:9e", property_mac)
    assert(ast == Mac("4f:3c:55:33:3a:9e"))
    assert(ast == Mac(Seq("4f", "3c", "55", "33", "3a", "9e")))
    assert(ast.address == Seq("4f", "3c", "55", "33", "3a", "9e"))
    assert(ast.toString == Mac("4f:3c:55:33:3a:9e").toString)
  }

  it should "completely parse some valid mac address" in {
    Try {
      parse("mac FF:FF:FF:FF:FF:FF", property_mac)
      parse("macFF:FF:FF:FF:FF:FF", property_mac)
      parse("mac ff:ff:ff:ff:ff:ff", property_mac)
      parse("macff:ff:ff:ff:ff:ff", property_mac)
      parse("mac 00:00:00:00:00:00", property_mac)
      parse("mac00:00:00:00:00:00", property_mac)
      parse("mac 33:33:ff:ff:ff:ff", property_mac) // ipv6 multicast
      parse("mac 01:00:5E:ff:ff:ff", property_mac) // ipv4 multicast
      parse("mac fF:Ff:fF:ff:FF:Ff", property_mac)
      parse("mac 00:00:5E:00:01:FA", property_mac) // VRRP
      parse("mac 00:00:0c:07:ac:ff", property_mac) // HSRP
      parse("mac 01:00:0C:CC:CC:CC", property_mac) // Cisco discovery protocol
      parse("mac 02:ae:ea:fa:af:cc", property_mac)
      parse("mac 00:11:22:33:44:55", property_mac)
      parse("mac 01:23:45:67:89:ab", property_mac)
      parse("mac cd:ef:dc:43:56:12", property_mac)
      parse("mac 01:23:45:67:89:AB", property_mac)
      parse("mac CD:EF:DC:43:56:12", property_mac)
      parse("macCD:EF:DC:43:56:12", property_mac)
    } match {
      case util.Failure(ex) =>
        println(ex)
        fail
      case util.Success(_) =>
        succeed
    }
  }

  it should "throw an ApdlParserException for invalid mac_property" in {
    assertThrows[ApdlParserException](parse("macs", property_mac))
    assertThrows[ApdlParserException](parse("mac :21:21:21:21:21", property_mac))
    assertThrows[ApdlParserException](parse("mac :21:21:21:21:21", property_mac))
    assertThrows[ApdlParserException](parse("mac ::21:21:21:21", property_mac))
    assertThrows[ApdlParserException](parse("mac :21::21:21:21", property_mac))
    assertThrows[ApdlParserException](parse("mac :21:21::21:21", property_mac))
    assertThrows[ApdlParserException](parse("mac :21::21:21:21", property_mac))
    assertThrows[ApdlParserException](parse("mac :21:21:21::21", property_mac))
    assertThrows[ApdlParserException](parse("mac :21:21:21:21:", property_mac))
    assertThrows[ApdlParserException](parse("mac 1", property_mac))
    assertThrows[ApdlParserException](parse("mac 1:2:3:4:4:5", property_mac))
    assertThrows[ApdlParserException](parse("mac gg:asd:1:1:2:1", property_mac))
    assertThrows[ApdlParserException](parse("mac ff:gg:ff:ff:ff:ff", property_mac))
    assertThrows[ApdlParserException](parse("mac -11:12:21:21:21:21", property_mac))
    assertThrows[ApdlParserException](parse("mac ", property_mac))
  }

  it should "correctly parse some random mac adress" in {
    val macChar = List(
      '0', '1', '2', '3', '4', '5',
      '6', '7', '8', '9', 'a', 'b',
      'c', 'd', 'e', 'f', 'A', 'B',
      'C', 'D', 'E', 'F'
    )
    val correctMacGen = for {
      a1 <- Gen.oneOf(macChar)
      b1 <- Gen.oneOf(macChar)
      c1 <- Gen.oneOf(macChar)
      d1 <- Gen.oneOf(macChar)
      e1 <- Gen.oneOf(macChar)
      f1 <- Gen.oneOf(macChar)
      a2 <- Gen.oneOf(macChar)
      b2 <- Gen.oneOf(macChar)
      c2 <- Gen.oneOf(macChar)
      d2 <- Gen.oneOf(macChar)
      e2 <- Gen.oneOf(macChar)
      f2 <- Gen.oneOf(macChar)
    } yield Seq((a1, a2), (b1, b2), (c1, c2), (d1, d2), (e1, e2), (f1, f2))
    check {
      forAll(correctMacGen) { mac =>
        val macStr = s"${mac.map(tuple => s"${tuple._1}${tuple._2}") mkString ":"}"
        val code = s"mac $macStr"
        parse(code, property_mac) == Mac(macStr)
      }
    }
  }

  behavior of "property_port"

  it should "parse a correctly formatted port property" in {
    assert(parse("port 8086", property_port) == Port(8086))
    assert(parse("port 90", property_port) == Port(90))
    assert(parse("port 86", property_port) == Port(86))
    assert(parse("port 9999", property_port) == Port(9999))
    assert(parse("port 65535", property_port) == Port(65535))
  }


  it should "not accept port smaller than 1 and greater that 65535" in {
    assertThrows[ApdlDslException](parse("port 0", property_port))
    assertThrows[ApdlDslException](parse("port 65536", property_port))
  }

  it should "not accept invalid formatted port" in {
    assertThrows[ApdlParserException](parse("port -2", property_port))
    assertThrows[ApdlParserException](parse("port 2*2", property_port))
    assertThrows[ApdlParserException](parse("port 2E10", property_port))
    assertThrows[ApdlParserException](parse("port -+-1", property_port))
    assertThrows[ApdlParserException](parse("port +2", property_port))
  }

  behavior of "property_database"

  it should "parse a correctly formatted database property" in {
    assert(parse("database example", property_database) == Database("example"))
    assert(parse("database eXaMpLe", property_database) == Database("eXaMpLe"))
    assert(parse("database Example", property_database) == Database("Example"))
    assert(parse("database _Example", property_database) == Database("_Example"))
    assert(parse("database _example", property_database) == Database("_example"))
    assert(parse("database e_x_a_mple", property_database) == Database("e_x_a_mple"))
  }

  it should "not accept invalid formatted database property" in {
    assertThrows[ApdlParserException](parse("databases asd", property_database))
    assertThrows[ApdlParserException](parse("database 1asd", property_database))
    assertThrows[ApdlParserException](parse("database 1_2asd", property_database))
    assertThrows[ApdlParserException](parse("database a*s*d", property_database))
    assertThrows[ApdlParserException](parse("database as¼d", property_database))
    assertThrows[ApdlParserException](parse("database a|sd", property_database))
    assertThrows[ApdlParserException](parse("database as/d", property_database))
  }

  behavior of "apdl_type"

  it should "correctly parse all the apdl's type" in {
    assert(parse("float", apdl_type) == ApdlFloat)
    assert(parse("int", apdl_type) == ApdlInt)
    assert(parse("double", apdl_type) == ApdlDouble)
    assert(parse("byte", apdl_type) == ApdlByte)
    assert(parse("short", apdl_type) == ApdlShort)
    assert(parse("char", apdl_type) == ApdlChar)
    assert(parse("long", apdl_type) == ApdlLong)
    assert(parse("bool", apdl_type) == ApdlBool)
  }

  it should "throw an exception when the type is unknow" in {
    val typeList = List("float", "int", "double", "byte", "short", "char", "long")
    val wrongTypeGen = for {
      a <- Gen.alphaStr suchThat (s => !typeList.contains(s))
    } yield a

    check {
      forAll(wrongTypeGen) { wrongTyp =>
        throws(classOf[ApdlParserException])(parse(wrongTyp, apdl_type))
      }
    }
  }

  behavior of "board_id"

  it should "correctly parse all the boards_id form platformio" in {
    val platformIoBoardsId = Source.fromFile("./src/test/resources/platformio_board_id.txt").getLines().toSeq
    for {
      id <- platformIoBoardsId
    } {
      val code = s""""$id""""
      assert(parse(code, board_id) == BoardId(id))
    }
  }
}

