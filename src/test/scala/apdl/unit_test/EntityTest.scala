package apdl.unit_test

import apdl.parser._
import org.scalacheck.Gen
import org.scalacheck.Prop._
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

import scala.util.parsing.input.CharSequenceReader

class EntityTest extends FlatSpec with Checkers {

  val parser = new ApdlParser

  import parser._

  def parse[A](code: String, astParser: Parser[A]): A = {
    parser.parse(astParser, new PackratReader[Char](new CharSequenceReader(code))) match {
      case Success(result, next) =>
        if (!next.atEnd) throw new ApdlParserException(s"Unable to completely parse $code")
        result
      case n: NoSuccess =>
        if (code != "") throw new ApdlParserException(s"Unable to parse $code: $n")
        else throw new ApdlParserException(s"Unable to parse '': $n")
    }
  }

  private def ipGen: Gen[Seq[Int]] = for {
    a <- Gen.choose(0, 999)
    b <- Gen.choose(0, 999)
    c <- Gen.choose(0, 999)
    d <- Gen.choose(0, 999)
  } yield Seq(a, b, c, d)

  def sampleIp: String = ipGen.sample match {
    case Some(value) => value mkString "."
    case None => "0.0.0.0"
  }

  private val macChar = List(
    '0', '1', '2', '3', '4', '5',
    '6', '7', '8', '9', 'a', 'b',
    'c', 'd', 'e', 'f', 'A', 'B',
    'C', 'D', 'E', 'F'
  )

  private def macGen: Gen[Seq[(Char, Char)]] = for {
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

  def sampleMac: String = macGen.sample match {
    case Some(value) => value map (x => s"${x._1}${x._2}") mkString ":"
    case None => "00:00:00:00:00:00"
  }

  private def portGen: Gen[Int] = for {
    port <- Gen.choose(1, 65535)
  } yield port

  def samplePort: String = portGen.sample match {
    case Some(value) => s"$value"
    case None => s"1"
  }

  def sampleIdentifier : String = {
    (for (id <- Gen.identifier) yield id).sample match {
      case Some(value) => value
      case None => "default_identifier"
    }
  }

  /* Transformation Test */

  val t1: String =
    """|transform def tf (x:int) -> float {
       |    val B : int = 3975
       |    val resistance : float = (float)(1023 - x) * 1000 / x
       |    val temperature : float = 1 / (log(resistance/1000) / B + 1 / 298.15) - 273.15
       |    return temperature
       |}""".stripMargin

  t1 should s"produce $t1" in {
    val expected: Transformater = Transformater(FunctionDecl(
      FunctionHeader(TfFloat(), "tf", List(TypedIdentifier("x", TfInt()))),
      FunctionBody(Block(List(
        NewVal(Symbol("B"), TfInt(), Literal("3975")),
        NewVal(Symbol("resistance"), TfFloat(), Div(Mul(Cast(TfFloat(), Sub(Literal("1023"), Symbol("x"))), Literal("1000")), Symbol("x"))),
        NewVal(Symbol("temperature"), TfFloat(),
          Sub(
            Div(Literal("1"),
              Add(
                Div(FunctionCall("log", List(Div(Symbol("resistance"), Literal("1000")))), Symbol("B")),
                Div(Literal("1"), Literal("298.15"))
              )
            ),
            Literal("273.15"))),
        Return(Symbol("temperature"))
      )))))
    assert(parse(t1, transform) == expected)
  }

  val t2: String =
    """|transform def x() -> int {
       | return 2
       |}""".stripMargin

  t2 should s"produce $t2" in {
    val expected = Transformater(FunctionDecl(
      FunctionHeader(TfInt(), "x", List()),
      FunctionBody(Block(List(Return(Literal("2")))))
    ))
    assert(parse(t2, transform) == expected)
  }

  val t3: String =
    """|transform def max(a:float,b:float,c:float,d:float) -> float {
       |  return max(max(a,b),max(c,d))
       |}""".stripMargin

  t3 should s"produce $t3" in {
    val expected: Transformater = Transformater(FunctionDecl(
      FunctionHeader(
        TfFloat(),
        "max",
        List(
          TypedIdentifier("a", TfFloat()),
          TypedIdentifier("b", TfFloat()),
          TypedIdentifier("c", TfFloat()),
          TypedIdentifier("d", TfFloat())
        )
      ),
      FunctionBody(Block(List(
        Return(FunctionCall("max", List(
          FunctionCall("max", List(Symbol("a"), Symbol("b"))),
          FunctionCall("max", List(Symbol("c"), Symbol("d")))
        )))
      )))
    ))

    assert(parse(t3, transform) == expected)
  }

  val t4: String =
    """|transform def factorial (x:int) -> int {
       |  if (x < 2) return 1 else return x * factorial(x - 1)
       |}""".stripMargin

  t4 should s"produce $t4" in {
    val expected: Transformater = Transformater(FunctionDecl(
      FunctionHeader(TfInt(), "factorial", List(TypedIdentifier("x", TfInt()))),
      FunctionBody(Block(List(IfThenElse(
        Smaller(Symbol("x"), Literal("2")),
        Return(Literal("1")),
        Return(Mul(Symbol("x"), FunctionCall("factorial", List(Sub(Symbol("x"), Literal("1"))))))
      ))))
    ))
    assert(parse(t4, transform) == expected)
  }
  val t5: String =
    """|transform def sumArray (a:int[],size:int) -> int {
       |  var res : int = 0
       |  while(size > 0) {
       |    res = res + a[size]
       |    size = size - 1
       |  }
       |  return res
       |}""".stripMargin
  t5 should s"produce $t5" in {
    val expected: Transformater = Transformater(FunctionDecl(
      FunctionHeader(TfInt(), "sumArray", List(
        TypedIdentifier("a", TfArray(TfInt())),
        TypedIdentifier("size", TfInt())
      )),
      FunctionBody(Block(List(
        NewVar(Symbol("res"), TfInt(), Some(Literal("0"))),
        While(
          Greater(Symbol("size"), Literal("0")),
          Block(List(
            ExpressionStatement(VarAssignement(Symbol("res"), Add(Symbol("res"), ArrayAccess(Symbol("a"), Symbol("size"))))),
            ExpressionStatement(VarAssignement(Symbol("size"), Sub(Symbol("size"), Literal("1"))))
          ))
        ),
        Return(Symbol("res"))
      )))
    ))
    assert(parse(t5, transform) == expected)
  }

  val t6: String =
    """|transform def printTemp(temp : int) -> void {
       |  printf("%d\n",temp)
       |}""".stripMargin

  t6 should s"throw an ApdlParserException" in {
    assertThrows[ApdlParserException](parse(t6, transform))
  }

  /* Source test */
  val t7: String =
    """|source a1 "uno" :
       | ip 172.16.0.100
       | mac 00:00:00:00:00:00
       | input a int from pin 1
       | input b int from pin 0
       | input c float from pin 32
       | send a to asd each 1 s
       | send b to asd each 2 s
       | send c to asd each 500 ms""".stripMargin

  val t7Expected: Source = GenericSource(
    "a1",
    BoardId("uno"),
    Mac("00:00:00:00:00:00"),
    Ip("172.16.0.100"),
    List(
      PinInput("a", ApdlInt, 1),
      PinInput("b", ApdlInt, 0),
      PinInput("c", ApdlFloat, 32)
    ),
    List(
      GenericSend("asd", "a", PeriodicSampling(1, TimeUnit.Second)),
      GenericSend("asd", "b", PeriodicSampling(2, TimeUnit.Second)),
      GenericSend("asd", "c", PeriodicSampling(500, TimeUnit.MilliSecond))
    )
  )

  t7 should s"produce $t7Expected" in {
    assert(parse(t7, source) == t7Expected)
  }

  val t8: String =
    """|source a1 "uno" :
       | ip 172.16.0.100
       | mac 00:00:00:00:00:00
       | input a int from pin 1
       | input b int from pin 0
       | input c float from pin 32
       | send a to asd each 1 s
       | send b to asd on update
       | send c to asd each 500 ms""".stripMargin

  val t8Expected: Source = GenericSource(
    "a1",
    BoardId("uno"),
    Mac("00:00:00:00:00:00"),
    Ip("172.16.0.100"),
    List(
      PinInput("a", ApdlInt, 1),
      PinInput("b", ApdlInt, 0),
      PinInput("c", ApdlFloat, 32)
    ),
    List(
      GenericSend("asd", "a", PeriodicSampling(1, TimeUnit.Second)),
      GenericSend("asd", "b", UpdateSampling()),
      GenericSend("asd", "c", PeriodicSampling(500, TimeUnit.MilliSecond))
    )
  )

  t8 should s"produce $t8Expected" in {
    assert(parse(t8, source) == t8Expected)
  }

  val t9: String =
    """|source a1 "uno" :
       | ip 172.16.0.100
       | mac 00:00:00:00:00:00
       | input a double from pin 1
       | input b long
       | input c float from pin 32
       | send a to asd on update
       | send b to asd on update
       | send c to asd on update""".stripMargin

  val t9Expected: Source = GenericSource(
    "a1",
    BoardId("uno"),
    Mac("00:00:00:00:00:00"),
    Ip("172.16.0.100"),
    List(
      PinInput("a", ApdlDouble, 1),
      GenericInput("b", ApdlLong),
      PinInput("c", ApdlFloat, 32)
    ),
    List(
      GenericSend("asd", "a", UpdateSampling()),
      GenericSend("asd", "b", UpdateSampling()),
      GenericSend("asd", "c", UpdateSampling())
    )
  )

  t9 should s"produce $t9Expected" in {
    assert(parse(t9, source) == t9Expected)
  }

  val t10: String =
    """|source a1 "uno" :
       | ip 172.16.0.100
       | mac 00:00:00:00:00:00
       | input a int from pin 1
       | input b int from pin 0
       | input c float from pin 32
       | send fac a to asd each 1 s
       | send b to asd each 2 s
       | send fac c to asd each 500 ms""".stripMargin

  val t10Expected: Source = GenericSource(
    "a1",
    BoardId("uno"),
    Mac("00:00:00:00:00:00"),
    Ip("172.16.0.100"),
    List(
      PinInput("a", ApdlInt, 1),
      PinInput("b", ApdlInt, 0),
      PinInput("c", ApdlFloat, 32)
    ),
    List(
      TfSend("asd", "fac", "a", PeriodicSampling(1, TimeUnit.Second)),
      GenericSend("asd", "b", PeriodicSampling(2, TimeUnit.Second)),
      TfSend("asd", "fac", "c", PeriodicSampling(500, TimeUnit.MilliSecond))
    )
  )

  t10 should s"produce $t10Expected" in {
    assert(parse(t10, source) == t10Expected)
  }

  /* Server test */

  behavior of "influxdb"

  it should "correctly parse and generate the ast of some correct influxdb" in {
    val influxDbGen = for {
      name <- Gen.identifier
      ip <- ipGen
      port <- portGen
      dbName <- Gen.identifier
    } yield (name, ip, port, dbName)

    check {
      implicit val generatorDrivenConfig = PropertyCheckConfig(minSize = 200, maxSize = 300)
      forAll(influxDbGen) { infos =>
        val (name, ip, port, dbName) = infos
        val code = s"influxdb $name : ip ${ip mkString "."} port $port database $dbName"
        parse(code, server) == InfluxDb(name, InfluxDbProperty(
          Ip(ip mkString "."),
          Port(port),
          Database(dbName)
        ))
      }
    }
  }

  it should "throws ApdlParserException for some invalids inputs" in {
    val wrongCodes = List(
      s"influxdb asd ip $sampleIp port $samplePort database $sampleIdentifier", // missing ':'
      s"influxdb 123 ip $sampleIp port $samplePort database $sampleIdentifier", // invalid influxdb id
      s"influxdb 1-2-3 ip $sampleIp port $samplePort database $sampleIdentifier", // invalid influxdb id
      s"influxdb a-b ip $sampleIp port $samplePort database $sampleIdentifier", // invalid influxdb id
      s"influxdb -a ip $sampleIp port $samplePort database $sampleIdentifier", // invalid influxdb id
      s"influxdb  ip $sampleIp port $samplePort database $sampleIdentifier", // missing id
      s"influxdb id ip $sampleIp port database $sampleIdentifier", // missing port value
      s"influxdb id ip port $samplePort database $sampleIdentifier", // missing ip value
      s"influxdb id ip $sampleIp port $samplePort database ", // missing database value
      s"influxdb asd $sampleIp port $samplePort database $sampleIdentifier", // missing ip keyword
      s"influxdb asd ip $sampleIp $samplePort database $sampleIdentifier", // missing port keyword
      s"influxdb asd ip $sampleIp port $samplePort $sampleIdentifier", // missing database keyword
      "influxdb source arduino1 \"uno\" :\n    ip 172.16.0.100\n    mac 98:4F:EE:00:81:54\n    input temp int from pin 1\n    input lum int from pin 0\n    send tf temp to influxdb each 1 m\n    send lum to influxdb each 1 s"
    )

    for(code <- wrongCodes) {
      assertThrows[ApdlParserException](parse(code,server))
    }
  }
}
