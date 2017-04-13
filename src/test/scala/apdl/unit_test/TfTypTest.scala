package apdl.unit_test

import apdl.parser._
import org.scalatest.FlatSpec

import scala.util.Try
import scala.util.parsing.input.CharSequenceReader

class TfTypTest extends FlatSpec {
  val parser = new ApdlParser

  import parser._

  def parseTyp(code: String): TfRetTyp = {
    parser.parse(tf_ret_type, new PackratReader(new CharSequenceReader(code))) match {
      case Success(result, next) =>
        if (next.atEnd) result
        else throw new ApdlParserException(s"Something is not parse : ${next.source}")
      case n: NoSuccess => throw new ApdlParserException(s"Unable to parser $code : $n")
    }
  }

  s"int" should "be convert to" in assert(parseTyp("int") == TfInt())
  s"bool" should "be convert to" in assert(parseTyp("bool") == TfBoolean())
  s"float" should "be convert to" in assert(parseTyp("float") == TfFloat())
  s"double" should "be convert to" in assert(parseTyp("double") == TfDouble())
  s"long" should "be convert to" in assert(parseTyp("long") == TfLong())
  s"byte" should "be convert to" in assert(parseTyp("byte") == TfByte())
  s"char" should "be convert to" in assert(parseTyp("char") == TfChar())
  s"short" should "be convert to" in assert(parseTyp("short") == TfShort())
  s"void" should "be convert to" in assert(parseTyp("void") == TfVoid())

  s"int[]" should "be convert to" in assert(parseTyp("int[]") == TfArray(TfInt()))
  s"float[]" should "be convert to" in assert(parseTyp("float[]") == TfArray(TfFloat()))
  s"char[]" should "be convert to" in assert(parseTyp("char[]") == TfArray(TfChar()))
  s"byte[]" should "be convert to" in assert(parseTyp("byte[]") == TfArray(TfByte()))
  s"double[]" should "be convert to" in assert(parseTyp("double[]") == TfArray(TfDouble()))
  s"long[]" should "be convert to" in assert(parseTyp("long[]") == TfArray(TfLong()))
  s"bool[]" should "be convert to" in assert(parseTyp("bool[]") == TfArray(TfBoolean()))
  s"short[]" should "be convert to" in assert(parseTyp("short[]") == TfArray(TfShort()))

  s"int[][]" should "be convert to" in assert(parseTyp("int[][]") == TfArray(TfArray(TfInt())))
  s"float[][]" should "be convert to" in assert(parseTyp("float[][]") == TfArray(TfArray(TfFloat())))
  s"char[][]" should "be convert to" in assert(parseTyp("char[][]") == TfArray(TfArray(TfChar())))
  s"byte[][]" should "be convert to" in assert(parseTyp("byte[][]") == TfArray(TfArray(TfByte())))
  s"double[][]" should "be convert to" in assert(parseTyp("double[][]") == TfArray(TfArray(TfDouble())))
  s"long[][]" should "be convert to" in assert(parseTyp("long[][]") == TfArray(TfArray(TfLong())))
  s"bool[][]" should "be convert to" in assert(parseTyp("bool[][]") == TfArray(TfArray(TfBoolean())))
  s"short[][]" should "be convert to" in assert(parseTyp("short[][]") == TfArray(TfArray(TfShort())))

  s"void[]" should "not pass" in assert {
    Try(parseTyp("void[]")) match {
      case util.Failure(_) => true
      case util.Success(_) => false
    }
  }

  s"void[][]" should "not pass" in assert {
    Try(parseTyp("void[][]")) match {
      case util.Failure(f) => true
      case util.Success(_) => false
    }
  }
}
