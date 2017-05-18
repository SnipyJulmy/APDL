package apdl.parser

import apdl.parser.ApdlType.{Id, Num, Str}

/**
  * An code generators which target the apdl language itself
  * Primarly use for test and try
  */
object ApdlBackendGenerators {

  def toApdlCode(define: Define): String = define match {
    case DefineInput(name, parameters, gens) =>
      s"""
         |@define input $name ${parameters map toApdlCode mkString " "} {
         |  ${gens map toApdlCode mkString "\n"}
         |}
       """.stripMargin
    case DefineComponent(name, parameters, inputs, outputType, gens) =>
      s"""
         |@define component $name ${parameters map toApdlCode mkString " "} {
         |  @in ${inputs map toApdlCode mkString " "}
         |  @out ${toApdlCode(outputType)}
         |  ${gens map toApdlCode mkString "\n"}
         |}
       """.stripMargin
  }

  def toApdlCode(parameter: Parameter): String = s"${parameter.id} : ${parameter.typ}"

  def toApdlCode(gen: (String, apdl.parser.Gen)): String =
    s"""
       |@gen ${gen._1} {
       |  ${toApdlCode(gen._2)}
       |}
     """.stripMargin

  def toApdlCode(outputType: ApdlType): String = outputType match {
    case Num => "num"
    case Str => "str"
    case Id => "id"
  }

  def toApdlCode(gen: Gen): String =
    s"""
       |global = "${gen.global}"
       |setup = "${gen.setup}"
       |loop = "${gen.loop}"
       |expr = "${gen.expr}"
     """.stripMargin

  def toApdlCode(x: Map[String, Gen]): String = x.map { case (k, v) =>
    s"""
       |@gen $k {
       | ${toApdlCode(v)}
       |}
    """.stripMargin
  } mkString "\n"

  def toApdlCode(parameters : Seq[Parameter]) : String = parameters.map(toApdlCode).mkString(" ")
}
