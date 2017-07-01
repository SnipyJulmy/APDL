package apdl

import java.io.File

class ApdlArgsParser {
  def parse(args: Array[String]): ApdlConfig = {

    val argsParser = new scopt.OptionParser[ApdlConfig]("apdl") {

      // Metadata
      head("apdl", "1.0")

      // Argument
      arg[File]("<file>.apdl")
        .action((f, c) => c.copy(mainFile = f))

      opt[File]('d', "dir")
        .action((o, c) => c.copy(outputDirectory = o))
        .text("Output directory")

      opt[Boolean]('o', "override")
        .action((b, c) => c.copy(overrideExistingProject = b))
        .text("Override existing output")

      opt[Boolean]('d', "debug")
        .action((b, c) => c.copy(debug = b))
        .text("Enable debug message")

      opt[Boolean]('H', "generate-handler")
        .action((b, c) => c.copy(generateHandler = b))
        .text("Generate the handler")

      opt[Boolean]('E', "generate-ecosystem")
        .action((b, c) => c.copy(generateEcosystem = b))
        .text("Generate ecosystem")

      help("help").text("display help")
    }

    argsParser.parse(args, ApdlConfig()) match {
      case Some(value) =>
        if (args.contains("--help")) {
          argsParser.showUsage()
        }
        value
      case None =>
        println("Unable to parse args")
        System.exit(0)
        throw new Exception("Unreachable code")
    }
  }
}

case class ApdlConfig(mainFile: File = new File("."),
                      outputDirectory: File = new File("./default-apdl-output"),
                      overrideExistingProject: Boolean = true,
                      debug: Boolean = true,
                      generateHandler: Boolean = true,
                      generateEcosystem: Boolean = true)
