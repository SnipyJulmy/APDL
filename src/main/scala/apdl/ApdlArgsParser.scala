package apdl

import java.io.File

class ApdlArgsParser {

}

case class ApdlConfig(mainFile: File = new File("."),
                      outputDirectory: File = new File("./default-apdl-output"),
                      overrideExistingProject: Boolean = true,
                      debug: Boolean = true)
