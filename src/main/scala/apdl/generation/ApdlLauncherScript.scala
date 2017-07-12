package apdl.generation

import java.io.{File, PrintWriter, StringWriter}

import apdl.parser.ApdlProject
import apdl.{ApdlCodeGenerationException, ApdlConfig}

class ApdlLauncherScript(val project: ApdlProject)(implicit config: ApdlConfig) {

  private val strScript: StringWriter = new StringWriter()

  def mkFile(rootDirectory: File): Unit = {
    mkScript()
    val file = new File(rootDirectory.getAbsolutePath + "/launcher.sh")
    if (file.exists()) {
      if (config.overrideExistingProject) {
        file.delete()
        file.createNewFile()
      }
      else {
        throw new ApdlCodeGenerationException("Handler file already exist")
      }
    }
    val pw = new PrintWriter(file)
    pw.print(strScript.toString)
    pw.flush()
    pw.close()
  }

  private def mkScript(): Unit = {
    strScript.append {
      """|#! /bin/bash
         |
         |if ! type "pip" > /dev/null; then
         |  echo "please install pip and/or python"
         |  exit
         |fi
         |
         |if ! type "virtualenv" > /dev/null; then
         |  sudo pip install virtualenv
         |else
         |  echo "virtualenv is installed !"
         |fi
         |
         |envName="apdl-env"
         |
         |virtualenv ${envName}
         |source ${envName}/bin/activate
         |
         |pip install docker-compose
         |pip install influxdb
         |pip install pyserial
         |
         |docker-compose up -d
         |""".stripMargin
    }

    project.devices.foreach { d =>
      val folderName = d.name
      strScript.append {
        s"""|cd $folderName

            |platformio run -t upload
            |cd ..
            |""".stripMargin
      }
    }

    strScript.append {
      s"""|python handler.py
          |""".stripMargin
    }

  }
}
