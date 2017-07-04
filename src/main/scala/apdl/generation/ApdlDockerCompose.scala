package apdl.generation

import java.io.{File, PrintWriter, StringWriter}

import apdl.{ApdlCodeGenerationException, ApdlConfig}

class ApdlDockerCompose() {

  def mkFile(rootDirectory: File)(implicit config: ApdlConfig): Unit = {
    val file = new File(rootDirectory.getAbsolutePath + "/docker-compose.yml")
    if (file.exists()) {
      if (config.overrideExistingProject) {
        file.delete()
        file.createNewFile()
      }
      else {
        throw new ApdlCodeGenerationException("Handler file already exist")
      }
    }

    val outputString: StringWriter = new StringWriter()

    outputString.append {
      s"""|version : '2'
          |services:
          |  influxdb:
          |    image: "influxdb:latest"
          |    container_name : "influxdb"
          |    ports:
          |      - "8086:8086"
          |    expose:
          |      - "8086"
          |    volumes:
          |      - ./data/influxdb:/var/lib/influxdb
          |  grafana:
          |    image: grafana/grafana:latest
          |    container_name: grafana
          |    ports:
          |      - "3000:3000"
          |    links:
          |      - "influxdb"
          |    volumes:
          |      - ./data/grafana:/var/lib/grafana""".stripMargin
    }

    val pw = new PrintWriter(file)
    pw.print(outputString.toString)
    pw.flush()
    pw.close()
  }

}
