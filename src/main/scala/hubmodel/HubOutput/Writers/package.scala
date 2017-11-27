package hubmodel.HubOutput

import java.io.{BufferedWriter, File, FileWriter}

import hubmodel.input.infrastructure.{NameConversions, ODPair}
import hubmodel.HubOutput.Writers.JSON.{ODQuantiles, TTbyODQuantiles}
import myscala.math.stats.Quantiles
import play.api.libs.json.{JsValue, Json, Writes}

/**
  * Created by nicholas on 3/31/17.
  */
package object Writers {

  implicit class PedestrianJsonWriters(data: Seq[ODQuantiles]) {

    def writeToJSON(nameMappings: NameConversions, fileName: String, path: String = ""): Unit = {
      val file = new File(path + fileName)
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(Json.prettyPrint(Json.toJson(data)))
      bw.close()
    }

  }


}
