package hubmodel.output

import java.io.{BufferedWriter, File, FileWriter}

import hubmodel.NewTimeNumeric.mkOrderingOps
import hubmodel.{NewTime, PedestrianTrait}
import myscala.math.stats.{Quantiles, computeQuantiles}
import play.api.libs.json.{JsValue, Json, Writes}

package object TRANSFORM {

  implicit class PopulationProcessing[T <: PedestrianTrait](pop: Iterable[T]) {

    def computeTT4TRANSFORM(quantiles: Seq[Double], startTime: NewTime, endTime: NewTime, fileName: String): Unit = {
      val res: collection.mutable.Map[(String, String), collection.mutable.ArrayBuffer[Double]] = collection.mutable.Map()
      pop.foreach(p => {
        if (p.entryTime >= startTime || p.exitTime <= endTime) {
          res.getOrElseUpdate((p.oZone.name, p.dZone.name), collection.mutable.ArrayBuffer()).append(p.travelTime.value)
        }
      })
      val file = new File(fileName)
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(Json.prettyPrint(Json.toJson(
        res.map(kv => ODWithQuantiles(kv._1._1, kv._1._2, startTime, endTime, computeQuantiles(quantiles)(kv._2)))
      )))
      bw.close()
    }
  }

  private case class ODWithQuantiles(O: String, D: String, start: NewTime, end: NewTime, q: Quantiles[Double])

  private implicit def ODQuantilesWrites: Writes[ODWithQuantiles] = new Writes[ODWithQuantiles] {
    def writes(d: ODWithQuantiles): JsValue = {
      Json.obj(
        "o" -> d.O,
        "d" -> d.D,
        "start_timestamp" -> d.start.toString,
        "end_timestamp" -> d.end.toString,
        "quantiles" -> d.q.quantiles,
        "values" -> d.q.values,
        "sample_size" -> d.q.sampleSize
      )
    }
  }
}
