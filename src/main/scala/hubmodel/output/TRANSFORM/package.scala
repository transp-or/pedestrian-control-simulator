package hubmodel.output

import java.io.{BufferedWriter, File, FileWriter}

import hubmodel.Time
import hubmodel.TimeNumeric.mkOrderingOps
import hubmodel.ped.PedestrianTrait
import myscala.math.stats.{Quantiles, computeQuantiles}
import play.api.libs.json.{JsValue, Json, Writes}

package object TRANSFORM {

  implicit class PopulationProcessingTRANSFORM[T <: PedestrianTrait](pop: Iterable[T]) {

    def computeTT4TRANSFORM(quantiles: Seq[Double], startTime: Time, endTime: Time, fileName: String, startDay: String = "1970-01-01", endDay: String = "2100-12-31"): Unit = {
      val res: collection.mutable.Map[(String, String), collection.mutable.ArrayBuffer[Double]] = collection.mutable.Map()
      pop.foreach(p => {
        if (p.entryTime >= startTime || p.exitTime <= endTime) {
          res.getOrElseUpdate((p.origin.name, p.finalDestination.name), collection.mutable.ArrayBuffer()).append(p.travelTime.value.toDouble)
        }
      })
      val file = new File(fileName)
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(Json.prettyPrint(Json.toJson(
        res.map(kv => ODWithQuantiles(kv._1._1, kv._1._2, startDay + " " + startTime.asReadable, endDay + " " + endTime.asReadable, computeQuantiles(quantiles)(kv._2)))
      )))
      bw.close()
    }
  }

  implicit class PopulationSummaryProcessingTRANSFORM(pop: Iterable[(String, String, Double, Double, Double)]) {

    def computeTT4TRANSFORM(quantiles: Seq[Double], startTime: Time, endTime: Time, fileName: String, startDay: String = "1970-01-01", endDay: String = "2100-12-31"): Unit = {
      val res: collection.mutable.Map[(String, String), collection.mutable.ArrayBuffer[Double]] = collection.mutable.Map()
      pop.foreach(p => {
        if (p._4 >= startTime.value || p._5 <= endTime.value) {
          res.getOrElseUpdate((p._1, p._2), collection.mutable.ArrayBuffer()).append(p._3)
        }
      })
      val file = new File(fileName)
      val bw = new BufferedWriter(new FileWriter(file))
      bw.write(Json.prettyPrint(Json.toJson(
        res.map(kv => ODWithQuantiles(kv._1._1, kv._1._2, startDay + " " + startTime.asReadable, endDay + " " + endTime.asReadable, computeQuantiles(quantiles)(kv._2)))
      )))
      bw.close()
    }
  }


  private case class ODWithQuantiles(O: String, D: String, start: String, end: String, q: Quantiles[Double])

  private implicit def ODQuantilesWrites: Writes[ODWithQuantiles] = new Writes[ODWithQuantiles] {
    def writes(d: ODWithQuantiles): JsValue = {
      Json.obj(
        "o" -> d.O,
        "d" -> d.D,
        "start_timestamp" -> d.start,
        "end_timestamp" -> d.end,
        "quantiles" -> d.q.quantiles,
        "values" -> d.q.values,
        "sample_size" -> d.q.sampleSize
      )
    }
  }
}
